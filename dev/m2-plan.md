# M2 plan: igvR renders the igvShiny widget

Goal: igvR stops using BrowserViz and its own igv.js frontend. `igvR()`
starts an httpuv server on a random port, serves the igvShiny htmlwidget page
plus a small transport shim, and opens it in the RStudio/Positron Viewer
(`getOption("viewer")`) or `utils::browseURL()`. The shim gives
`igvShiny.js` the two Shiny calls it needs (`Shiny.addCustomMessageHandler`,
`Shiny.setInputValue`) over one WebSocket. igvR depends on igvShiny, never
the reverse. The public igvR API keeps its names and signatures.

Inputs: `dev/m2-inventory.md` (this branch), igvShiny `night/m1-4`
(`dev/m1-plan.md`, `inst/htmlwidgets/igvShiny.js`).

## Architecture in one picture

```
R console                               browser / Viewer
---------                               ----------------
igvR object
 └ env: httpuv server (randomPort)  <── GET /            page: widget deps + shim + JSON spec
        ws, element id, inputs      <── GET /lib/...     htmlwidgets.js, igv-3.8.4.min.js, igvShiny.js
        "fake session"              <── GET /tracks/f    files written by igvShiny builders
        sendCustomMessage(type,msg) ──> ws {type,message} -> handlers registered by igvShiny.js
        inputs[[name]] <- value     <── ws {input,value}  <- Shiny.setInputValue(...)
```

The "fake session" is an environment with `sendCustomMessage(type, message)`
(writes a JSON frame to the socket, or queues it until the socket is open),
`registerDataObj(name, data, filterFunc)` (adds a range-served route, so
`igvShiny::serveLocalFile()` works), `userData`, `onSessionEnded()` (file
cleanup on `close`), and `ns = function(x) x`. igvShiny's senders and
`loadTrack()` take it unchanged: they only call `session$sendCustomMessage`,
`.trackFile(session, ext)` and `serveLocalFile(session, path)`.

The igvShiny JS already queues messages until its igv browser exists
(`addBrowserMessageHandler` + `flushPendingMessages`) and reports
`igvReady`; the shim only has to queue frames until the socket is open.

## Stages (one PR each)

1. **Transport core, unexported** (`R/transport.R`, `inst/transport/shim.js`).
   httpuv on `httpuv::randomPort()`, page generation from
   `igvShiny::igvShiny(genomeOptions)` via htmlwidgets dependency
   resolution, `/lib` + `/tracks` static routes, WebSocket frames, fake
   session, `.waitForInput(name, timeout)` that pumps `httpuv::service()`
   with a timeout (no infinite loops). Tests: R-only (start/stop server, GET
   `/` contains shim and spec, GET `/tracks/x`, ws round-trip with a node
   or `websocket` client if available, else skipped). Nothing exported
   changes. This is where `inst/prototype/` graduates.
2. **igvR class without BrowserViz.** New class (slot: environment). `igvR()`
   keeps the signature, uses stage 1, opens Viewer/browser. Define
   `setBrowserWindowTitle()` (and a replacement for `closeWebSocket`) in
   igvR. `ping()` answered by the shim. DESCRIPTION: drop BrowserViz from
   Depends, add igvShiny to Imports (needs the igvShiny release or
   `Remotes:` for CI). Old methods still use the old handlers at this point,
   so this stage must also land 3 or the package is broken: **stages 2 and 3
   go in the same PR if 3 is small enough; otherwise 2 ships behind an
   internal option `igvR.transport = "widget"` (default "browserviz").**
3. **Delegate navigation**: `showGenomicRegion`, `getGenomicRegion` (reply
   via `currentGenomicRegion.<id>` input, plus the cached `locuschange`
   value), `removeTracksByName`, `setGenome`/`setCustomGenome` (re-render
   the widget with `igvShiny::parseAndValidateGenomeSpec()` output, wait for
   `igvReady`), `getSupportedGenomes`.
4. **Delegate tracks**: delete igvR's `R/*Track.R`, import and re-export
   igvShiny's classes, constructors, `trackInfo`, `trackSize`;
   `displayTrack(obj, track)` -> `igvShiny::loadTrack(fakeSession, id,
   track, deleteTracksOfSameName = ...)`. Remove all `.display*` helpers
   and `myQP`. Keep `test_Tracks.R` as API regression tests. Decide GWAS
   (Q2) first.
5. **igvShiny gaps** (PRs in igvShiny, then a small igvR PR): JS handlers +
   R senders for `getTrackNames` (reply input), `zoomIn`/`zoomOut`,
   `setTrackHeight`, `showTrackLabels`, `saveToSVG` (reply input with
   `browser.toSVG()`), remote-url `VariantTrack` (m1 Q9). Then igvR
   delegates the remaining methods.
6. **Track click / motif logos**: `setTrackClickFunction`,
   `enableMotifLogoPopups`, `.writeMotifLogoImagesUpdateTrackNames` moved
   to `get_tracks_dir()`; needs an igvShiny popover hook (Q4).
7. **Cleanup**: delete `inst/browserCode`, makefile `browserCode` target,
   AGENTS.md step 5, `BrowserViz:::log` calls, Containerfile BrowserViz;
   update vignettes/README text; NEWS + version bump (only here).

## Risks

- **R1 Blocking console.** igvR's API is synchronous; replies need
  `httpuv::service()` pumping. With no timeout today a lost reply hangs R.
  M2 adds timeouts but stays blocking; non-blocking is M3.
- **R2 Name masking.** igvShiny exports `getGenomicRegion`,
  `showGenomicRegion`, `removeTracksByName` (functions of `session, id`) and
  `parseAndValidateGenomeSpec`. igvR has S4 generics of the same names.
  `library(igvR); library(igvShiny)` masks igvR's. Mitigation: igvR never
  `import(igvShiny)`, uses `importFrom` for classes only and `igvShiny::`
  for functions; document; longer term, ask igvShiny to make those
  functions S4 generics with a `ShinySession` method (Q1).
- **R3 Class identity.** `is(igv, "BrowserViz")` becomes FALSE; code using
  BrowserViz methods on an igvR object (`port()`, `closeWebSocket()`,
  `getBrowserWindowSize()`) breaks. Only `setBrowserWindowTitle` is used in
  docs; the rest is assumed rare.
- **R4 Track class behaviour drift.** igvShiny ports differ in small ways
  (m1-plan "Deviations": UCSC bedGraph height, colors not converted to hex,
  GFF3 defaults, displayMode not honoured in the bed handler m1 Q6,
  VariantTrack slots partly unsent m1 Q10). Users see slightly different
  rendering.
- **R5 igv.js 2.13 -> 3.8.4.** Option names and SVG export may differ;
  `saveToSVG` output changes.
- **R6 Page URL assumptions.** igvShiny.js builds URLs as
  `window.location.href + "tracks/..."`; the page must live at `/`. The
  RStudio Viewer proxies `localhost` ports (`rstudioapi::translateLocalUrl`
  / `/p/<port>/` on Server/Workbench, Posit Cloud): href then contains a
  path prefix, which works only because the code appends to href; verify
  in RStudio Server and Positron (UNVERIFIED).
- **R7 Global state in igvShiny.** `state$userAddedTracks` and
  `state$requestedHeight` are package-global, not per widget; two igvR
  objects share them. Harmless for `displayTrack`, wrong for a future
  `getTrackNames` implemented in R.
- **R8 Release ordering.** igvR on Bioc cannot depend on an unreleased
  igvShiny; M2 merges must wait for the igvShiny m1 release (or the same
  Bioc devel cycle).
- **R9 Viewer and WebSockets.** RStudio Desktop Viewer supports ws to
  localhost; Positron's viewer and VS Code webviews have restrictions
  (UNVERIFIED). Fallback: `browseURL`.

## Open questions

1. Should igvShiny turn `getGenomicRegion`/`showGenomicRegion`/
   `removeTracksByName` into S4 generics (session method) so igvR can add
   `igvR` methods instead of masking? Or should igvR rename nothing and
   live with masking?
2. GWAS: igvR's `GWASTrack`/`GWASUrlTrack` (Track parent) vs igvShiny's
   standalone `GWASTrack`. Options: (a) igvR keeps its classes under the
   same name (clash remains), (b) igvR adopts igvShiny's `GWASTrack` and
   maps old constructor args (breaking), (c) rename igvR's to
   `GWASTableTrack` with a deprecated alias.
3. Is a timeout default (e.g. 30 s, `getOption("igvR.timeout")`) acceptable
   for blocking calls, erroring instead of hanging?
4. `setTrackClickFunction` evaluates user JS text in the page. Keep it (add
   a generic "eval popover function" handler to igvShiny), or replace with
   a declarative popover option?
5. What replaces `closeWebSocket(igv)` - an igvR `close()` method,
   finalizer on the object, or both?
6. `currently.supported.stock.genomes()` vs igvShiny `get_basic_genomes()`:
   one source of truth?
7. Keep `portRange`/`host` arguments (ignored with a message) or deprecate
   now?
8. Must the page work offline? igvShiny vendors igv 3.8.4 (yes), igvR's
   current page pulls jQuery from code.jquery.com (no). Stock genomes still
   need network either way.

## What M3 inherits (non-blocking console, igvShinyProxy)

- The fake session + WebSocket transport is the seam: M3 replaces
  `.waitForInput()` busy-waiting with `later`-driven callbacks/promises,
  so the console stays free and `httpuv` runs on the background loop
  (`httpuv::startServer` already services via `later` in interactive R).
- Reply plumbing is uniform: every request that needs an answer gets a
  `setInputValue` name; M3 can expose these as promises or reactive-like
  getters without touching igvShiny.
- `igvShinyProxy` (M3): the fake session object is effectively a proxy for
  a Shiny session; it can be generalised and moved to igvShiny so any R
  process (scripts, notebooks, igvR) drives the widget the same way.
- Open issues carried over: per-widget state in igvShiny (R7), multiple
  igvR instances per process, Viewer/proxy URL handling (R6, R9).
