# M2 inventory: igvR on the igvShiny widget

Scope: every place in igvR (master @ f5b23ff) that touches BrowserViz, the
igvR JS frontend (`inst/browserCode`), fixed ports, m4/jQuery, the blocking
`while (!browserResponseReady())` loops, and the igvR Track classes that now
duplicate igvShiny (branch `night/m1-4`, igvShiny 1.9.52).

M2 actions:

- **delete**: removed in M2, nothing replaces it in igvR.
- **delegate**: igvR keeps the public method, the body calls igvShiny
  (a `.xxxTrackPayload` / `.trackPayload` builder, `loadTrack()`, or an
  igvShiny sender) through the igvR transport.
- **transport**: replaced by the new igvR transport (httpuv server + shim).
- **keep**: unchanged in M2.
- **gap**: igvShiny has no equivalent yet; needs an igvShiny change
  (JS handler and/or R sender) before igvR can delegate.

## 1. BrowserViz (class, constructor, transport)

| file:line | what | M2 action |
|---|---|---|
| DESCRIPTION:13 | `Depends: ... BrowserViz (>= 2.17.1)` | delete; add `igvShiny` (Imports, `>= 1.9.52`), `htmlwidgets`, `jsonlite`; keep `httpuv` |
| NAMESPACE / R/igvR.R:4 | `@import BrowserViz` | delete |
| R/igvR.R:15-20 | `setClass("igvR", contains = "BrowserViz", prototype(uri = "http://localhost", 9000))` | transport: igvR becomes its own class (slots: an environment with httpuv server handle, port, ws, pending replies, element id, tracks dir). `is(igv, "BrowserViz")` will be FALSE (see plan, risk R3) |
| R/igvR.R:24-28 | `igvBrowserFile` global + `.onLoad` pointing at `browserCode/dist/igvApp.html` | delete; page is generated from the igvShiny widget (`htmlwidgets::saveWidget`-style dependency resolution) |
| R/igvR.R:76-79 | `setupMessageHandlers()` / `addRMessageHandler("handleResponse", ...)` (BrowserViz dispatch) | transport: replies arrive as `setInputValue` frames on the WebSocket |
| R/igvR.R:124-146 | `igvR(portRange = 15000:15100, host, title, browserFile, quiet)` calls `BrowserViz(...)` with `httpQueryProcessingFunction = myQP`, then `setBrowserWindowTitle()` | transport: keep the signature (portRange accepted, ignored or used as a hint, deprecation note), start httpuv on `httpuv::randomPort()`, open with `getOption("viewer", utils::browseURL)` |
| R/igvR.R:143 | `setBrowserWindowTitle(obj, title)` (inherited BrowserViz generic) | transport: set `<title>` in the generated page; igvR must define/keep `setBrowserWindowTitle` itself because vignettes/demos call it (see §6) |
| R/igvR.R:28,228,307,620-621,683-1203 | 38 calls to `BrowserViz:::log(...)` (unexported function of another package; BiocCheck notes) | delete; replace with a local `.log()` (futile.logger is already pulled in by igvShiny) or drop |
| R/igvR.R:1487-1541 | `myQP()`: BrowserViz HTTP query hook serving `?<absolute tempfile path>`; serves ANY readable file on disk by absolute path, png/bam as binary, everything else via `scan()` as text | delete; the new httpuv `call` serves `/tracks/<file>` from `igvShiny::get_tracks_dir()` only, plus `/lib/...` widget deps and range requests (see §4). Security improvement: no more arbitrary-path file serving |
| R/igvR.R:3, NAMESPACE | `@import httpuv` | keep (now used directly) |
| Containerfile:32 | installs BrowserViz for CI | delete BrowserViz, add igvShiny (from GitHub until Bioc release) |
| inst/unitTests/test_igvR.R:12-16 | `BrowserViz::webBrowserAvailableForTesting()` guard, `checkTrue(all(c("igvR","BrowserViz") %in% is(igv)))` | delegate test guard to a local helper; change class assertion |
| inst/unitTests/igvR-vignette.R, test_setCustomGenome.R | same `BrowserViz::` guard | as above |

## 2. Blocking request/reply (`send` + `while (!browserResponseReady)`)

Every public method sends one BrowserViz message and then spins
`while (!browserResponseReady(obj)) service(100)`; the reply is the JS
handler's `hub.send({cmd: msg.callback, status, payload})`. There is no
timeout: a missing reply hangs the R console forever.

`send(` at R/igvR.R: 167 232 323 406 474 503 532 561 714 759 794 828 918 999
1050 1107 1154 1205 1231 1267 1327 1366 1397.
`while` loops at R/igvR.R: 176 241 332 415 483 512 539 570 657 1240 1276 1336
1375 1404.

| file:line | method | JS handler (igvApp.js) | igvShiny equivalent (night/m1-4) | M2 action |
|---|---|---|---|---|
| R/igvR.R:166-180 | `ping()` | `ping` | none | transport: answered by the shim itself (no igvShiny change) |
| R/igvR.R:227-245 | `setGenome()` | `setGenome` (re-creates igv browser) | genome is fixed at widget render (`igvShiny(genomeOptions)`); no runtime "set genome" message | transport: re-render the widget with new `parseAndValidateGenomeSpec()` options (send a new `renderValue` payload over the socket, the JS factory already handles re-render via `igvRenderId`); wait for `igvReady` |
| R/igvR.R:294-336 | `setCustomGenome()` | `setCustomGenome` | same as above, `parseAndValidateGenomeSpec(dataMode = "localFiles"/"http")` | as setGenome; local fasta must go through range serving (§4) |
| R/igvR.R:357-390 | `getSupportedGenomes()` | none (pure R) | `get_basic_genomes()` etc. | delegate (or keep `currently.supported.stock.genomes()`) |
| R/igvR.R:404-436 | `getGenomicRegion()` | `getGenomicRegion` | `getGenomicRegion(session, id)` -> `setInputValue("currentGenomicRegion.<id>")` | delegate + transport wait on that input; also cache the `locuschange` input so no round trip is needed |
| R/igvR.R:463-487 | `showGenomicRegion()` (accepts list/GRanges/string) | `showGenomicRegion` | `showGenomicRegion(session, id, region)` | delegate; keep igvR's region normalisation in R |
| R/igvR.R:502-516 | `zoomOut()` | `zoomOut` | none | gap: needs igvShiny `zoomOut` handler (or a generic "callBrowserMethod") |
| R/igvR.R:531-543 | `zoomIn()` | `zoomIn` | none | gap |
| R/igvR.R:559-574 | `setTrackClickFunction()` (ships JS source as text, `new Function` in browser) | `setTrackClickFunction` | igvShiny sends `trackClick` input; no custom JS popover | gap (security-sensitive; see plan Q4) |
| R/igvR.R:608-662 | `displayTrack()` dispatch on `trackInfo()` | `display*FromUrl` (10 handlers) | `loadTrack(session, id, track)` for all 15 ported classes | delegate: `displayTrack(obj, track)` -> `igvShiny::loadTrack(<transport session>, id, track, deleteTracksOfSameName = ...)` |
| R/igvR.R:1229-1244 | `showTrackLabels()` | `showTrackLabels` | none | gap |
| R/igvR.R:1265-1280 | `getTrackNames()` | `getTrackNames` | none (igvShiny keeps R-side `state$userAddedTracks`, global, not per widget) | gap: needs a JS handler replying via `setInputValue("trackNames.<id>")` |
| R/igvR.R:1325-1340 | `removeTracksByName()` | `removeTracksByName` | `removeTracksByName(session, id, trackNames)` | delegate (no reply in igvShiny: return immediately, or wait on a new ack) |
| R/igvR.R:1358-1379 | `setTrackHeight()` | `setBrowserTrackHeight` | none | gap |
| R/igvR.R:1396-1408 | `saveToSVG()` | `getSVG` (returns svg text) | none | gap: needs handler replying via `setInputValue` with `browser.toSVG()` |
| R/igvR.R:1465-1484 | `enableMotifLogoPopups()` (built on `setTrackClickFunction`) | — | none | gap (follows setTrackClickFunction) |

## 3. Track display internals and Track classes (duplicates of igvShiny)

### 3a. `.display*` helpers in R/igvR.R (write a tempfile, build a message, send)

| file:line | what | igvShiny builder | M2 action |
|---|---|---|---|
| R/igvR.R:666-724 | `.displayVariantTrack` (vcf object -> tempfile, or url list) | `.vcfTrackPayload` via `loadTrack(VariantTrack)`; url form refused (igvShiny m1 Q9) | delegate (object form); url form = gap |
| R/igvR.R:726-769 | `.displayAlignmentTrack` (export BAM to tempfile) | `.bamLocalDataTrackPayload` | delegate |
| R/igvR.R:771-804 | `.displayRemoteAlignmentTrack` | `.bamUrlTrackPayload` | delegate |
| R/igvR.R:806-838 | `.displayCramTrack` | `.cramUrlTrackPayload` | delegate |
| R/igvR.R:840-862 | `.writeMotifLogoImagesUpdateTrackNames` (MotifDb/seqLogo png per feature, url = `igv@uri?<png>`) | none | keep in igvR, rewrite to write into `get_tracks_dir()` and rename features to `tracks/<png>` url; depends on setTrackClickFunction gap |
| R/igvR.R:864-929 | `.displayAnnotationTrack` (bed via rtracklayer export) | `.bedTrackPayload` via `.trackPayload(igvAnnotationTrack)` | delegate |
| R/igvR.R:931-1010 | `.displayQuantitativeTrack` (bedGraph) | `.bedGraphTrackPayload` | delegate |
| R/igvR.R:1012-1060 | `.displayBedpeInteractionsTrack` | `.bedpeTrackPayload` + new JS `loadBedpeTrackFromFile` (unverified in browser, m1 Q11) | delegate |
| R/igvR.R:1062-1117 | `.displayGWASTrack` | igvShiny `GWASTrack` is a different class (no Track parent); igvR's not ported | keep igvR class or map to igvShiny `GWASTrack` + `display()` (plan Q2) |
| R/igvR.R:1119-1164 | `.displayGWASUrlTrack` | as above (`data.mode = "url"`) | as above |
| R/igvR.R:1166-1215 | `.displayGFF3Track` | `.gff3TrackPayload` / `.gff3UrlTrackPayload` | delegate |
| R/igvR.R:687,694,734,741,868,905,942,981,1016,1038,1067,1085,1186,1195 | `tempfile()` in session tempdir + `sprintf("%s?%s", igv@uri, temp.filename)` (absolute path in URL) | `.trackFile(session, ext)` -> `tracks/<file>` | delete (builders own file naming) |

### 3b. S4 Track class files (name clash when igvR and igvShiny are both attached)

igvShiny (night/m1-4) exports the same class and constructor names, plus
`trackInfo`/`trackSize` generics. Attaching both masks one set, and an igvR
object passed to `igvShiny::loadTrack()` has no method (different class
definition, same name). M2 must delete the igvR copies and re-export
igvShiny's.

| file | igvR class (exported) | igvShiny night/m1-4 | M2 action |
|---|---|---|---|
| R/Track.R (1-121; brewer.pal at :82, generics trackInfo/trackSize at :27-28) | `Track`, `trackInfo`, `trackSize` | same names, `R/tracks.R` | delete; `importFrom(igvShiny, ...)` + re-export |
| R/igvAnnotationTrack.R | `igvAnnotationTrack` | yes | delete + re-export |
| R/DataFrameAnnotationTrack.R | `DataFrameAnnotationTrack` | yes | delete + re-export |
| R/GRangesAnnotationTrack.R | `GRangesAnnotationTrack` | yes | delete + re-export |
| R/UCSCBedAnnotationTrack.R | `UCSCBedAnnotationTrack` | yes | delete + re-export |
| R/QuantitativeTrack.R | `QuantitativeTrack` | yes | delete + re-export |
| R/DataFrameQuantitativeTrack.R | `DataFrameQuantitativeTrack` | yes | delete + re-export |
| R/GRangesQuantitativeTrack.R | `GRangesQuantitativeTrack` | yes | delete + re-export |
| R/UCSCBedGraphQuantitativeTrack.R | `UCSCBedGraphQuantitativeTrack` (height hard-coded 50) | yes (honours height) | delete + re-export (behaviour change, NEWS later) |
| R/GenomicAlignmentTrack.R | `GenomicAlignmentTrack` | yes (slot `ANY` + validity) | delete + re-export |
| R/RemoteAlignmentTrack.R | `RemoteAlignmentTrack` | yes (bamIndex default `<url>.bai`) | delete + re-export |
| R/CramTrack.R | `CramTrack` | yes | delete + re-export |
| R/VariantTrack.R (`setClassUnion("VCF.or.NULL")` at :5) | `VariantTrack` | yes (`vcf.obj` is `ANY`) | delete + re-export; class union disappears |
| R/BedpeInteractionsTrack.R | `BedpeInteractionsTrack` | yes | delete + re-export |
| R/GFF3Track.R | `GFF3Track` | yes | delete + re-export |
| R/GWASTrack.R | `GWASTrack` (Track parent, slots chrom.col/pos.col/pval.col/colorTable) | **different** `GWASTrack` (standalone, data.mode/url/...) | clash with no drop-in: plan Q2 |
| R/GWASUrlTrack.R (`@exportClass GWASTrack` typo at :3) | `GWASUrlTrack` | covered by igvShiny `GWASTrack(data.mode = "url")` | keep as igvR wrapper or deprecate (plan Q2) |
| DESCRIPTION Collate | lists all Track files | — | update when files are deleted |
| inst/unitTests/test_Tracks.R | tests igvR Track constructors/trackInfo | igvShiny has its own tests | keep as API regression tests against re-exported classes; drop the ones pinning old slot defaults |

### 3c. Other exported functions that clash with igvShiny exports

igvShiny also exports `getGenomicRegion`, `showGenomicRegion`,
`removeTracksByName` as plain functions `(session, id, ...)` and
`parseAndValidateGenomeSpec`. igvR has S4 generics `getGenomicRegion(obj)`,
`showGenomicRegion(obj, region)`, `removeTracksByName(obj, trackNames)`
(R/igvR.R:50-67) and its own `parseAndValidateGenomeSpec`
(R/genomeSpec.R:94). With both attached the later one masks the other.

| file:line | symbol | M2 action |
|---|---|---|
| R/igvR.R:50-53, 66-67 | generics `getGenomicRegion`, `showGenomicRegion`, `removeTracksByName` | keep igvR generics (public API); call igvShiny with `igvShiny::` and never `import(igvShiny)` wholesale; document the masking (plan R2) |
| R/genomeSpec.R:94-166 | `parseAndValidateGenomeSpec()` (returns options for igvApp.js) | delegate to `igvShiny::parseAndValidateGenomeSpec()` (it produces the widget's `genomeOptions`); re-export igvShiny's to remove the duplicate |
| R/genomeSpec.R:33-54 | `currently.supported.stock.genomes()` (httr fetch of igv genome list) | keep or delegate to `igvShiny::get_basic_genomes()` (plan Q6) |
| R/igvR.R:203-211 | `url.exists()` | keep |

## 4. File serving and fixed ports

| file:line | what | M2 action |
|---|---|---|
| R/igvR.R:19 | prototype port `9000` | delete |
| R/igvR.R:124 | `portRange = 15000:15100` default | transport: `httpuv::randomPort()`; keep arg for compatibility |
| R/igvR.R:85, 91 (roxygen) | docs mention port 9000 / websocket port range | update docs |
| inst/unitTests/test_igvR.R:14 | `portRange = 12000:12020` | update tests |
| R/igvR.R:1487-1541 | `myQP` file server (whole file, no Range) | transport: httpuv `call` serving `/tracks/` (static) and `/session/<token>/dataobj/<name>` with Range, so igvShiny's `serveLocalFile(session, path)` works against the shim session (`session$registerDataObj`) |
| igvShiny `.tracksDir()` (R/utils.R:47) | registers a shiny resource path `tracks` | the igvR server must serve the same directory (`get_tracks_dir()`) itself; shiny resource paths are not used by a bare httpuv server |
| igvShiny JS: `window.location.href + "tracks/..."` (igvShiny.js:695,752,783,922,968,1062,1148) | URLs are relative to page href | the igvR page must be served at `/` with no path or query, otherwise every track url breaks |

## 5. igvR JS frontend, m4, jQuery

| file | what | M2 action |
|---|---|---|
| inst/browserCode/src/igvApp.js (1044 lines) | BrowserViz hub handlers (§2) | delete |
| inst/browserCode/src/igvApp.pre | m4 template, includes jQuery 1.12.4 + jQuery UI 1.12.1 **from code.jquery.com** (network at runtime), igv 2.13.9, browserviz.js | delete |
| inst/browserCode/src/igvApp.css | page css | delete |
| inst/browserCode/dist/igvApp.html | m4 output (bundled igv 2.13.9 inline) | delete |
| inst/browserCode/lib/igv-2.12.6.js, igv-2.12.6-patched.js, igv.min-2.12.6.js, igv-2.13.9.js, igv-2.13.11.js, igv-patched.js | 6 vendored igv.js copies | delete (igvShiny ships igv 3.8.4) |
| inst/browserCode/lib/browserviz.js | BrowserViz client | delete |
| inst/browserCode/makefile | `m4 -P igvApp.pre > dist/igvApp.html` | delete |
| makefile:5,8-9 (`quick`, `all`, `browserCode`) | m4 build step | delete target; `quick` = roxy + install |
| AGENTS.md:14-15 | instructs to rebuild browserCode | update in the stage that deletes browserCode |
| new | `inst/transport/igvR-shim.js` (~30 lines) | add: defines `window.Shiny` (`addCustomMessageHandler`, `setInputValue`) over a WebSocket, loaded before `igvShiny.js` |

## 6. Inherited BrowserViz API used by users (implicit public API)

Not defined in igvR, but reachable from an igvR object and used in docs.

| where | symbol | M2 action |
|---|---|---|
| 9 vignettes (v00-v08), 7 demos, 8 roxygen examples (R/*Track.R, R/igvR.R) | `setBrowserWindowTitle(igv, ...)` | keep: define in igvR (generic + method) so scripts keep working |
| README.md | BrowserViz mention | update docs |
| BrowserViz exports (`closeWebSocket`, `port`, `browserResponseReady`, `getBrowserResponse`, `getBrowserWindowSize`, ...) | reachable today via `Depends: BrowserViz` | delete; `closeWebSocket(igv)` gets an igvR replacement (`close()`/`stopServer`), plan Q5 |
| vignettes/*.Rmd | run only `if (interactive())`-guarded igvR code | keep; update text/screenshots later |

## 7. Not affected (keep)

`R/genomeSpec.R` helpers other than the clash above, `url.exists()`,
`.parseChromLocString()` (R/igvR.R:1544), `inst/extdata`, MotifDb/seqLogo
Suggests, `tests/testthat` scaffolding.
