# M2 prototype (not part of the igvR API)

`widgetServer.R` starts httpuv on `httpuv::randomPort()`, writes the igvShiny
widget page with `htmlwidgets::saveWidget(selfcontained = FALSE)`, injects
`shim.js` before `igvShiny.js`, serves `/` and `/tracks/`
(`igvShiny::get_tracks_dir()`), and passes a stand-in session (an
environment with `sendCustomMessage`) to `igvShiny::loadTrack()`.

`shim.js` defines `window.Shiny` with `addCustomMessageHandler` and
`setInputValue` over one WebSocket. It has no `outputBindings`, so
htmlwidgets stays in static mode and renders from the JSON spec.

Headless check (needs igvShiny >= 1.9.52 from `night/m1-4`, node >= 22):

```sh
PROTO_DIR=inst/prototype PROTO_URL_FILE=/tmp/url Rscript inst/prototype/verify.R &
sleep 5; node inst/prototype/verify.js "$(cat /tmp/url)"; wait
```

Verified headless: page and its 4 scripts served (200), shim loads before
igvShiny.js, R frames queued before the socket opens are flushed on
connect, the real `igvShiny.js` handlers `removeTracksByName` and
`loadBedTrackFromFile` run and build an igv.js config, the bed url in it is
served, and `setInputValue` frames reach R (`igvReady`, a test input).

UNVERIFIED (needs a real browser): igv.js rendering, htmlwidgets static
render calling `renderValue`, the real `igvReady` from `createBrowser`, the
RStudio/Positron Viewer (and its proxied URLs), stock genome loading.
