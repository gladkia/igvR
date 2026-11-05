# igvR M2 prototype: serve the igvShiny htmlwidget from httpuv, with a
# WebSocket shim standing in for Shiny. NOT part of igvR's exported API, not
# collated, not loaded by the package. Source it by hand:
#
#   source(system.file(package = "igvR", "prototype", "widgetServer.R"))
#   p <- igvWidgetPrototype("hg38", "chr1:1-20000")
#   protoLoadBed(p, data.frame(chrom = "chr1", start = c(1000, 5000),
#                              end = c(2000, 7000), name = c("a", "b")))
#   protoStop(p)
#
# Requires igvShiny (night/m1-4 or later), httpuv, htmlwidgets, jsonlite.

igvWidgetPrototype <- function(genomeName = "hg38",
                               initialLocus = "all",
                               id = "igvR",
                               open = interactive(),
                               shimFile = system.file(package = "igvR",
                                                      "prototype", "shim.js")) {
  stopifnot(file.exists(shimFile))
  genomeOptions <- igvShiny::parseAndValidateGenomeSpec(
    genomeName, initialLocus = initialLocus)
  widget <- igvShiny::igvShiny(genomeOptions, elementId = id,
                               height = "600px")

  # page + widget dependencies (htmlwidgets.js, igv.min.js, igvShiny.js)
  # under lib/; selfcontained = FALSE needs no pandoc
  site <- tempfile("igvR-proto-")
  dir.create(site)
  page <- file.path(site, "index.html")
  htmlwidgets::saveWidget(widget, page, selfcontained = FALSE,
                          libdir = "lib", title = "igvR")
  file.copy(shimFile, file.path(site, "shim.js"))
  html <- readLines(page, warn = FALSE)
  # the shim must define window.Shiny before igvShiny.js registers handlers
  head <- grep("<head>", html, fixed = TRUE)[1]
  html <- append(html, '<script src="shim.js"></script>', after = head)
  writeLines(html, page)

  self <- new.env(parent = emptyenv())
  self$id <- id
  self$site <- site
  self$ws <- NULL
  self$outbox <- list()
  self$inputs <- new.env(parent = emptyenv())

  # the stand-in for a shiny session: all igvShiny senders and loadTrack()
  # use only sendCustomMessage (and .trackFile, which skips the
  # onSessionEnded hook when it is absent)
  session <- new.env(parent = emptyenv())
  session$userData <- new.env(parent = emptyenv())
  session$ns <- function(x) x
  session$sendCustomMessage <- function(type, message) {
    frame <- as.character(jsonlite::toJSON(
      list(type = type, message = message), auto_unbox = TRUE,
      null = "null", na = "null", dataframe = "columns", digits = NA,
      force = TRUE, rownames = FALSE))
    if (is.null(self$ws)) {
      self$outbox[[length(self$outbox) + 1L]] <- frame
    } else {
      self$ws$send(frame)
    }
    invisible(NULL)
  }
  self$session <- session

  self$port <- httpuv::randomPort()
  self$server <- httpuv::startServer("127.0.0.1", self$port, list(
    staticPaths = list(
      "/" = httpuv::staticPath(site, indexhtml = TRUE),
      "/tracks" = httpuv::staticPath(igvShiny::get_tracks_dir())
    ),
    onWSOpen = function(ws) {
      self$ws <- ws
      for (frame in self$outbox) ws$send(frame)
      self$outbox <- list()
      ws$onMessage(function(binary, text) {
        msg <- jsonlite::fromJSON(text, simplifyVector = TRUE)
        assign(msg$name, msg$value, envir = self$inputs)
      })
      ws$onClose(function() self$ws <- NULL)
    }
  ))
  self$url <- sprintf("http://127.0.0.1:%d/", self$port)
  if (open) {
    viewer <- getOption("viewer", utils::browseURL)
    viewer(self$url)
  }
  self
}

# block (with timeout) until the page has sent input `name`
protoWaitForInput <- function(proto, name, timeout = 10) {
  deadline <- Sys.time() + timeout
  while (!exists(name, envir = proto$inputs, inherits = FALSE)) {
    if (Sys.time() > deadline)
      stop(sprintf("timed out after %ss waiting for input '%s'", timeout, name))
    httpuv::service(50)
  }
  get(name, envir = proto$inputs)
}

# one small bed track through the igvShiny builder (.trackPayload) + sender
protoLoadBed <- function(proto, tbl, trackName = "protoBed", color = "blue") {
  track <- igvShiny::DataFrameAnnotationTrack(trackName, tbl, color = color)
  igvShiny::loadTrack(proto$session, proto$id, track,
                      validateReference = FALSE)
}

protoStop <- function(proto) {
  httpuv::stopServer(proto$server)
  unlink(proto$site, recursive = TRUE)
  invisible(NULL)
}
