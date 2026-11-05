suppressMessages(library(igvShiny))
PROTO_DIR <- Sys.getenv("PROTO_DIR", "inst/prototype")
source(file.path(PROTO_DIR, "widgetServer.R"))
p <- igvWidgetPrototype("hg38", "chr1:1-20000", open = FALSE,
                        shimFile = file.path(PROTO_DIR, "shim.js"))
protoLoadBed(p, data.frame(chrom = "chr1", start = c(1000L, 5000L),
                           end = c(2000L, 7000L), name = c("a", "b")))
cat("queued frames before page connected:", length(p$outbox), "\n")
cat(p$url, "\n", file = Sys.getenv("PROTO_URL_FILE"))
cat("ready input:", protoWaitForInput(p, "igvReady", 30), "\n")
cat("protoDone input:", protoWaitForInput(p, "protoDone", 10), "\n")
protoStop(p)
cat("R side OK\n")
