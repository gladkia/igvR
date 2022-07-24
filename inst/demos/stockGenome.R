library(igvR)
igv <- igvR()
stock.genomes <- sort(getSupportedGenomes(igv))
message(sprintf("%d stock.genomes provided by igv.org", length(stock.genomes)))
message(paste(stock.genomes, collpase=" "))
set.seed(37)
random.indices <- sample(1:length(stock.genomes), size=5)
chosen.genomes <- stock.genomes[random.indices]

for(genome in chosen.genomes){
    message(sprintf("setting genome %s", genome))
    setGenome(igv, genome)
    Sys.sleep(1)
    }
