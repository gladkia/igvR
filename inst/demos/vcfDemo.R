library(VariantAnnotation)

  url.data.size <- function(url){
     url.info <- system(sprintf("curl -I -L %s", url), intern=TRUE, ignore.stderr=TRUE)
     file.length.raw.text <- url.info[grep("Content-Length", url.info)]
        # for instance:  "Content-Length" "16048144961\r"
     as.numeric(sub("\\r", "", strsplit(file.length.raw.text, ": ")[[1]][2]))
     }

  url.exists <- function(url){
     url.data.size(url) > 0
     }

      #------------------------------------------------------------------
      # 5k about PILRA/B variant, pval 10e-26 in bellenquez 2022 AD gwas
      #------------------------------------------------------------------
igv <- start.igv("PILRA", "hg19")
url <- "https://gladki.pl/igvR/ampad/NIA-1898/chr7.vcf.gz"
showGenomicRegion(igv, "chr7:100,332,734-100,337,641")
roi <- getGenomicRegion(igv)
gr.slice <- with(roi, GRanges(seqnames="7", IRanges(start=start, end=end)))
require(VariantAnnotation)
vcf <- readVcf(url, "hg19", gr.slice, verbose=TRUE)
mtx.geno <- geno(vcf)$GT
dim(mtx.geno)  # 133 1894
track <- VariantTrack("AMP-AD", vcf)
displayTrack(igv, track)




f <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(f, "hg19")
     # get oriented around the contents of this vcf
start <- 50586118
end   <- 50633733
roi <- GRanges(seqnames="22", ranges=IRanges(start=start, end=end))
     # names=c("gene_79087", "gene_644186")))
vcf.sub <- readVcf(f, "hg19", param=roi)
track <- VariantTrack("chr22-tiny", vcf.sub, displayMode="Collapsed")
showGenomicRegion(igv, sprintf("chr22:%d-%d", start-10000, end+10000))
displayTrack(igv, track)


url <- "https://gladki.pl/igvR/ampad/NIA-1898/chr2.vcf.gz"
url.exists(url)
url.data.size(url)
roi <- GRanges(seqnames="2", IRanges(start=1100000, end=1101000))
vcf <- readVcf(url, "hg19", roi, verbose=TRUE)
mtx.geno <- geno(vcf)$GT
dim(mtx.geno)  # 33 1894

igv <- start.igv("chr2", "hg19")
track <- VariantTrack("AMP-AD", vcf)
displayTrack(igv, track)

showGenomicRegion(igv, "chr22:50301422-50312106")

# try another remote ffile
url2 <- "https://s3.amazonaws.com/igv.org.demo/nstd186.GRCh38.variant_call.vcf.gz"
url2 <- "https://s3.amazonaws.com/igv.org.demo/nstd186.GRCh38.variant_call.vcf.gz"
url.exists(url2)
roi2 <- GRanges(seqnames="22", IRanges(start=1, end=50320248))
vcf2 <- readVcf(url2, "hg38", roi2)
igv <- start.igv("chr2", "hg38")
track <- VariantTrack("nstd186", vcf2, visibilityWindow=-1)
displayTrack(igv, track)


mtx.geno <- geno(vcf2)$GT
dim(mtx.geno)  # 33 1894

tbl.info <- as.data.frame(info(vcf2))
dim(tbl.info)

vcf2


fl <- system.file("extdata", "ex2.vcf", package="VariantAnnotation")
vcf <- readVcf(fl, "hg19", verbose=TRUE)
vcf


f <- "chr2-sub-hg38.vcf"
file.exists(f)
vcf.hg38 <- readVcf(f, "hg38", verbose=TRUE)
tbl.info <- as.data.frame(info(vcf.hg38))
dim(tbl.info)
mtx.geno <- geno(vcf.hg38)$GT
dim(mtx.geno)
