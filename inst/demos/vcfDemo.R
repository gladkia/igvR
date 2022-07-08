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


url <- "https://igv-data.systemsbiology.net/ampad/NIA-1898/chr2.vcf.gz"
url.exists(url)
url.data.size(url)
roi <- GRanges(seqnames="2", IRanges(start=1100000, end=1101000))
vcf <- readVcf(url, "hg19", roi, verbose=TRUE)
mtx.geno <- geno(vcf)$GT
dim(mtx.geno)  # 33 1894

# try another remote ffile
url2 <- "https://s3.amazonaws.com/igv.org.demo/nstd186.GRCh38.variant_call.vcf.gz"
url.exists(url2)
roi2 <- GRanges(seqnames="22", IRanges(start=1100000, end=1101000))
vcf2 <- readVcf(url2, "hg38", roi2)
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
