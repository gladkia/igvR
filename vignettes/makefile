DATADIR=/Users/paul/github/igvR/vignettes/macs2
BAM=GSM749704_hg19_wgEncodeUwTfbsGm12878CtcfStdAlnRep1.bam
NAME=GSM749704_hg19_chr19

run:
	docker run -v $(DATADIR):/data/ fooliu/macs2 callpeak -t /data/$(BAM) -n $(NAME) --outdir data/


vcf:
	R -e "rmarkdown::render('v08.vcf.Rmd')";
	open v08.vcf.html
gwas:
	R -e "rmarkdown::render('v07.gwas.Rmd')";
	open v07.gwas.html
ah:
	R -e "rmarkdown::render('v06.annotationHub.Rmd')";
	open v06.annotationHub.html

pairedEnd:
	R -e "rmarkdown::render('pairedEnd.Rmd')";
	open pairedEnd.html
	R -e "library(knitr); purl('pairedEnd.Rmd')"

tableBrowser:
	R -e "rmarkdown::render('ucscTableBrowser.Rmd')";
	open ucscTableBrowser.html
	R -e "library(knitr); purl('ucscTableBrowser.Rmd')"

stockGenome:
	R -e "rmarkdown::render('stockGenome.Rmd')";
	open stockGenome.html
	R -e "library(knitr); purl('stockGenome.Rmd')"

customGenome:
	R -e "rmarkdown::render('customGenome.Rmd')";
	open customGenome.html
	R -e "library(knitr); purl('customGenome.Rmd')"

ctcfChIP:
	R -e "rmarkdown::render('ctcfChIP.Rmd')";
	open ctcfChIP.html
#	R -e "library(knitr); purl('customGenome.Rmd')"


vigExtract:
	R -e "library(knitr); purl('basicIntro.Rmd')"
	R -e "library(knitr); purl('AlzheimersVariantsNearMEF2C.Rmd')"
	R -e "library(knitr); purl('ctcfChipSeq.Rmd')"
