DATADIR=/Users/paul/github/igvR/vignettes/macs2
BAM=GSM749704_hg19_wgEncodeUwTfbsGm12878CtcfStdAlnRep1.bam
NAME=GSM749704_hg19_chr19

run:
	docker run -v $(DATADIR):/data/ fooliu/macs2 callpeak -t /data/$(BAM) -n $(NAME) --outdir data/


pairedEnd:
	R -e "rmarkdown::render('pairedEnd.Rmd')";
	open pairedEnd.html
	R -e "library(knitr); purl('pairedEnd.Rmd')"

vigExtract:
	R -e "library(knitr); purl('basicIntro.Rmd')"
	R -e "library(knitr); purl('AlzheimersVariantsNearMEF2C.Rmd')"
	R -e "library(knitr); purl('ctcfChipSeq.Rmd')"
