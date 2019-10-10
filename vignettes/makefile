DATADIR=/Users/paul/github/igvR/vignettes/macs2
BAM=GSM749704_hg19_wgEncodeUwTfbsGm12878CtcfStdAlnRep1.bam
NAME=GSM749704_hg19_chr19

run:
	docker run -v $(DATADIR):/data/ fooliu/macs2 callpeak -t /data/$(BAM) -n $(NAME) --outdir data/
