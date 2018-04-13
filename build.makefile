all:  docs vig build  check biocCheck

docs:
	R -e "devtools::document()"
vig:
	R -e "devtools::build_vignettes()"

build:
	(cd ..; R CMD build --no-build-vignettes igvR)

install:
	(cd ..; R CMD INSTALL igvR)

check:
	(cd ..; R CMD check `ls -t igvR_* | head -1`)

biocCheck:
	(cd ..; R CMD BiocCheck `ls -t igvR_* | head -1`)
