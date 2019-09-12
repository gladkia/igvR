quick:	browserCode docs install

all:  browserCode docs vig build install check biocCheck


browserCode:
	(cd inst/browserCode; make assemble)

docs:
	R -e "devtools::document()"
vig:
	R -e "devtools::build_vignettes()"

build:
	(cd ..; R CMD build igvR)

install:
	(cd ..; R CMD INSTALL --no-test-load igvR)

check:
	(cd ..; R CMD check `ls -t igvR_* | head -1`)

biocCheck:
	(cd ..; R CMD BiocCheck `ls -t igvR_* | head -1`)

test:
	 for x in inst/unitTests/test_*.R; do echo ============== $$x; R -f $$x; done
