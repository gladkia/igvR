default:
	@echo quick [browserCode roxy install]

quick:	browserCode roxy install

all:  browserCode roxy vig build install check


browserCode:
	(cd inst/browserCode; make build.js)

roxy:
	R -e "devtools::document()"

vig:
	R -e "devtools::build_vignettes()"


build:
	(cd ..; R CMD build igvR)

install:
	(cd ..; R CMD INSTALL --no-test-load igvR)

check:
	(cd ..; R CMD check --no-manual --ignore-vignettes `ls -t igvR_* | head -1`)

biocCheck:
	(cd ..; R CMD BiocCheck `ls -t igvR_* | head -1`)

test:
	(export BATCH_TEST_MODE="on"; for x in inst/unitTests/test_*.R; do echo ============== $$x; R -f $$x; done)

site:
	R -e "devtools::build_site()"
