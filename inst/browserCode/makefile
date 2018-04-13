build: webpack assemble buildPackage installPackage

assemble:
	(cd ./dist; m4 -P igv.html-template | sed s/\`//g > igv.html)
	ls -l ./dist/igv.html

buildPackage:
	(cd ../../..; R CMD build --no-build-vignettes igvR)

installPackage:
	(cd ../../..; R CMD INSTALL `ls -at igvR_* | head -1`)

justR: buildPackage installPackage

webpack:
	npm update
	webpack

test:
	(cd ./dist; pwd)

all: clean install run

clean:
	- rm -rf dist/
	- rm -rf node_modules

install:
	npm install
