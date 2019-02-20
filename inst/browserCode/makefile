build: assemble

assemble:
	(cd ./src; m4 -P igvApp.pre > ../dist/igvApp.html)
	ls -l ./dist/igvApp.html

buildPackage:
	(cd ../../..; R CMD build --no-build-vignettes igvR)

installPackage:
	(cd ../../..; R CMD INSTALL `ls -at igvR_* | head -1`)

justR: buildPackage installPackage

