# run_local_ci.R

cat("R library paths:\n"); cat(paste(.libPaths(), collapse="\n")); cat("\n")

# Load the igvR package (already installed in the Containerfile)
suppressPackageStartupMessages(library(igvR))

# Run gDRstyle checks, skipping linting, tests, and pkgdown build
cat("
--- Running gDRstyle::checkPackage (skipping lint, tests, and pkgdown) ---
")
gDRstyle::checkPackage(pkgName = "igvR", repoDir = ".", skip_lint = TRUE, skip_tests = TRUE, skip_pkgdown = TRUE)

# Exit cleanly after checks
quit(save="no")
