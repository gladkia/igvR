FROM bioconductor/bioconductor_docker:devel

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# System deps for Rsamtools (explicit even if image should have them)
RUN apt-get update && apt-get install -y \
    libbz2-dev \
    liblzma-dev \
    zlib1g-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Layer 1a: remotes from CRAN (always works, needed for GitHub fallbacks)
RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org')"

# Layer 1b: BiocParallel — Bioc 3.23 devel tarball has 404, use GitHub fallback
RUN Rscript -e " \
  tryCatch( \
    BiocManager::install('BiocParallel', update=FALSE, ask=FALSE), \
    error=function(e) { \
      cat('BiocManager 404, installing BiocParallel from GitHub\n'); \
      remotes::install_github('Bioconductor/BiocParallel') \
    } \
  ); \
  if (!'BiocParallel' %in% installed.packages()[,'Package']) stop('BiocParallel install failed') \
"

# Layer 1c: simple packages — no heavy C++ deps
RUN Rscript -e "BiocManager::install(c('RUnit', 'BiocGenerics', 'GenomicRanges', \
                                        'httpuv', 'RColorBrewer', 'httr', \
                                        'knitr', 'rmarkdown', 'seqLogo', 'BrowserViz', \
                                        'randomcoloR'), \
                                      update=FALSE, ask=FALSE)"

# Layer 2: Rsamtools (compiles against libbz2/lzma/zlib — needs BiocParallel)
RUN Rscript -e "BiocManager::install('Rsamtools', update=FALSE, ask=FALSE); \
                if (!'Rsamtools' %in% installed.packages()[,'Package']) stop('Rsamtools install failed')"

# Layer 3: packages that need Rsamtools
RUN Rscript -e "BiocManager::install(c('GenomicAlignments', 'rtracklayer', 'VariantAnnotation', 'MotifDb'), \
                                      update=FALSE, ask=FALSE)"

# Layer 4: gDRstyle (separate — may need different repo or has own quirks)
RUN Rscript -e "BiocManager::install('gDRstyle', update=FALSE, ask=FALSE); \
                if (!'gDRstyle' %in% installed.packages()[,'Package']) stop('gDRstyle install failed')"

# Copy the entire package source
COPY . /tmp/igvR_source

# Install igvR itself (remotes respects R's full libPaths, finds BiocManager-installed deps)
RUN Rscript -e "remotes::install_local('/tmp/igvR_source', dependencies=TRUE, upgrade='never')"

# Verify installation and show where it landed
RUN Rscript -e "paths <- .libPaths(); cat('libPaths:\n'); cat(paste(paths, collapse='\n')); cat('\n'); if (!'igvR' %in% installed.packages()[,'Package']) stop('igvR not installed!')"

# igvShiny needed for test_genomeSpec.R extdata (depends on igvR, install after)
RUN Rscript -e " \
  tryCatch( \
    BiocManager::install('igvShiny', update=FALSE, ask=FALSE), \
    error=function(e) { \
      cat('BiocManager failed, trying GitHub\n'); \
      remotes::install_github('Bioconductor/igvShiny') \
    } \
  )"

# Set working directory for `make check-podman`
WORKDIR /pkg
