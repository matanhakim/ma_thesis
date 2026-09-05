# Reproducible build environment for the thesis.
#
#   docker build -t ma-thesis .
#   docker run --rm -v "$PWD/output:/thesis/output" ma-thesis
#
# The image pins R (rocker/r-ver), Quarto and every R package (renv.lock),
# installs the free David CLM typeface used in the figures and the PDF, and
# runs the whole {targets} pipeline, which ends by rendering the thesis to
# HTML, Word and PDF into output/.

FROM rocker/r-ver:4.5.2

ARG QUARTO_VERSION=1.8.27

# System libraries needed by the R packages (curl, xml2, ragg, textshaping,
# readxl) and the Culmus Hebrew fonts (David CLM replaces David).
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    fontconfig \
    fonts-culmus \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libpoppler-cpp-dev \
    libssl-dev \
    libtiff5-dev \
    libxml2-dev \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/* \
  && fc-cache -f

# Quarto (bundles Pandoc and Typst).
RUN curl -fsSL -o /tmp/quarto.deb \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb" \
  && dpkg -i /tmp/quarto.deb \
  && rm /tmp/quarto.deb

WORKDIR /thesis

# Restore the exact R package versions recorded in renv.lock. Linux binaries
# come from the Posit Public Package Manager, which keeps the build fast; the
# versions themselves are fixed by the lockfile.
ENV RENV_CONFIG_REPOS_OVERRIDE="https://p3m.dev/cran/__linux__/noble/latest" \
    RENV_PATHS_LIBRARY="/thesis/renv/library"
COPY renv.lock renv.lock
RUN Rscript -e "install.packages('renv', repos = 'https://p3m.dev/cran/__linux__/noble/latest')" \
  && Rscript -e "renv::restore(lockfile = 'renv.lock', prompt = FALSE)"

# The project itself (data, code, document).
COPY . .

CMD ["Rscript", "-e", "targets::tar_make()"]
