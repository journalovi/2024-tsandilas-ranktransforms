# Based on https://github.com/analythium/quarto-docker-examples/blob/main/Dockerfile.base

# Version number of Quarto to download and use
ARG QUARTO_VERSION="1.4.554"

ARG OS_USERNAME=quarto
ARG UID=1000
ARG GID=1000

FROM eddelbuettel/r2u:20.04

# librsvg2-bin is to allow SVG conversion when rendering a PDF file
# (will install the rsvg-view binary)
RUN set -e -x && \
    apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    pandoc-citeproc \
    curl \
    gdebi-core \
    librsvg2-bin \
    python3.8 python3-pip \
    nodejs \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN set -e -x && \
    install.r shiny jsonlite ggplot2 htmltools remotes renv knitr rmarkdown quarto lmerTest effectsize ARTool plotly tidyverse reshape2 dplyr ggplot2 tidyr bib2df stringr brms faux egg latex2exp ggExtra patchwork crosstalk kableExtra  gridExtra ordinal nparLD cowplot

# Download and install Quarto
ARG QUARTO_VERSION
RUN set -e -x && \
    curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && gdebi --non-interactive quarto-linux-amd64.deb \
    && rm -f quarto-linux-amd64.deb

# Should be done for the user; won't work if done for root
# (quarto will say that "tinytex is not installed")
ARG OS_USERNAME
ARG UID
ARG GID

RUN set -e -x && \
    groupadd -g $GID -o "${OS_USERNAME}" && \
    useradd -m -u $UID -g $GID -o -s /bin/bash "${OS_USERNAME}"

USER "${OS_USERNAME}"

# Install tools like tinytex to allow conversion to PDF
RUN set -e -x && \
    quarto install tool tinytex --update-path

RUN set -e -x && \
    printf "\e[0;105m%s\e[0;0m\n" "Run tlmgr update" \
    && ~/.TinyTeX/bin/x86_64-linux/tlmgr update --self --all && \
    ~/.TinyTeX/bin/x86_64-linux/fmtutil-sys --all

# See https://github.com/rstudio/tinytex/issues/426 for explanation
RUN set -e -x && \
    printf "\e[0;105m%s\e[0;0m\n" "Run tlmgr install for a few tinyText packages (needed for PDF conversion)" \
    && ~/.TinyTeX/bin/x86_64-linux/tlmgr install fvextra footnotebackref pagecolor sourcesanspro sourcecodepro titling

USER root

RUN set -e -x && \
    mkdir -p /input

USER "${OS_USERNAME}"

WORKDIR /