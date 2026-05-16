FROM rocker/shiny:4.5.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libicu-dev \
    libpng-dev \
    libssl-dev \
    libxml2-dev \
    make \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . /app

RUN R -q -e "install.packages('pak', repos = 'https://cloud.r-project.org')"
RUN R -q -e "pak::pkg_install(c('roxygen2', 'devOpifex/mcpr', 'ambiorix', '.'))"

ENV OFFENSIVETOOLMAPPER_DATA_DIR=/app/inst/extdata

EXPOSE 8788 3000
