# R 4.1.2
FROM rocker/r-ubuntu:20.04

LABEL maintainer="bernibra <bernat.bramon@gmail.com>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libudunits2-dev libgeos-dev libgeos++-dev \
    libproj-dev libudunits2-dev libv8-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install.r shiny shinyalert bslib dplyr purrr stringr markdown shinyjs 

RUN Rscript -e "install.packages('BiocManager')"

RUN Rscript -e "BiocManager::install(c('Biobase','GEOquery', 'ArrayExpress'), version = BiocManager::version())"

RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
COPY app .
RUN chown app:app -R /home/app
USER app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]

