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

RUN R -e 'install.packages("remotes")'

RUN Rscript -e 'remotes::install_version("BiocManager",upgrade="never", version = "1.30.23")'

RUN Rscript -e "BiocManager::install(c('Biobase','GEOquery', 'ArrayExpress'), version = BiocManager::version())"

RUN Rscript -e 'remotes::install_version("yaml",upgrade="never", version = "2.3.8")'

RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61.1")'

RUN Rscript -e 'remotes::install_version("bsplus",upgrade="never", version = "0.1.4")'

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home
COPY app ./app/
COPY data ./data/
RUN chown app:app -R /home/app/
RUN chown app:app -R /home/data
USER app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]

