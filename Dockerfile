# R 4.1.2
FROM rocker/tidyverse:4.1.2

LABEL maintainer="bernibra <bernat.bramon@gmail.com>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('markdown', 'BiocManager', 'datasets', 'shiny', 'shinyjs'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2022-08-01'))"

RUN R -e "BiocManager::install(c('GEOquery', 'ArrayExpress'), version = BiocManager::version())"

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
COPY app .
RUN chown app:app -R /home/app
USER app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]

