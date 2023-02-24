FROM rocker/r-ver:4.2.0

RUN apt-get update && apt-get install -y --no-install-recommends \
  libgit2-dev \
  libxml2-dev \
  libssl-dev \
  libz-dev \
  libcurl4-gnutls-dev \
  libsodium-dev \
  libpq-dev


RUN mkdir /data

RUN mkdir /app
RUN mkdir /app/ecap11s6
RUN chmod -R 755 /app


# COPY package source
WORKDIR /app/ecap11s6


# RENV
COPY renv.lock .
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cran.wu.ac.at/'))"
RUN R -e "renv::restore()"


# Pkg source
COPY run6.R run6.R
COPY DESCRIPTION .
COPY NAMESPACE .
COPY R R
COPY man man

RUN chmod -R 755 /app

# install ecap11s6
RUN R CMD build .
RUN R -e "install.packages('"$(ls | grep ecap11s6)"', repos = NULL, type = 'source')"

# at docker starts
CMD R -e "source('run6.R')"
# CMD R CMD BATCH run5.R /data/isp/08shift/output.csv/ecap11s5.log
