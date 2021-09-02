FROM rocker/geospatial

RUN sudo apt-get update \
  && apt-get install -y --no-install-recommends \
    gdal-bin \
    aria2 \
    libpython2-dev \
    libudunits2-dev \
    libgdal-dev \
    libjq-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libnode-dev \
    libssl-dev \
    libcairo2-dev

# install gsutils
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
   curl \
   apt-transport-https \
   ca-certificates \
   gnupg \
  && echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
  && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
       google-cloud-sdk

RUN install2.r --error \
   stringr \
   R.utils \
   sqldf \
   dplyr \
   DBI \
   RSQLite \
   magrittr

RUN R -e "devtools::install_github('appelmar/gdalcubes_R')"

# install sen2tools from repository
COPY ./ /usr/local/src/sen2tool/
WORKDIR /usr/local/src/sen2tool/
RUN R -e "devtools::install('.')"

# remove temporary files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds /usr/local/src/sen2tools/ \
    && rm -rf /var/lib/apt/lists/*

