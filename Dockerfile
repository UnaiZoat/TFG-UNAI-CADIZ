# Usar imagen base de R con Shiny pre-instalado
FROM rocker/shiny:4.3.0

# Instalar dependencias del sistema
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/futbol-app


COPY App.R .



COPY descarga_datos_annio.R .
COPY descargaautomaticadatosresultados.R .
COPY descargaautomaticadatostiros.R .
COPY descargaautomaticadatostirosencontra.R .
COPY descargaautomaticadatosgolesafavor.R .
COPY descargaautomaticadatostopgoleadores.R .
COPY descargaautomaticadatosgolesencontra.R .

COPY *.csv ./


RUN mkdir -p data www logs




RUN R -e "install.packages(c('shiny', 'ggplot2', 'dplyr', 'plotly', 'bslib', 'promises', 'future', 'rvest', 'writexl'), repos='https://cran.rstudio.com/')"


EXPOSE 3838


RUN chmod -R 755 /srv/shiny-server/

