# Utilise une image de base pour R et Shiny
FROM rocker/shiny:latest

# Met à jour le gestionnaire de paquets et installe les bibliothèques système nécessaires pour sf
RUN apt-get update && apt-get install -y \
    libudunits2-0 \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev && \
    rm -rf /var/lib/apt/lists/*  # Nettoie le cache pour réduire la taille de l'image

# Copie tous les fichiers de ton projet dans le répertoire de travail du conteneur
COPY . /srv/shiny-server/

# Installe les packages R nécessaires
RUN R -e "install.packages(c('tidyr', 'arrow', 'bslib', 'dplyr', 'ggplot2', 'leaflet', 'minpack.lm', 'lubridate', 'plotly', 'readr', 'reshape2', 'sf', 'shiny', 'shinydashboard', 'stringr', 'data.table', 'shinyWidgets', 'DT'))"

# Expose le port utilisé par Shiny
EXPOSE 3838

# Démarre le serveur Shiny
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]