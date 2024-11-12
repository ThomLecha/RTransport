# Utilise une image de base pour R et Shiny
FROM rocker/shiny:latest

# Copie tous les fichiers de ton projet dans le répertoire de travail du conteneur
COPY . /srv/shiny-server/

# Installe les packages R nécessaires
RUN R -e "install.packages(c('tidyr', 'arrow', 'bslib', 'dplyr', 'ggplot2', 'leaflet', 'minpack.lm', 'lubridate', 'plotly', 'readr', 'reshape2', 'sf', 'shiny', 'shinydashboard', 'stringr', 'data.table', 'shinyWidgets', 'DT'))"

# Expose le port utilisé par Shiny
EXPOSE 3838

# Démarre le serveur Shiny
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]