# Global variables ----
YEARS_LIST <- 2018:2022
MONTHS_LIST = 1:12

# Environment ----
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(ggplot2)
library(plotly)
library(gt)
library(leaflet)
library(bslib)

source("./functions.R")

# LOAD DATA----
urls <- create_data_list("./sources.yml")
pax_apt_all <- import_airports_data(unlist(urls$airports))
pax_cie_all <- import_airlines_data(unlist(urls$airlines))
pax_lsn_all <- import_airlinks_data(unlist(urls$links))
airports_location <- st_read(urls$geojson$airport)
list_airports <- unique(pax_apt_all$apt)
default_airport <- list_airports[1]

# Dataframe required for the app ------------------------
traffic_airports <- pax_apt_all %>%
  mutate(traffic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  filter(apt %in% default_airport) %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  )
