#rm(list = ls())
# Environment ----
library(arrow)
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
n = stringr::str_locate(getwd(),"Documents")[2]
outputdir = paste0(stringr::str_sub(getwd(),1,n),"/data")
datatype = c("apt", "cie", "lsn")
datapath = paste0(outputdir,"/asp-", datatype,".parquet")
pax_apt_all = clean_dataframe(read_parquet(datapath[1]))
pax_cie_all = clean_dataframe(read_parquet(datapath[2]))
pax_lsn_all = clean_dataframe(read_parquet(datapath[1]))
airports_location <- st_read("airports.geojson")
list_airports <- unique(pax_apt_all$apt)
#default_airport <- list_airports[1]
default_airport <- "LFPG"

# Dataframe required for the app ------------------------
traffic_airports <- pax_apt_all %>%
  mutate(traffic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  filter(apt %in% default_airport) %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  )
