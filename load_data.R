##################################
###         R TRANSPORT        ###
##################################

n = stringr::str_locate(getwd(),"Documents")[2]
datapath = paste0(stringr::str_sub(getwd(),1,n),"/init.R")
rm(n)
user_app =  file.exists(datapath) # add user item and applications if TRUE, do nothing if FALSE

year_num = 2019:2024 #annees d'observation par ex. c(2019,2022,2023, 2024)
year_char = as.list(as.character(year_num))
month_char = c(paste0("0", 1:9),10:12) #mois d'observation par ex. c("04", "05","06")
month_num = as.list(as.integer(month_char))

if (user_app) {
  library(data.table)
  source(datapath) #run init prog of user
  pax_apt = readRDS(paste0(datadir,"pax_apt.RDS"))
}

datapath_apt = "https://www.data.gouv.fr/fr/datasets/r/884e5754-2ad6-436d-9699-148e3f6e2b7c"
datapath_cie = "https://www.data.gouv.fr/fr/datasets/r/314cfa80-fe1f-4834-b18e-93476eb82c91"
#datapath_lsn = "https://www.data.gouv.fr/fr/datasets/r/a3947c3b-36ae-4aa0-bee2-323f6d684f0e"
datapath_iptap = "https://www.data.gouv.fr/fr/datasets/r/ca158a15-0f41-4528-b370-282ce04e22d4"

pax_apt_all = clean_dataframe(read_parquet(datapath_apt))
pax_cie_all = clean_dataframe(read_parquet(datapath_cie))
#pax_lsn_all = clean_dataframe(read_parquet(datapath_lsn))
iptap = read.csv(datapath_iptap, header=TRUE,sep=";",dec=",")

recent_date = get_recent_date(pax_apt_all,"anmmois")
date_max=as.character(recent_date)
airports_location <- st_read("airports.geojson")
list_airports <- unique(pax_apt_all$apt)
default_airport <- "LFPG"

# Dataframe required for the app ------------------------
traffic_airports <- pax_apt_all %>%
  mutate(traffic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  filter(apt %in% default_airport) %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  )
