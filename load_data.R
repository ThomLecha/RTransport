##################################
###   R TRANSPORT - DATA       ###
##################################

n = stringr::str_locate(getwd(),"Documents")[2]
datadir = stringr::str_sub(getwd(),1,n)
datapath = paste0(datadir,"/init.R")
rm(n)
user_app =  file.exists(datapath) # add user item and applications if TRUE, do nothing if FALSE
user_app = FALSE #test when false
year_num = 2019:2024 #annees d'observation par ex. c(2019,2022,2023, 2024)
year_char = as.list(as.character(year_num))
month_char = c(paste0("0", 1:9),10:12) #mois d'observation par ex. c("04", "05","06")
month_num = as.list(as.integer(month_char))


datapath_apt = paste0(datadir,"/asp-apt.parquet")
datapath_cie = paste0(datadir,"/asp-cie.parquet")
datapath_iptap = paste0(datadir,"/asp-iptap.csv")

tryCatch({
  # Lecture du fichier CSV
  read.csv("https://www.data.gouv.fr/fr/datasets/r/ca158a15-0f41-4528-b370-282ce04e22d4", header=TRUE,sep=";",dec=",")
  
  datapath_apt = "https://www.data.gouv.fr/fr/datasets/r/884e5754-2ad6-436d-9699-148e3f6e2b7c"
  datapath_cie = "https://www.data.gouv.fr/fr/datasets/r/314cfa80-fe1f-4834-b18e-93476eb82c91"
  #datapath_lsn = "https://www.data.gouv.fr/fr/datasets/r/a3947c3b-36ae-4aa0-bee2-323f6d684f0e"
  datapath_iptap = "https://www.data.gouv.fr/fr/datasets/r/ca158a15-0f41-4528-b370-282ce04e22d4"
}, 
# Gestion de l'erreur
error = function(e) {
  # Si une erreur survient, afficher "erreur"
  print("erreur de chargement sur internet, chargement de secours sur DD")
  #datapath_apt = paste0(datadir,"/asp-apt.parquet")
  #datapath_cie = paste0(datadir,"/asp-cie.parquet")
  #datapath_iptap = paste0(datadir,"/asp-iptap.csv")
})

iptap = clean_dataframe(read.csv(datapath_iptap, header=TRUE,sep=";",dec=",")) %>% mutate(date = as.Date(paste(anmois, "15", sep=""), format = "%Y%m%d"))
pax_apt_all = clean_dataframe(read_parquet(datapath_apt)) %>% 
  mutate(apt_pax = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>% 
  mutate(date = as.Date(paste(anmois, "15", sep=""), format = "%Y%m%d"))
pax_cie_all = clean_dataframe(read_parquet(datapath_cie)) %>% mutate(date = as.Date(paste(anmois, "15", sep=""), format = "%Y%m%d"))
#pax_lsn_all = clean_dataframe(read_parquet(datapath_lsn)) %>% mutate(date = as.Date(paste(anmois, "15", sep=""), format = "%Y%m%d"))

recent_date = max(pax_apt_all$date)

airports_location = st_read("airports.geojson")
date_max=as.character(recent_date)
list_airports = sort(unique(pax_apt_all$apt))
list_cie = sort(unique(pax_cie_all$cie_nom))
list_price_flows = setdiff(names(iptap), c("anmois","date","an","mois"))

date_max2=NULL
list_airports_end = NULL
list_cou = NULL
list_ihh_flows=NULL
list_traffic_flows = NULL

if (user_app) {
  library(data.table)
  source(datapath) #run init prog of user
  pax_apt = readRDS(paste0(datadir,"pax_apt.RDS"))
  #pax_apt = readRDS(paste0(datadir,"pax_apt.RDS")) %>% mutate(anmois=paste0(an,mois))
  # Dataframe required for the app ------------------------
  recent_date2 = max(pax_apt$date)
  date_max2=as.character(recent_date2)
  list_anmois = unique((paste0(pax_apt$an,pax_apt$mois)))
  list_airports_end = sort(unique(apt$aptoaci))
  list_cou = sort(unique(pax_apt$countrynameoaciapt2))
  list_ihh_flows=sort(unique(pax_apt$e3fscapt2))
  list_traffic_flows = sort(unique(pax_apt$e3fscapt2))
  
  #a="2019"
  #m="06"
  #airp = "LFOB"
  tmp = NULL
  for (airp in intersect(apt_fra,list_airports)) { 
    for (anmois in list_anmois) {
      a = substr(anmois,1,4)
      m = substr(anmois,5,6)
      df = pax_apt %>% 
        filter (an==a & mois==m) %>% 
        filter(apt1==airp | apt2==airp) %>% 
        mutate(route = case_when(
          apt1==airp ~ paste0(apt1,apt2),
          apt2==airp ~ paste0(apt2,apt1)
        )) %>% 
        filter(pax>159)#correspond à au moins 1 vol A/R par semaine d'au moins 20 passagers, soit au moins 160 passagers par mois
      dest = length(unique(df$route))
      
      #df = pax_apt %>% 
      #filter (an==a & mois==m) %>% 
      #filter(formulaire==airp) %>% 
      #filter(pax>159) %>%
      #summarise(sieges = sum(sieges, na.rm = T), pax = round(sum(pax, na.rm = T)), mvt = sum(mvt, na.rm = T))
      #x = c(a, m,airp,dest,df$sieges, df$pax, df$mvt)
      x = c(a, m,airp,dest)
      tmp = rbind(x,tmp)
    }
  }
  tmp = as.data.frame(tmp)
  row.names(tmp)=NULL
  names(tmp)=c("an","mois","apt","dest")
  #names(tmp)=c("an","mois","apt","dest","sieges","pax","mvt")
  traffic_connect = tmp %>%
    mutate(dest=as.integer(dest)) %>% 
    mutate(date = as.Date(paste(an,mois, "15", sep=""), format = "%Y%m%d")) %>% 
    tidyr::pivot_wider(names_from=apt,values_from=dest,names_prefix="")
  rm(df,tmp)

  traffic_ihh = pax_apt %>% 
    filter(fluxindic==1) %>%
    group_by(cielabel, e3fscapt2, date) %>%
    summarise(pax = sum(pax, na.rm = T)) %>% 
    ungroup
  tmp = traffic_ihh %>%
    group_by(e3fscapt2, date) %>%
    summarise(paxtot = sum(pax, na.rm = T)) %>%
    ungroup %>% 
    select(e3fscapt2, date, paxtot)
  traffic_ihh = traffic_ihh %>% 
    inner_join(tmp, by=c("e3fscapt2", "date")) %>% 
    mutate(share2 = (pax/paxtot)^2) %>%
    group_by(e3fscapt2, date) %>%
    summarise(ihh = round(sum(share2, na.rm = T),3)) %>%
    ungroup %>%
    tidyr::pivot_wider(names_from=e3fscapt2,values_from=ihh,names_prefix="") %>% 
    select(-"NA")
  rm(tmp)
  
  traffic_cou = pax_apt %>%
    filter(fluxindic == 1) %>% 
    #mutate (date = as.Date(paste0(anmois, "01"), format = "%Y%m%d")) %>%
    group_by(date, countrynameoaciapt2) %>%
    summarise(pax = round(sum(pax, na.rm = T))) %>% 
    ungroup() %>% 
    tidyr::pivot_wider(names_from=countrynameoaciapt2,values_from=pax,names_prefix="")

  traffic_route = pax_apt %>%
    filter(fluxindic == 1) %>% 
    #mutate (date = as.Date(paste0(anmois, "01"), format = "%Y%m%d")) %>%
    group_by(date, apt1, apt2) %>%
    summarise(pax = round(sum(pax, na.rm = T))) %>% 
    ungroup() %>% 
    mutate(apt1apt2=paste0(apt1,apt2)) %>% 
    select(date,apt1apt2,pax) %>% 
    tidyr::pivot_wider(names_from=apt1apt2, values_from=pax,names_prefix="")
  
  traffic_flows = pax_apt %>%
    filter(fluxindic == 1) %>% 
    #mutate (date = as.Date(paste0(anmois, "01"), format = "%Y%m%d")) %>%
    #group_by(date, faisceau) %>%
    group_by(date, e3fscapt2) %>%
    summarise(pax = round(sum(pax, na.rm = T)/1000000,3)) %>% 
    ungroup() %>% 
    #tidyr::pivot_wider(names_from=faisceau,values_from=pax,names_prefix="")
    tidyr::pivot_wider(names_from=e3fscapt2,values_from=pax,names_prefix="")
}
