##################################
###   R TRANSPORT - FUNCTIONS  ###
##################################

# INPUT DATE SELECTION----
input_date <- function(input_id, label, maxDate) {
  shinyWidgets::airDatepickerInput(
    inputId = input_id,
    label = label,
    #value = maxDate,
    multiple = TRUE,
    view = "months",
    minView = "months",
    minDate = "2010-01-01",
    maxDate = maxDate,
    dateFormat = "MMMM yyyy",
    language = "en"
  )
}

# CLEAN DATA----
clean_dataframe <- function(df){
  names(df) <- tolower(names(df))  #lower case for variable names
  df <- df %>% 
    mutate(anmois = as.character(anmois)) %>% 
    mutate(an = str_sub(anmois,1,4),mois = str_sub(anmois,5,6)) %>%
    mutate(mois = str_remove(mois, "^0+"))
  return(df)
}

get_recent_date <- function(df, anmois) { # Find most recent date
  x=max(df$anmois)
  recent_date=as.Date(paste0(x, "01"), format = "%Y%m%d") # Return date in format "YYYYMM"
  return(recent_date)
}

# MAP LEAFLET AIRPORT----
map_leaflet_airport <- function(months, years){
  palette <- c("green", "orange", "darkred")
  traffic_date <- pax_apt_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")) %>%
    filter(mois %in% months, an %in% years) %>% 
    mutate(traffic = apt_pax_dep+apt_pax_arr+apt_pax_tr) %>% 
    group_by(apt) %>%
    summarise(traffic = round(sum(traffic)/1000000,1)) %>%
    ungroup() %>% 
    mutate(volume = case_when(
      (traffic > 1)~3,
      (traffic >= 0.1)~2,
      (traffic < 0.1)~1)
    ) %>% 
    mutate(color = palette[volume])
  
  traffic_airports = airports_location %>%
    inner_join(traffic_date, by = c("Code.OACI" = "apt"))

  icons <- awesomeIcons(
    icon = 'plane',
    iconColor = 'black',
    library = 'fa',
    markerColor = traffic_airports$color
  )
  carte_interactive <- leaflet(traffic_airports) %>% addTiles() %>%
    addAwesomeMarkers(
      icon=icons[],
      label=~paste0(Nom, "", " (",Code.OACI, ") : ", traffic, " millions de voyageurs")
    )
  return(carte_interactive)
}

#PLOT & PREDICTION MODELS----
# Définir le modèle: y = a/x + b + c*x
modele <- function(par, x) {
  a <- par[1]
  b <- par[2]
  c <- par[3]
  return(a/x + b + c*x)
}
# Fonction objectif (somme des carrés des écarts)
objectif <- function(par, x, y) {
  y_pred <- modele(par, x)
  return(y - y_pred)
}

#TESTING ZONE
#selected_list = c("met_inter_met","met_inter_om")
#selected_list = c("met_inter_met")
#df = iptap

#selected_list = c("RYANAIR","TRANSAVIA FRANCE")
#df=pax_cie_all %>% mutate(selected_var=cie_nom,pax = cie_pax)

plot_traffic_selection = function(df, selected_list){
  if("selected_var" %in% colnames(df)){# transpose le dataframe df s'il est sous la forme d'une variable, ex. pax, qu'il faut positionner en colonne, ex. c("Pax AF", "Pax Lufthansa")
    df = df %>% 
      filter(selected_var %in% selected_list) %>% 
      group_by(date, selected_var) %>%
      summarise(Value = sum(pax, na.rm=T)) %>%
      ungroup() %>%
      tidyr::pivot_wider(names_from = selected_var, values_from = Value)
  }
  df = df %>%
    select(date,all_of(selected_list)) %>% 
    arrange(date)
  date_first = year(min(df$date))
  precovid = df %>% filter(date<as.Date("2020-03-01"))#modélise jusqu'au COVID
  postcovid = df %>% filter(date>as.Date("2021-04-30"))#modélise après le COVID
  evol_precovid=NULL
  tcam_precovid=NULL
  evol_postcovid=NULL
  tcam_postcovid=NULL
  i=0
  for (var in selected_list){#calculate aagr for each selectionned variable
    i=i+1
    tmp = precovid %>%
      select(date,var) %>% 
      rename("Value"=var)
    tmp$ligne <- 1:nrow(tmp)
    lm_model <- lm(tmp$Value ~ tmp$ligne)#regression linéaire par les moindres carrés ordinaires MCO
    y_pred = predict(lm_model)
    n = length(y_pred)
    evol_precovid[i]=(y_pred[12]-y_pred[1])/100
    Value_t1 = as.integer(tmp %>% filter(year(date)==date_first) %>% summarise(Value=sum(Value)))
    Value_t2 = as.integer(tmp %>% filter(year(date)=="2019") %>% summarise(Value=sum(Value)))
    tcam_precovid[i] = round(100*((Value_t2/Value_t1)^(1/(2019-date_first))-1),1)
    var2 = paste0(var,"_precovid")
    df[[var2]] = c(y_pred, rep(NA, nrow(df)-n))# Ajout de la colonne avec NA pour les lignes manquantes
    tmp = postcovid %>%
      select(date,var) %>% 
      rename("Value"=var)
    tmp$ligne <- 1:nrow(tmp)
    param_init <- c(a = 1, b = 1, c = 1)  # Valeurs initiales a, b, c du modèle y = a/x + b + c*x
    resultat <- nls.lm(param_init, fn = objectif, x = tmp$ligne, y = tmp$Value) # Ajustement du modèle non-linéaire
    coefficients <- resultat$par  # Récupérer les coefficients ajustés
    a <- coefficients[1]
    b <- coefficients[2]
    c <- coefficients[3]
    y_pred <- modele(c(a, b, c), tmp$ligne)  # Générer les prédictions avec le modèle ajusté
    n = length(y_pred)
    evol_postcovid[i]=(y_pred[n]-y_pred[n-1])
    var2 = paste0(var,"_postcovid")
    df[[var2]] = c(rep(NA, nrow(df) - n), y_pred)# Ajout de la colonne avec NA pour les lignes manquantes
    tcam_postcovid[i]=round(100*((y_pred[n]/y_pred[n-1])^12-1),1)
  }
  title_plot=paste0(unlist(tcam_precovid), collapse = "% & ")
  title_plot=paste0("tcam pre-covid : ", title_plot,"%, tcam post-covid : ")
  tmp=paste0(unlist(tcam_postcovid), collapse = "% & ")
  title_plot=paste0(title_plot,tmp,"%")
  data_long = reshape2::melt(df, id.vars = "date", variable.name = "Variable", value.name = "Value")  # Transforme les données en format long pour faciliter le tracé
  figure_plotly <- data_long %>%
    plot_ly(x = ~date, y = ~Value, color = ~Variable, type = 'scatter', mode = 'lines') %>%
    layout(
      title = list(text = title_plot,
                   x = 0.5,              # Centrer le titre horizontalement
                   xanchor = "center",   # Ancrage du titre au centre
                   y = 0.95,              # Positionner légèrement au-dessus du graphique
                   yanchor = "top"       # Ancrage en haut
      ),
      xaxis = list(title = 'Date'),
      shapes = list(list(type = "rect",
                         x0 = as.Date("2020-03-15"), x1 = as.Date("2021-05-01"),
                         y0 = min(data_long$Value, na.rm = TRUE), y1 = max(data_long$Value, na.rm = TRUE),
                         fillcolor = "rgba(100, 100, 200, 0.3)",  # Couleur de la zone (avec transparence)
                         line = list(width = 0),  # Pas de bordure
                         layer = "below") # Placer la forme en dessous des traces
      ),
      annotations = list(list(x = as.Date("2020-10-01"),  # Date au centre de la période
                              y = max(data_long$Value, na.rm = TRUE) * 0.8,     # Position verticale du texte à 50% du max des valeurs de Mpax
                              text = "COVID",
                              xref = "x",  # Référencer l'axe x
                              yref = "y",  # Référencer l'axe y
                              showarrow = FALSE,
                              font = list(size = 20, color = "black", family = "Arial", bold = TRUE),  # Texte en gras
                              xanchor = 'center',
                              yanchor = 'middle')
    ))
  return(figure_plotly)
}
