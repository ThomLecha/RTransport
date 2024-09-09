##################################
###         R TRANSPORT        ###
##################################

# INPUT DATE SELECTION----
input_date <- function(input_id, label, maxDate) {
  shinyWidgets::airDatepickerInput(
    inputId = input_id,
    label = label,
    value = maxDate,
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

#df=pax_cie_all %>% mutate(selected_var=cie_nom,pax = cie_pax)
#selected_list = "RYANAIR"
#title_plot = "Trafic cie"
#title_y="pax"
#legend_y="année"

plot_traffic_selection = function(df, selected_list){
  df = df %>% 
    filter(selected_var %in% selected_list) %>% 
    select(selected_var, date, anmois, an, pax)
  predict1 = df %>% filter(anmois<"202003")#modélise jusqu'au COVID
  predict1$ligne <- 1:nrow(predict1)
  lm_model <- lm(predict1$pax ~ predict1$ligne)#regression linéaire par les moindres carrés ordinaires MCO
  #n = length(predict(lm_model))
  y_pred = predict(lm_model)
  n = length(y_pred)
  tcam=round(100*((y_pred[n]/y_pred[1])^(12/n)-1),1)
  df$predicted_pax <- c(y_pred, rep(NA, nrow(df)-n))# Ajout de la colonne avec NA pour les lignes manquantes
  predict2 = df %>% filter(anmois>"202104")#modélise après le COVID
  predict2$ligne <- 1:nrow(predict2)
  param_init <- c(a = 1, b = 1, c = 1)  # Valeurs initiales a, b, c du modèle y = a/x + b + c*x
  resultat <- nls.lm(param_init, fn = objectif, x = predict2$ligne, y = predict2$pax) # Ajustement du modèle non-linéaire
  coefficients <- resultat$par  # Récupérer les coefficients ajustés
  a <- coefficients[1]
  b <- coefficients[2]
  c <- coefficients[3]
  y_pred <- modele(c(a, b, c), predict2$ligne)  # Générer les prédictions avec le modèle ajusté
  n = length(y_pred)
  df$predicted2_pax <- c(rep(NA, nrow(df) - n), y_pred)# Ajout de la colonne avec NA pour les lignes manquantes
  tcam2=round(100*((y_pred[n]/y_pred[n-1])^12-1),1)

  df = df %>%
    mutate(Mpax=round(pax/1000000,3)) %>% 
    mutate(predicted_Mpax=round(predicted_pax/1000000,3)) %>% 
    mutate(predicted2_Mpax=round(predicted2_pax/1000000,3)) %>% 
    select(date, Mpax, predicted_Mpax, predicted2_Mpax, selected_var)
  figure_plotly = df %>%
    plot_ly(
      x = ~date, y = ~Mpax,
      type = 'scatter', mode = 'lines+markers', color = ~selected_var
      ) %>% 
    add_trace(
      y = ~predicted_Mpax,
      mode = 'lines', 
      line = list(color = 'red')) %>% 
    add_trace(
      y = ~predicted2_Mpax,
      mode = 'lines', 
      line = list(color = 'red')) %>% 
    layout(
      showlegend = FALSE,
      title = list(
        text = paste0("tcam pre-covid = ",tcam,"%, post-covid = ",tcam2,"%"),# post-covid = ", round(100*tcam2,1)," %"),     # Le texte du titre
        x = 0.5,              # Centrer le titre horizontalement
        xanchor = "center",   # Ancrage du titre au centre
        y = 0.95,              # Positionner légèrement au-dessus du graphique
        yanchor = "top"       # Ancrage en haut
      ),
      shapes = list(
        list(
          type = "rect",
          x0 = as.Date("2020-03-15"), x1 = as.Date("2021-04-15"),
          y0 = min(df$Mpax), y1 = max(df$Mpax),
          fillcolor = "rgba(100, 100, 200, 0.3)",  # Couleur de la zone (avec transparence)
          line = list(width = 0),  # Pas de bordure
          layer = "below") # Placer la forme en dessous des traces
      ),
      
      # Ajouter un texte "COVID" au centre de la zone hachurée
      annotations = list(
        list(
          x = as.Date("2020-09-01"),  # Date au centre de la période
          y = max(df$Mpax) * 0.5,     # Position verticale du texte à 50% du max des valeurs de Mpax
          text = "COVID",
          xref = "x",  # Référencer l'axe x
          yref = "y",  # Référencer l'axe y
          showarrow = FALSE,
          font = list(size = 20, color = "black", family = "Arial", bold = TRUE),  # Texte en gras
          xanchor = 'center',
          yanchor = 'middle')
      )
      )
  
  return(figure_plotly)
}

plot_evol = function(df, selected, title_plot, title_y, legend_y){
  df = df %>% select("date",all_of(selected))
  data_long = melt(df, id.vars = "date", variable.name = "Variable", value.name = "Value")  # Transforme les données en format long pour faciliter le tracé
  figure_plotly <- data_long %>%
    plot_ly(x = ~date, y = ~Value, color = ~Variable, type = 'scatter', mode = 'lines') %>%
    layout(
      title = title_plot,
      xaxis = list(title = 'Date'),
      yaxis = list(title = title_y),
      legend = list(title = list(text = legend_y))
    )
  return(figure_plotly)
}
