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

#PLOT ----

plot_traffic_selection = function(df, selected_list){
  df = df %>% filter(selected_var %in% selected_list)
  tmp = df
  tmp$ligne <- 1:nrow(tmp)
  tmp = tmp %>% select(ligne, date, anmois, an, pax)
  mco = tmp %>% filter(anmois<"202003")#modélise jusqu'au COVID
  lm_model <- lm(mco$pax ~ mco$ligne)#regression linéaire des moindres carrés ordinaires MCO
  tmp$predicted_pax <- c(predict(lm_model), rep(NA, nrow(tmp) - length(predict(lm_model))))# Ajout de la colonne avec NA pour les lignes manquantes
  tmp_annuel = tmp %>%
    filter(an == min(an)|an == "2019") %>%
    group_by(an) %>%
    summarise(predicted_pax=sum(predicted_pax)) %>% 
    ungroup() %>% 
    mutate(an=as.integer(an))
  n = tmp_annuel[2,1]-tmp_annuel[1,1]
  tcam = as.numeric((tmp_annuel[2,2]/tmp_annuel[1,2])^(1/n)-1)#calcule un taux de croissance annuel moyen
  tmp = tmp %>%
    mutate(Mpax=round(pax/1000000,3)) %>% 
    mutate(predicted_Mpax=round(predicted_pax/1000000,3)) %>% 
    select(date, predicted_Mpax)
  
  df = df %>% 
    group_by(selected_var,date) %>%
    summarise(Mpax = round(sum(pax, na.rm = T)/1000000,3)) %>%
    ungroup()
  figure_plotly = df %>%
    plot_ly(
      x = ~date, y = ~Mpax,
      type = 'scatter', mode = 'lines+markers', color = ~selected_var
      ) %>% 
    add_trace(x = tmp$date, 
              y = tmp$predicted_Mpax, 
              name = paste0("tcam pre-covid = ",round(100*tcam,1)," %"), 
              mode = 'lines', 
              line = list(color = 'red'))
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
