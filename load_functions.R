##################################
###         R TRANSPORT        ###
##################################

#INIT DATA----
clean_dataframe <- function(df){
  names(df) <- tolower(names(df))  #lower case for variable names
  df <- df %>% 
    mutate(
      anmois = as.character(anmois)
      ) %>% 
    mutate(
      an = str_sub(anmois,1,4),
      mois = str_sub(anmois,5,6)
    ) %>%
    mutate(
      mois = str_remove(mois, "^0+")
    )
  return(df)
}

get_recent_date <- function(df, anmois) { # Find most recent date
  x=max(df$anmois)
  recent_date=as.Date(paste0(x, "01"), format = "%Y%m%d") # Return date in format "YYYYMM"
  return(recent_date)
}

#MAP LEAFLET AIRPORT----
map_leaflet_airport <- function(df, airports_location, months, years){
  palette <- c("green", "orange", "darkred")
  traffic_date <- df %>%
    mutate(
      date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
    ) %>%
    filter(mois %in% months, an %in% years) %>% 
    mutate(traffic = apt_pax_dep+apt_pax_arr+apt_pax_tr) %>% 
    group_by(apt) %>%
    summarise(traffic = round(sum(traffic)/1000000,1)) %>%
    ungroup() %>% 
    mutate(volume = case_when(
      (traffic > 1)~3,
      (traffic >= 0.1)~2,
      (traffic < 0.1)~1
      )
    ) %>% 
    mutate(color = palette[volume])
  
  traffic_airports <- airports_location %>%
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

#PLOT AIRPORT LINE----
plot_airport_line <- function(df, selected_airport){
  traffic_airports <- df %>% filter(apt %in% selected_airport)
  figure_plotly <- traffic_airports %>%
    plot_ly(
      x = ~date, y = ~apt_pax,
      text = ~apt_nom,
      hovertemplate = paste("<i>Airport:</i> %{text}<br>Traffic: %{y}") ,
      type = 'scatter', mode = 'lines+markers')
  return(figure_plotly)
}

# ADDITIONNAL FUNCTIONS ----

#PLOT INDEX PRICE----
#str(iptap)
#selected_flows = c("fra_ens","met_inter_met","met_inter_om")
#df=iptap
plot_price_index = function(df, selected_flows){
  price = df %>%
    select("date",all_of(selected_flows))
  data_long <- melt(price, id.vars = "date", variable.name = "Variable", value.name = "Value")  # Transforme les données en format long pour faciliter le tracé
    
  #figure_plotly <- price %>%
  figure_plotly <- data_long %>%
    plot_ly(
      x = ~date, y = ~Value, color = ~Variable, type = 'scatter', mode = 'lines') %>%
    layout(
      title = 'Évolution par faisceau géographique',
      xaxis = list(title = 'Date'),
      yaxis = list(title = 'IPTAP'),
      legend = list(title = list(text = '<b>Faisceaux</b>'))
    )
  return(figure_plotly)
}
