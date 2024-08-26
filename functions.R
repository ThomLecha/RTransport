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
create_data_from_input <- function(data, years, months){
  data <- data %>%
    filter(mois %in% months, an %in% years)
  return(data)
}
summary_stat_airport <- function(data){
  table2 <- data %>%
    group_by(apt, apt_nom) %>%
    summarise(
      paxdep = round(sum(apt_pax_dep, na.rm = T),3),
      paxarr = round(sum(apt_pax_arr, na.rm = T),3),
      paxtra = round(sum(apt_pax_tr, na.rm = T),3)) %>%
    arrange(desc(paxdep)) %>%
    ungroup()
  
  return(table2)
}
summary_stat_links <- function(data){
  agg_data <- data %>%
    group_by(lsn_fsc) %>%
    summarise(
      paxloc = round(sum(lsn_pax_loc, na.rm = TRUE)*1e-6,3)
    ) %>%
    ungroup()
  return(agg_data)
}
get_recent_date <- function(df, anmois) {
  # Trouve la date la plus récente
  x=max(df$anmois)
  recent_date=as.Date(paste0(x, "01"), format = "%Y%m%d")
  # Retourne la date en format "YYYYMM"
  return(recent_date)
}

#TABLE----
create_table_airports <- function(stats_airports){
  stats_airports_table <- stats_airports %>%
    mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")
    ) %>%
    select(name_clean, everything())
  table_airports <- gt(stats_airports_table)
  table_airports <- table_airports %>%
    cols_hide(columns = starts_with("apt"))
  table_airports <- table_airports %>%
    fmt_number(columns = starts_with("pax"), suffixing = TRUE)
  table_airports <- table_airports %>%
    fmt_markdown(columns = "name_clean")
  table_airports <- table_airports %>%
    cols_label(
      name_clean = md("**Airport**"),
      paxdep = md("**Depart**"),
      paxarr = md("**Arriv**"),
      paxtra = md("**Transit**")
    ) %>%
    tab_header(
      title = md("**Airports** from most to least frequented")
    ) %>%
    tab_style(
      style = cell_fill(color = "powderblue"),
      locations = cells_title()
    ) %>%
    tab_source_note(source_note = md("_Source: DGAC, data available on data.gouv.fr_"))
  table_airports <- table_airports %>%
    opt_interactive()
  return(table_airports)
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
    summarise(traffic = sum(traffic)) %>%
    ungroup() %>% 
    mutate(volume = case_when(
      (traffic > 999999)~3,
      (traffic > 99999)~2,
      (traffic < 100000)~1
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
      label=~paste0(Nom, "", " (",Code.OACI, ") : ", traffic, " voyageurs")
    )
  return(carte_interactive)
}

#PLOT AIRPORT LINE----
plot_airport_line <- function(df, selected_airport){
  traffic_airports <- df %>%
    mutate(traffic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
    filter(apt %in% selected_airport) %>%
    mutate(
      date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
    )
  figure_plotly <- traffic_airports %>%
    plot_ly(
      x = ~date, y = ~traffic,
      text = ~apt_nom,
      hovertemplate = paste("<i>Airport:</i> %{text}<br>Traffic: %{y}") ,
      type = 'scatter', mode = 'lines+markers')
  return(figure_plotly)
}
