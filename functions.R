#INIT DATA----
clean_dataframe <- function(df){
  # Create an et mois columns
  df <- df %>% 
    mutate(
      an = str_sub(ANMOIS,1,4),
      mois = str_sub(ANMOIS,5,6)
    ) %>%
    mutate(
      mois = str_remove(mois, "^0+")
    )
  # lower case for variable names
  colnames(df) <- tolower(colnames(df))
  return(df)
}
create_data_from_input <- function(data, years, months){
  data <- data %>%
    filter(mois %in% months, an %in% years)
  return(data)
}
create_data_list <- function(source_file){
  catalogue <- yaml::read_yaml(source_file)
  return(catalogue)
#' Creates a 2-levels list of urls, pointing to open source data
#' @param source_file yaml file containing data urls 
#' @return list (level 1 = concepts, level 2 = year).
#' @examples
#'  create_data_list("sources.yml")
}
import_airlines_data <- function(list_files){
  pax_cie_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      CIE = col_character(),
      CIE_NOM = col_character(),
      CIE_NAT = col_character(),
      CIE_PAYS = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  return(pax_cie_all)
}
import_airlinks_data <- function(list_files){
  pax_lsn_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      LSN = col_character(),
      LSN_DEP_NOM = col_character(),
      LSN_ARR_NOM = col_character(),
      LSN_SCT = col_character(),
      LSN_FSC = col_character(),
      .default = col_double()
    ) 
  ) %>% 
    clean_dataframe()
  return(pax_lsn_all)
}
import_airports_data <- function(list_files){
  pax_apt_all <- readr::read_csv2(
    list_files, 
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  return(pax_apt_all)
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

#MAP LEAFLET AIRPORT----
map_leaflet_airport <- function(df, airports_location, months, years){
  #palette <- c("green", "orange", "darkred")
  traffic_date <- df %>%
    mutate(
      date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
    ) %>%
    filter(mois %in% months, an %in% years)
  traffic_airports <- airports_location %>%
    inner_join(traffic_date, by = c("Code.OACI" = "apt"))
  #traffic_airports <- traffic_airports %>%
  #  mutate(
  #    volume = ntile(traffic, 3)
  #  ) %>%
  #  mutate(color = palette[volume])
  
  icons <- awesomeIcons(
    icon = 'plane',
    iconColor = 'black',
    library = 'fa'#,
    #markerColor = traffic_airports$color
  )
  
  carte_interactive <- leaflet(traffic_airports) %>% addTiles() %>%
    addAwesomeMarkers(
      icon=icons[],
      label=~paste0(Nom, "", " (",Code.OACI, ") ")
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

#TABLES----
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
