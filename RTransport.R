##################################
###   R TRANSPORT - MAIN       ###
##################################

# rm(list = ls())
if (!exists("pax_apt_all")){ # Avoid loading libraries, functions and data if already loaded
  library(arrow) # read and write parquet
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(leaflet) # realize maps
  library(minpack.lm)  # ajustement non-linéaire
  library(lubridate) # handle dates
  library(plotly) # creates interactives graphs
  library(readr)
  library(reshape2)# transform data in long format to draw variables on the same graph
  library(sf)
  library(shiny)
  library(shinydashboard)
  library(stringr) # bring useful string functions
  source("./load_functions.R")
  source("./load_data.R")
}

# UI User Interface ----
# UI Inputs ----
main_color = "black"

input_submit = actionButton(inputId = "submit", label = "LOAD & SAVE")

input_date_apt = input_date("date_apt", "Période d'observation", date_max) 
input_date_cie = input_date("date_cie", "Période d'observation", date_max)
input_date_route_apt = input_date("date_route_apt", "Période d'observation", date_max2)
input_date_route_cou = input_date("date_route_cou", "Période d'observation", date_max2)

input_airport = selectInput("select_apt",NULL,choices = list_airports,selected = "LFPG",multiple = TRUE)
input_airport_start = selectInput("select_apt_start",NULL,choices = list_airports,selected = "LFPG",multiple = TRUE)
input_airport_end = selectInput("select_apt_end",NULL,choices = list_airports_end,selected = "KJFK",multiple = TRUE)
input_airport_connect = selectInput("select_apt_connect",NULL,choices = list_airports,selected = "LFOB",multiple = TRUE)
input_cie = selectInput("select_cie",NULL,choices = list_cie,selected = c("RYANAIR"),multiple = TRUE)
input_cou = selectInput("select_cou",NULL,choices = list_cou,selected = "CHINA",multiple = TRUE)
input_ihh_flows = selectInput("select_ihh_flows",label = "Faisceaux géographiques",choices = list_ihh_flows,selected = c("Métro"),multiple = TRUE)
input_price_flows = selectInput("select_price_flows",label = "Faisceaux géographiques",choices = list_price_flows,selected = c("met_inter_met"),multiple = TRUE)
input_traffic_flows = selectInput("select_traffic_flows",label = "Faisceaux géographiques",choices = list_traffic_flows,selected = c("Amérique du Nord", "Métro"),multiple = TRUE)

# UI Define ----
ui <- dashboardPage(dashboardHeader(title = "R Transport"),
  
  # SIDEBAR MENU
  if (user_app) { dashboardSidebar(sidebarMenu(
    menuItem("Trafic par aéroport", tabName = "apt", icon = icon("passport")),
    menuItem("Trafic par compagnie", tabName = "cie", icon = icon("plane")),
    menuItem("Emissions de CO2", tabName = "co2", icon = icon("temperature-high")),
    menuItem("Indice des prix", tabName = "iptap", icon = icon("euro-sign")),
    menuItem("Concentration IHH", tabName = "ihh", icon = icon("key")),
    menuItem("Faisceaux", tabName = "traffic", icon = icon("key")),
    menuItem("Pays", tabName = "route_cou", icon = icon("key")),
    menuItem("Routes", tabName = "route_apt", icon = icon("key")),
    menuItem("Connectivité", tabName = "connect", icon = icon("key")),
    menuItem("SEARCH", tabName = "search", icon = icon("hand-sparkles")),
    menuItem("SAVE DATA", tabName = "save_data", icon = icon("hand-sparkles"))
    ))
  } else { dashboardSidebar(sidebarMenu(
    menuItem("Trafic des aéroports français", tabName = "apt", icon = icon("passport")),
    menuItem("Trafic des compagnies", tabName = "cie", icon = icon("plane")),
    menuItem("Emissions de CO2", tabName = "co2", icon = icon("temperature-high")),
    menuItem("Prix", tabName = "iptap", icon = icon("euro-sign"))
    ))}
  ,
  
  # MAIN BODY CONTENT
  dashboardBody(tabItems( 
    tabItem(tabName = "apt", # Airport traffic
            h2("Tableau de bord du trafic aérien réalisé lors du Funathon 2024 Insee & DGAC"),
            bg = main_color,
            inverse = TRUE,
            layout_columns(card(
              HTML('<a href="https://inseefrlab.github.io/funathon2024_sujet2/">👉️ refaire ce type de tableau en R ou Python</a>'),
              HTML('<a href="https://www.data.gouv.fr/fr/datasets/trafic-aerien-commercial-mensuel-francais-par-paire-daeroports-par-sens-depuis-1990/">👉️ séries accessibles sur data.gouv.fr</a>'),
              input_date_apt,
              DT::DTOutput("table_traffic_apt")),
              layout_columns(card(leafletOutput("carte")),
                             card(card_header("Aéroports", class = "bg-dark"),
                                  input_airport,
                                  plotlyOutput("plot_apt")),
                             col_widths = c(12,12)),
              cols_widths = c(12,12,12))
            ),
      
      tabItem(tabName = "cie",      # Company traffic
              h2("Trafic aérien des compagnies, en millions de passagers, source DGAC"),
              HTML('<a href="https://www.data.gouv.fr/fr/datasets/trafic-aerien-commercial-mensuel-francais-par-paire-daeroports-par-sens-depuis-1990/">👉️ séries accessibles sur data.gouv.fr</a>'),
              input_cie,
              plotlyOutput("plot_cie"),
              input_date_cie,
              DT::DTOutput("table_traffic_cie")
      ),
      
      tabItem(tabName = "co2", # CO2 emissions
              h2("Emissions de CO2 du trafic aérien, en million de tonnes, source DGAC"),
              HTML('<a href="https://www.ecologie.gouv.fr/politiques-publiques/emissions-gazeuses-liees-trafic-aerien">👉️ bilan des émissions gazeuses du transport aérien</a>')
      ),
      
      tabItem(tabName = "iptap", # Price index
              h2("Indice des prix du transport aérien - IPTAP, source DGAC"),
              HTML('<a href="https://www.data.gouv.fr/fr/datasets/indices-des-prix-du-transport-aerien-de-passagers/">👉 séries accessibles sur data.gouv.fr</a>'),
              input_price_flows,
              plotlyOutput("plot_price")
      ),
      
      tabItem(tabName = "connect", # Connectivity
              h2("Connectivité directe, source DGAC"),
              input_airport_connect,
              plotlyOutput("plot_connect")
      ),

      tabItem(tabName = "ihh", # Concentration index
              h2("Indice de Herfindahl-Hirschman - IHH, source DGAC"),
              HTML('<a href="https://www.ecologie.gouv.fr/politiques-publiques/observatoire-concurrence">👉 observatoire de la concurrence</a>'),
              input_ihh_flows,
              plotlyOutput("plot_ihh")
      ),
      
      tabItem(tabName = "traffic", # Traffic flows
              input_traffic_flows,
              plotlyOutput("plot_traffic"),
              verbatimTextOutput(outputId = "texte"),
              checkboxGroupInput("mon", "Mois:",
                                 month_char,
                                 inline = T),
              radioButtons("yea", "Année:",
                           year_char,
                           inline = T),
              DT::dataTableOutput("table_traffic")
      ),
      
      tabItem(tabName = "route_apt", # Route selection and traffic between 2 airports
              input_airport_start,
              input_airport_end,
              plotlyOutput("plot_route"),
              input_date_route_apt,
              DT::dataTableOutput("table_route_apt")
      ),

      tabItem(tabName = "route_cou", # Route selection and traffic with a foreign country
              input_cou,
              plotlyOutput("plot_cou"),
              input_date_route_cou,
              DT::dataTableOutput("table_route_cou")
      ),
      
      tabItem(tabName = "search", # Search airport or airline
              textInput(inputId = "airport_lib",
                        label = "Airport",
                        value = "orly"),
              DT::dataTableOutput("table_search_apt"),
              textInput(inputId = "company_lib",
                        label = "Company",
                        value = "tvf"),
              DT::dataTableOutput("table_search_cie")
              ),
      
      tabItem(tabName = "save_data", # SAVE DATA
              input_submit,
              #textOutput("year_output"),  # Pour afficher la valeur de y
              textOutput("result_submit")  # Pour afficher le message "data loaded"
              )
    )
    )
  )

# SERVER ----
server <- function(input, output, session) {
  # SUBMIT ----
  observeEvent(input$submit, {# Action lors du clic sur le bouton "submit"
    showModal(modalDialog(# Afficher une boîte de dialogue de confirmation
      title = "ATTENTION OPERATION LONGUE",
      "Confirmez-vous ?",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm", "Confirmer")))
      )
    })
  
  observeEvent(input$confirm, {  # Action lors de la confirmation
    removeModal()    # Fermer la boîte de dialogue
    t1 <- Sys.time()    # Calcul après confirmation
    filepath=paste0(userdir,"save_data.R")
    source(filepath)
    output$result_submit <- renderText({paste0("Data loaded and saved in ", Sys.time() - t1)})
  })
  
  # MAPS & PLOTS ----
  output$carte = renderLeaflet(map_leaflet_airport(month(input$date_apt), year(input$date_apt)))
  output$plot_apt = renderPlotly(plot_traffic_selection(pax_apt_all %>% mutate(selected_var=apt,pax = apt_pax),input$select_apt))
  output$plot_cie = renderPlotly(plot_traffic_selection(pax_cie_all %>% mutate(selected_var=cie_nom,pax = cie_pax),input$select_cie))
  output$plot_cou = renderPlotly(plot_traffic_selection(traffic_cou,input$select_cou))
  output$plot_connect = renderPlotly(plot_traffic_selection(traffic_connect,input$select_apt_connect))
  output$plot_ihh = renderPlotly(plot_traffic_selection(traffic_ihh,input$select_ihh_flows))
  output$plot_price = renderPlotly(plot_traffic_selection(iptap,input$select_price_flows))
  output$plot_route = renderPlotly(plot_traffic_selection(traffic_route,paste0(input$select_apt_start,input$select_apt_end)))
  output$plot_traffic = renderPlotly(plot_traffic_selection(traffic_flows, input$select_traffic_flows))
  
  # REACTIVES ----
  dfrouteapt = reactive({
    df = pax_apt %>%
      filter(fluxindic==1) %>% 
      filter(apt1 %in% input$select_apt_start) %>% 
      filter(apt2 %in% input$select_apt_end) %>%
      mutate(mois=sub("^0", "", mois)) %>% #enlève le "0" lorsqu'il apparaît, sinon la condition suivante ne fonctionne pas
      filter(mois %in% month(input$date_route_apt),an %in% year(input$date_route_apt)) %>% 
      filter(!is.na(cielabel)) %>%
      group_by(cielabel,ciepayinfo) %>%
      summarise(mvt = sum(mvt, na.rm=T), pax = round(sum(pax, na.rm = T)), sieges = sum(sieges, na.rm = T)) %>%
      arrange(desc(pax)) %>%
      ungroup()})
  dfroutecou = reactive({
    df = pax_apt %>%
      filter(fluxindic==1) %>% 
      filter(countrynameoaciapt2 %in% input$select_cou) %>% 
      #filter(countrynameoaciapt2 == "UNITED STATES OF AMERICA") %>% 
      mutate(mois=sub("^0", "", mois)) %>% #enlève le "0" lorsqu'il apparaît, sinon la condition suivante ne fonctionne pas
      filter(mois %in% month(input$date_route_cou),an %in% year(input$date_route_cou)) %>% 
      filter(!is.na(cielabel)) %>%
      group_by(cielabel,ciepayinfo) %>%
      summarise(mvt = sum(mvt, na.rm=T), pax = round(sum(pax, na.rm = T)), sieges = sum(sieges, na.rm = T)) %>%
      arrange(desc(pax)) %>%
      ungroup()})
  dfsearchapt = reactive({return(apt %>% filter(str_detect(simplify_text(apt$label), input$airport_lib)|str_detect(simplify_text(apt$aptname), input$airport_lib)|str_detect(simplify_text(apt$aptoaci), input$airport_lib)|str_detect(simplify_text(apt$aptiata), input$airport_lib)|str_detect(simplify_text(apt$apays), input$airport_lib)|str_detect(simplify_text(apt$countrynameoaci), input$airport_lib)))})
  dfsearchcie = reactive({return(cie %>% filter(str_detect(simplify_text(cie$ciecodeoaci), input$company_lib)|str_detect(simplify_text(cie$cieiata), input$company_lib)|str_detect(simplify_text(cie$cielabel), input$company_lib)|str_detect(simplify_text(cie$ciepayinfo), input$company_lib)))})
  dftraffic = reactive({return(pax_apt %>% filter((mois %in% input$mon)&(an == input$yea)))})
  dftrafficapt = reactive({
    pax_apt_all %>% 
      filter(mois %in% month(input$date_apt), an %in% year(input$date_apt)) %>% 
      group_by(apt, apt_nom) %>%
      summarise(
        pax = round(sum(apt_pax, na.rm = T)/1000000,3),
        depart = round(sum(apt_pax_dep, na.rm = T)/1000000,3),
        arriv = round(sum(apt_pax_arr, na.rm = T)/1000000,3),
        transit = round(sum(apt_pax_tr, na.rm = T)/1000000,3)) %>%
      arrange(desc(pax)) %>%
      ungroup()})
  dftrafficcie = reactive({
    pax_cie_all %>% 
      filter(mois %in% month(input$date_cie), an %in% year(input$date_cie)) %>% 
      group_by(cie, cie_nom) %>%
      summarise(pax = round(sum(cie_pax, na.rm = T)/1000000,3)) %>%
      arrange(desc(pax)) %>%
      ungroup()})

  # RENDER TABLES ----
  output$table_search_apt <- DT::renderDataTable({DT::datatable(dfsearchapt())})
  output$table_search_cie <- DT::renderDataTable({DT::datatable(dfsearchcie())})
  output$table_traffic_apt <- DT::renderDataTable({DT::datatable(dftrafficapt())})
  output$table_traffic_cie <- DT::renderDataTable({DT::datatable(dftrafficcie())})
  output$table_route_apt <- DT::renderDataTable({DT::datatable(dfrouteapt())})
  output$table_route_cou <- DT::renderDataTable({DT::datatable(dfroutecou())})
  output$table_traffic <- DT::renderDataTable(DT::datatable({
    data = bind_rows(
      dftraffic() %>%
        filter(fluxindic==1) %>%
        summarise(pax = round(sum(pax, na.rm = T)/1000000,3)),
      dftraffic() %>%
        filter(fluxindic==1) %>%
        group_by(re) %>%
        summarise(pax = round(sum(pax, na.rm = T)/1000000,3)) %>%
        ungroup,
      dftraffic() %>%
        filter(fluxindic==1) %>%
        group_by(faisceau) %>%
        summarise(pax = round(sum(pax, na.rm = T)/1000000,3)) %>%
        ungroup,
      dftraffic() %>%
        filter(fluxindic==1) %>%
        group_by(routes_det) %>%
        summarise(pax = round(sum(pax, na.rm = T)/1000000,3)) %>%
        ungroup,
      dftraffic() %>%
        filter(fluxindic==1) %>%
        group_by(cietype) %>%
        summarise(pax = round(sum(pax, na.rm = T)/1000000,3)) %>%
        ungroup,
      dftraffic() %>%
        filter((fluxindic==1)) %>%
        group_by(countrynameoaciapt1, countrynameoaciapt2) %>%
        summarise(pax = round(sum(pax, na.rm = T)/1000000,3)) %>%
        ungroup
    )
  }, class = "cell-border compact hover stripe",
  extensions = c("Scroller", "FixedColumns", "Buttons", "Select"), 
  selection = "none",
  options = list(
    autoWidth = T,
    columnDefs = list(list(width = '20px', targets = c(0,1,2))),
    dom = "Bfrtip", 
    scrollY = 285, scrollX = 400, scroller = TRUE,
    fixedColumns = list(leftColumns = 1),
    select = list(style = 'os', items = 'row'),
    buttons = c(# enregistrements
      'copy', 'csv',# visualisation des colonnes
      'colvis',# sélection des elements
      'selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells'
    )
  )))
}

# Launch Shiny App ----
shinyApp(ui = ui, server = server)