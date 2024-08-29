##################################
###         R TRANSPORT        ###
##################################

# rm(list = ls())
if (!exists("pax_apt_all")){ # Avoid loading libraries, functions and data if already loaded
  # Libraries ----
  library(arrow)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(lubridate)
  library(plotly)
  library(readr)
  library(sf)
  library(shiny)
  library(shinydashboard)
  library(stringr)
  # Load functions & data, including global variables ----
  source("./load_functions.R")
  source("./load_data.R")
}

# UI User Interface ----

# User Interface Inputs
main_color <- "black"
input_date <- shinyWidgets::airDatepickerInput(
  "date",
  label = "Select one or several months",
  value = date_max,
  multiple = TRUE,
  view = "months",
  minView = "months",
  minDate = "2010-01-01",
  maxDate = date_max,
  dateFormat = "MMMM yyyy",
  language = "en"
)

input_airport <- selectInput(
  "select",
  NULL,#means no label, otherwise just write "months selected" instead of NULL
  choices = list_airports,
  selected = default_airport
)

# User Interface define
ui <- dashboardPage(
  dashboardHeader(title = "R Transport"
  ),
  
  # Sidebar Menu
  if (user_app) {
    dashboardSidebar(
      sidebarMenu(
        menuItem("Airport traffic", tabName = "apt", icon = icon("passport")),
        menuItem("Company traffic", tabName = "cie", icon = icon("plane")),
        menuItem("CO2 emissions", tabName = "co2", icon = icon("temperature-high")),
        menuItem("Price index", tabName = "iptap", icon = icon("euro-sign")),
        menuItem("Detailed traffic", tabName = "traffic", icon = icon("user")),
        menuItem("Search apt or cie", tabName = "search", icon = icon("user"))
        )
      )
  } else {
    dashboardSidebar(
      sidebarMenu(
        menuItem("Airport traffic", tabName = "apt", icon = icon("passport")),
        menuItem("Company traffic", tabName = "cie", icon = icon("plane")),
        menuItem("CO2 emissions", tabName = "co2", icon = icon("temperature-high")),
        menuItem("Price index", tabName = "iptap", icon = icon("euro-sign"))
        )
    )
  }
  ,
  
  # Main Body Content
  dashboardBody(
    tabItems(
      # Airport page
      tabItem(tabName = "apt",
              h2("Realized for the Funathon 2024, Insee & DGAC"),
              bg = main_color,
              inverse = TRUE,
              layout_columns(
                card(
                  HTML(
                    '<a href="https://inseefrlab.github.io/funathon2024_sujet2/">👉️Have fun by making this app yourself</a>'
                  ),
                  input_date,
                  DT::DTOutput("table_traffic_apt")
                ),
                layout_columns(
                  card(leafletOutput("carte")),
                  card(card_header("Airport selection", class = "bg-dark"),
                       input_airport,
                       plotlyOutput("lineplot")
                  ),
                  col_widths = c(12,12)
                ),
                cols_widths = c(12,12,12)
              )
      ),
      
      # Company Page
      tabItem(tabName = "cie",
              h2("Données"),
              p("Cette section présente les données disponibles.")
      ),
      
      # CO2 emissions
      tabItem(tabName = "co2",
              h2("Données"),
              p("Cette section présente les données disponibles.")
      ),
      
      # Price index
      tabItem(tabName = "iptap",
              h2("Données"),
              p("Cette section présente les données disponibles.")
      ),
      
      # Detailed traffic
      tabItem(tabName = "traffic",
              verbatimTextOutput(outputId = "texte"),
              checkboxGroupInput("mon", "Mois:",
                                 month_char,
                                 inline = T),
              radioButtons("yea", "Année:",
                           year_char,
                           inline = T),
              DT::dataTableOutput("table_traffic")
      ),
      
      # Search airport or airline
      tabItem(tabName = "search",
              textInput(
                inputId = "airport_lib",
                label = "Airport",
                value = ""
              ),
              DT::dataTableOutput("table_search_apt"),
              textInput(
                inputId = "company_lib",
                label = "Company",
                value = ""
              ),
              DT::dataTableOutput("table_search_cie")
              )
      )
    )
  )

# SERVER ----
server <- function(input, output, session) {
  output$carte <- renderLeaflet(
    map_leaflet_airport(
      pax_apt_all, airports_location,
      month(input$date), year(input$date)
    )
  )
  output$lineplot <- renderPlotly(
    plot_airport_line(pax_apt_all, input$select)
  )
  # REACTIVE
  dfsearchapt = reactive({
    return(apt %>% filter(str_detect(simplify_text(apt$label), input$airport_lib)|str_detect(simplify_text(apt$aptname), input$airport_lib)|str_detect(simplify_text(apt$aptoaci), input$airport_lib)|str_detect(simplify_text(apt$aptiata), input$airport_lib)|str_detect(simplify_text(apt$apays), input$airport_lib)|str_detect(simplify_text(apt$countrynameoaci), input$airport_lib)))
  })
  dfsearchcie = reactive({
    return(cie %>% filter(str_detect(simplify_text(cie$ciecodeoaci), input$company_lib)|str_detect(simplify_text(cie$cieiata), input$company_lib)|str_detect(simplify_text(cie$cielabel), input$company_lib)|str_detect(simplify_text(cie$ciepayinfo), input$company_lib)))
  })
  dftraffic = reactive({
    return(pax_apt %>% filter((mois %in% input$mon)&(an == input$yea)))
  })
  dftrafficapt = reactive({
    pax_apt_all %>% 
      filter(mois %in% month(input$date), an %in% year(input$date)) %>% 
      group_by(apt, apt_nom) %>%
      summarise(
        pax = round(sum(apt_pax_dep+apt_pax_arr+apt_pax_tr, na.rm = T)/1000000,3),
        depart = round(sum(apt_pax_dep, na.rm = T)/1000000,3),
        arriv = round(sum(apt_pax_arr, na.rm = T)/1000000,3),
        transit = round(sum(apt_pax_tr, na.rm = T)/1000000,3)) %>%
      arrange(desc(pax)) %>%
      ungroup()
  })
  output$table_search_apt <- DT::renderDataTable({
    DT::datatable(dfsearchapt())
  })
  output$table_search_cie <- DT::renderDataTable({
    DT::datatable(dfsearchcie())
  })
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
        #group_by(faisceau, e3fscapt2) %>%
        #group_by(bregionsueapt1, bregionsueapt2) %>%
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
    #pageLength = 3,
    #lengthMenu = c(3, 14, 22),
    autoWidth = T,
    columnDefs = list(list(width = '20px', targets = c(0,1,2))),
    dom = "Bfrtip", 
    # scroll :
    scrollY = 285, scrollX = 400, scroller = TRUE,
    #scrollY = 145, scrollX = 400, scroller = TRUE,
    # fixer les colonnes : 
    fixedColumns = list(leftColumns = 1),
    # selection :
    select = list(style = 'os', items = 'row'),
    buttons = c(
      # enregistrements
      'copy', 'csv',
      # visualisation des colonnes
      'colvis',
      # selection des elements
      'selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells'
    )
  )))
  output$table_traffic_apt <- DT::renderDataTable({
    DT::datatable(dftrafficapt())
  })
}

# Launch Shiny App ----
shinyApp(ui = ui, server = server)