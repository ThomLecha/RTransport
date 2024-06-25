main_color <- "black"

input_date <- shinyWidgets::airDatepickerInput(
  "date",
  label = "Select a month",
  value = "2019-01-01",
  view = "months",
  minView = "months",
  minDate = "2018-01-01",
  maxDate = "2022-12-01",
  dateFormat = "MMMM yyyy",
  language = "fr"
)

input_airport <- selectInput(
  "select",
  "Aéroport choisi",
  choices = liste_aeroports,
  selected = default_airport
)

ui <- page_navbar(
  title = "Air traffic in France",
  bg = main_color,
  inverse = TRUE,
  header = em("Realized at the funathon 2024, organized by Insee and DGAC"),
  layout_columns(
    card(
      HTML(
        '<a href="https://inseefrlab.github.io/funathon2024_sujet2/">👉️ Retourner au tutoriel pour construire cette application</a>'
      ),
      input_date,
      gt_output(outputId = "table")
    ),
    layout_columns(
      card(leafletOutput("carte")),
      card(card_header("Traffic in an airport", class = "bg-dark"),
           input_airport,
           plotlyOutput("lineplot")
          ),
      col_widths = c(12,12)
    ),
    cols_widths = c(12,12,12)
  )
  
)

