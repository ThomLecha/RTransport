main_color <- "black"
input_date <- shinyWidgets::airDatepickerInput(
  "date",
  label = "Select one or several months",
  value = date_max,
  multiple = TRUE,
  view = "months",
  minView = "months",
  minDate = "2010-01-01",
  maxDate = date_max,#see in main script in global.R
  dateFormat = "MMMM yyyy",
  language = "en"
)
input_airport <- selectInput(
  "select",
  NULL,#means no label, otherwise just write "months selected" instead of NULL
  choices = list_airports,
  selected = default_airport
)

ui <- page_navbar(
  title = "Air traffic in France",
  bg = main_color,
  inverse = TRUE,
  header = em("App realized for the Funathon 2024 by Insee and DGAC"),
  layout_columns(
    card(
      HTML(
        '<a href="https://inseefrlab.github.io/funathon2024_sujet2/">👉️Have fun by making this app yourself</a>'
      ),
      input_date,
      gt_output(outputId = "table")
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
)