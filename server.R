function(input, output) {
  table_links = reactive({
    return(
      summary_stat_airport(
        create_data_from_input(
          pax_apt_all,
          year(input$date),
          month(input$date)
        )
      )
    )
  })
  output$table <- render_gt(
    create_table_airports(table_links())
  )
  output$carte <- renderLeaflet(
    map_leaflet_airport(
      pax_apt_all, airports_location,
      month(input$date), year(input$date)
    )
  )
  output$lineplot <- renderPlotly(
    plot_airport_line(pax_apt_all, input$select)
  )
}