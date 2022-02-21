server <- function(input, output) {
  
  # Reactive for produce line chart data for selected county
  line_data <- reactive({
    get_line_data(input$selected_dateS, input$selected_dateE,
                  input$selected_county, input$selected_motive)
  })
  
  
  # Render the line chart
  output$line_plot <- renderPlot({
    plot_line(line_data(), input$selected_topic)
  })
  
  # Render the table
  output$info_table
    
    

}