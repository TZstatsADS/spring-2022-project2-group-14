server <- function(input, output) {
  options(shiny.maxRequestSize=60*1024^2)
  
  # Render the line chart
  output$line_plot <- renderPlot({
    plot_line(input$selected_dateS, input$selected_dateE,
              input$selected_county, input$selected_motive, input$selected_topic)
  })
  
  # Render the table
  
  output$plot_line_table <- renderTable({
    plot_line_table(input$selected_dateS, input$selected_dateE,
              input$selected_county, input$selected_motive, input$selected_topic)
  })

    
    

}