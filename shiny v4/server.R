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
  output$info_table <- renderTable({
    hc_pre_covid %>%
      dplyr::select(`Bias Motive Description`, `Full Complaint ID`) %>%
      group_by(`Bias Motive Description`) %>%
      summarise(cases = length(`Full Complaint ID`))
    kable(tab1, caption = "Hate crime cases before the Covid-19")
    
    hc_since_covid %>%
      dplyr::select(`Bias Motive Description`, `Full Complaint ID`) %>%
      group_by(`Bias Motive Description`) %>%
      summarise(cases = length(`Full Complaint ID`))
    kable(tab2, caption = "Hate crime cases during the Covid-19")}
  )
    
    

}