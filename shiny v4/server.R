server <- function(input, output) {
  
  # Render the line chart
  output$line_plot <- renderPlot({
    plot_line(input$selected_dateS, input$selected_dateE,
              input$selected_county, input$selected_motive, input$selected_topic)
  })
  
  # Render the table
  output$info_table <- renderTable({
    tab1 <- hc_pre_covid %>%
      dplyr::select(`Bias Motive Description`, `Full Complaint ID`) %>%
      group_by(`Bias Motive Description`) %>%
      summarise(cases_before_covid = length(`Full Complaint ID`))
    tab2 <- hc_since_covid %>%
      dplyr::select(`Bias Motive Description`, `Full Complaint ID`) %>%
      group_by(`Bias Motive Description`) %>%
      summarise(cases_during_covid = length(`Full Complaint ID`))
    print(left_join(tab1, tab2))
    }
  )

    
    

}
