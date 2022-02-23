
server <- function(input, output, session) {
  
  
  motive <- reactive({
    if ( "ANTI-JEWISH" %in% input$motive){
      return( 
        "ANTI-JEWISH"
      )
    }
    if ( "ANTI-ASIAN" %in% input$motive){
      return( 
        "ANTI-ASIAN"
      )
    }
    if ( "ANTI-MALE HOMOSEXUAL (GAY)" %in% input$motive){
      return( 
        "ANTI-MALE HOMOSEXUAL (GAY)"
      )
    }
    if ( "ANTI-BLACK" %in% input$motive){
      return( 
        "ANTI-BLACK"
      )
    }
    if ( "ANTI-WHITE" %in% input$motive){
      return( 
        "ANTI-WHITE"
      )
    }
    if ( "ANTI-TRANSGENDER" %in% input$motive){
      return( 
        "ANTI-TRANSGENDER"
      )
    }
    if ( "ANTI-MUSLIM" %in% input$motive){
      return( 
        "ANTI-MUSLIM"
      )
    }
  })  
  
  hc_wave <- reactive({
    if ( "Pre First Wave" %in% input$Period){
      return( 
        hc_pre_covid_gis
      )
    }
    if ( "Post First Wave" %in% input$Period){
      return( 
        hc_since_covid_gis
      )
    }
    if ( "All Time" %in% input$Period){
      return( 
        hc_gis
      )
    }
  })
  
  
  output$tsPlot0 <- renderPlot({
    data1 = covid_19
    data2 = motive()
    
    combine1(COVID_Whole_Cases,data2)
    
  })
  
  output$tsPlot1 <- renderPlot({
    data1 = covid_19
    data2 = domestic_V
    
    combine2(COVID_Whole_Cases,data2)
    
  })
  
  output$my_tmap = renderTmap({
    data3 = hc_wave()
    tmap_mode("view")
    
    tm_shape(data3) + tm_polygons("count", legend.title = "Hate Crime Count")
  })
  
  
  vic_sub_type <- reactive({
    if ( "M" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_M
      )
    }
    if ( "F" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_F
      )
    }
    if ( "E" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_E
      )
    }
    if ( "D" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_D
      )
    }
    if ( "<18" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_18
      )
    }
    if ( "18-24" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_24
      )
    }
    if ( "25-44" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_44
      )
    }
    if ( "45-64" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_64
      )
    }
    if ( "65+" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_65
      )
    }
    if ( "AMERICAN INDIAN/ALASKAN NATIVE" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_native
      )
    }
    if ( "ASIAN / PACIFIC ISLANDER" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_asian
      )
    }
    if ( "WHITE" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_white
      )
    }
    if ( "BLACK HISPANIC" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_black_his
      )
    }
    if ( "WHITE HISPANIC" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_white_his
      )
    }
    
    if ( "BLACK" %in% input$selected_vic_subtypes){
      return( 
        crime_vic_black
      )
    }
  
  })

  observe({
    updateSelectInput(session,
                      "selected_vic_type",
                      choices = c('VIC_SEX', 'VIC_AGE_GROUP','VIC_RACE'))
  })
  
  
  observe({
    updateSelectInput(
      session,
      "selected_vic_subtypes",
      choices = selector(input$selected_vic_type)
    )
    
    
  })
  
  output$tsPlot2 <- renderPlot({
    data1 = covid_19
    data2 = vic_sub_type()
    
    combine3(COVID_Whole_Cases,data2)
    
  })
}
