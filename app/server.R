server <- function(input, output) {

    
    motive <- reactive({
        if ( "ANTI-JEWISH" %in% input$motive){
            return( 
                subset(hc_b, `Bias Motive Description` == "ANTI-JEWISH")
                )
        }
        if ( "ANTI-ASIAN" %in% input$motive){
            return( 
                subset(hc_b, `Bias Motive Description` == "ANTI-ASIAN")
            )
        }
        if ( "ANTI-MALE HOMOSEXUAL (GAY)" %in% input$motive){
            return( 
                subset(hc_b, `Bias Motive Description` == "ANTI-MALE HOMOSEXUAL (GAY)")
            )
        }
        if ( "ANTI-BLACK" %in% input$motive){
            return( 
                subset(hc_b, `Bias Motive Description` == "ANTI-BLACK")
            )
        }
        if ( "ANTI-WHITE" %in% input$motive){
            return( 
                subset(hc_b, `Bias Motive Description` == "ANTI-WHITE")
            )
        }
        if ( "ANTI-TRANSGENDER" %in% input$motive){
            return( 
                subset(hc_b, `Bias Motive Description` == "ANTI-TRANSGENDER")
            )
        }
        if ( "ANTI-MUSLIM" %in% input$motive){
            return( 
                subset(hc_b, `Bias Motive Description` == "ANTI-MUSLIM")
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
        
        linechart(data1, data2)

    })
    
    output$my_tmap = renderTmap({
      data3 = hc_wave()
      tmap_mode("view")
      
      tm_shape(data3) + tm_polygons("count", legend.title = "Hate Crime Count")
    })
}