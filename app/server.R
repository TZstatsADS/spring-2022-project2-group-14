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
    output$tsPlot0 <- renderPlot({
        data1 = covid_19
        data2 = motive()
        
        linechart(data1, data2)

    })
}