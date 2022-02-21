library(shinydashboard)

ui <- dashboardPage(skin = "purple",
                    title="Covid_19 and Crime in New York City",
                    dashboardHeader(title=span("Covid_19 and Crime in NYC",style="font-size: 16px")),
                    
                    dashboardSidebar(   sidebarMenu(
                      menuItem("Home", tabName = "home", icon = icon("home")),
                      menuItem("Covid-19 Cases & Hate Crimes", tabName = "c_19_hc"),
                      menuItem("Map", tabName = "map")
                      
                    )),
                    
                    
                    ## Body content
                    dashboardBody(
                      tabItems(
                        
                        # Home tab content
                        tabItem(tabName = "home",
                                h2("How Covid_19 impacted crime in New York City"),
                                h4("By Joel Mugyenyi, Rishav Agarwal, Shanyue Zeng, Lichun He"),
                        ),
                        
                        #------------------Covid & Crime----------------------------
                        tabItem(tabName = "c_19_hc", fluidPage(
                          
                          # App title ----
                          titlePanel("Covid-19 & Hate Crime"),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Select motive ----
                              dateInput("selected_dateS", "Time period: Start", Sys.Date()-20, min= 2019-02-01, max = 2021-12-31),
                              dateInput("selected_dateE", "Time period: End", Sys.Date(), min= 2019-02-01, max = 2021-12-31),
                              hr(), 
                              
                              radioButtons("selected_county", "County:", 
                                           c("All" = TRUE, "BRONX" = FALSE, 'BROOKLYN'= FALSE, 
                                             'MANHATTAN'= FALSE, 'QUEENS'= FALSE, 'STATEN ISLAND' = FALSE)),
                             
                              
                              radioButtons("selected_topic", "Topic:", 
                                           c("Both" = TRUE, "Hate Crime" = FALSE, "Covid-19" = FALSE)),
                              hr(), 
                              
                              radioButtons("selected_motive", "Hate Crime Motive:", 
                                           c("All" = TRUE, "ANTI-JEWISH" = FALSE, "ANTI-ASIAN" = FALSE, "ANTI-BLACK" = FALSE, 
                                             "ANTI-MALE HOMOSEXUAL (GAY)" = FALSE, "ANTI-WHITE" = FALSE, "ANTI-MUSLIM" = FALSE,
                                             "ANTI-TRANSGENDER" = FALSE)),
                              hr()
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: tsPlot ----
                              #plotOutput(outputId = "tsPlot0"),
                              plotOutput("line_plot"),
                              tableOutput("info_table")
                              
                            )
                          )
                        )
                        
                        )
                        
                       
                            
                          )
                        )
                        )
                        
                      
                    
