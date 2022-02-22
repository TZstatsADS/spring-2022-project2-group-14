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
                                h4("By Joel Mugyenyi, Rishav Agarwal, Shanyue Zeng, Lichun He, Ananya Iyer"),
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
                              dateInput("selected_dateS", "Time period: Start", Sys.Date()-960),
                              dateInput("selected_dateE", "Time period: End", Sys.Date()),
                              hr(), 
                              
                              radioButtons("selected_county", "County:", 
                                           c("All" = "All", "BRONX" = "BRONX", 'BROOKLYN'= 'BROOKLYN', 
                                             'MANHATTAN'= 'MANHATTAN', 'QUEENS'= 'QUEENS', 'STATEN ISLAND' = 'STATEN ISLAND')),
                              hr(),
                              
                              radioButtons("selected_topic", "Topic:", 
                                           c("Both" = "Both", "Hate Crime" = "Hate Crime", "Covid-19" = "Covid-19")),
                              hr(), 
                              
                              radioButtons("selected_motive", "Hate Crime Motive:",
                                           c("All" = "All", "ANTI-JEWISH" = "ANTI-JEWISH", "ANTI-ASIAN" = "ANTI-ASIAN",
                                             "ANTI-BLACK" = "ANTI-BLACK", "ANTI-MALE HOMOSEXUAL (GAY)" = "ANTI-MALE HOMOSEXUAL (GAY)",
                                             "ANTI-WHITE" = "ANTI-WHITE" , "ANTI-MUSLIM" = "ANTI-MUSLIM",
                                             "ANTI-TRANSGENDER" = "ANTI-TRANSGENDER")),
                              hr()
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: tsPlot ----
                              #plotOutput(outputId = "tsPlot0"),
                              plotOutput("line_plot"),
                              tableOutput("plot_line_table")
                              
                            )
                          )
                        )
                        
                        )
                        
                       
                            
                          )
                        )
                        )
                        
                      
                    
