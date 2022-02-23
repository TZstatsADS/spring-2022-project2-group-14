if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}


ui <- dashboardPage(skin = "purple",
                    title="Covid_19 and Crime in New York City",
                    dashboardHeader(title=span("Covid_19 and Crime in NYC",style="font-size: 16px")),
                    
                    dashboardSidebar(   sidebarMenu(
                      menuItem("Home", tabName = "home", icon = icon("home")),
                      menuItem("Covid-19 Cases & Hate Crimes", tabName = "c_19_hc"),
                      menuItem("Covid-19 & Hate Crimes (Time Period)", tabName = "c_19_hc_line"),
                      menuItem("Covid-19 Cases & Domestic Violence", tabName = "c_19_dv"),
                      menuItem("Covid-19 Cases & Crime Victims", tabName = "c_19_crime_vics"),
                      menuItem("Hate Crimes based on Borough", tabName = "map"),
                      menuItem("Complaints in Manhattan", tabName = "complaints"),
                      menuItem("Hate Crimes based on County", tabName = "hatecrime")
                    )),
                    
                    
                    ## Body content
                    dashboardBody(
                      tabItems(
                        
                        # Home tab content
                        tabItem(tabName = "home",
                                
                                h2("How Covid_19 impacted crime in New York City"),
                                h4("By Joel Mugyenyi, Rishav Agarwal, Shanyue Zeng, Lichun He, Ananya Iyer"),
                                
                                h5("Over the past year, there have been multiple stories about surging crimes in New York City. In this app, we explore the data and examine the relationships between the multiple covid waves New York City has faced and that has impacted crimes committed"),
                                
                                hr(),
                                
                                h5("We focus on the following crime subcategories:"),
                                tags$li("Hate Crimes"),
                                img(src = 'https://i.insider.com/60908fcbf22c6b00185db650?width=2000&format=jpeg&auto=webp', height = '170px', width = '350px'),
                                hr(),
                                tags$li("Domestic Violence"),
                                img(src = 'https://assets.losspreventionmedia.com/uploads/2019/11/Women-domestic-violence-1280x720.jpg', height = '170px', width = '350px'),
                                hr(),
                                tags$li("Victim Subcategories"),
                                img(src = 'https://www.canada.ca/content/dam/themes/policing/victims/20140318.jpg', height = '170px', width = '350px'),
                                
                                hr(),
                                a(
                                  href="https://drive.google.com/drive/folders/13pINm4bOA40jCpoVpQxSu4lqoXUCdYcb?usp=sharing", 
                                  "Click here for data!"
                                ),
                                hr(),
                                a(href="https://github.com/TZstatsADS/spring-2022-project2-group-14", "Click here for the GitHub repo!")
                        ),
                        
                        #------------------Covid & Crime----------------------------
                        tabItem(tabName = "c_19_hc_line", fluidPage(
                          
                          # App title ----
                          titlePanel("Covid-19 & Hate Crime (Time Period)"),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Select items ----
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
                              
                              # Output: ----
                              plotOutput("line_plot"),
                              tableOutput("plot_line_table")
                              
                            )
                          )
                        )
                        
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
                              selectInput(inputId = "motive",
                                          label = "Select a bias motive:",
                                          choices = c("ANTI-JEWISH", "ANTI-ASIAN", "ANTI-MALE HOMOSEXUAL (GAY)",
                                                      "ANTI-BLACK","ANTI-WHITE","ANTI-TRANSGENDER","ANTI-MUSLIM"))
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: tsPlot ----
                              plotOutput(outputId = "tsPlot0")
                              
                            )
                          )
                        )
                        
                        ), 
                        #------------------Covid & Crime----------------------------
                        tabItem(tabName = "c_19_dv", fluidPage(
                          
                          # App title ----
                          titlePanel("Covid-19 & Domestic Violence"),
                          
                          # # Sidebar layout with input and output definitions ----
                          # sidebarLayout(
                          # 
                          #   # Main panel for displaying outputs ----
                          #   mainPanel(
                          # 
                          # Output: tsPlot ----
                          plotOutput(outputId = "tsPlot1")
                          # 
                          #   )
                          # )
                        )
                        
                        ),
                        
                        #------------------Covid & Crime Victims----------------------------
                        tabItem(tabName = "c_19_crime_vics", fluidPage(
                          
                          # App title ----
                          titlePanel("Covid-19 & Crime Victims"),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            sidebarPanel(
                              "The pulldown menu shows victims of crime categorized by sex, race and age group.",
                              
                              selectInput("selected_vic_type",
                                          "Victim category:",
                                          choices = c('VIC_SEX', 'VIC_AGE_GROUP','VIC_RACE')),
                              hr(),
                              "The pulldown menu below is populated with subcategories of the victim category chosen above.",
                              
                              selectInput("selected_vic_subtypes",
                                          "Victim subcategory:",
                                          choices = ""),
                              hr(),
                              HTML("The corresponding dataset can be found here: <a href='https://docs.google.com/spreadsheets/d/1gjSRHR-p2wYBwAXKx2JiAYWL5KKEdt-l4UtZ4CBf6Bg/edit?usp=sharing' target='_blank'>Crime victim categories</a>")
                            ),
                            mainPanel(
                              plotOutput(outputId = "tsPlot2")
                            )
                          )
                        )
                        
                        ), 
                        
                        #------------------MAP----------------------------
                        tabItem(tabName = "map", fluidPage(
                          
                          # App title ----
                          titlePanel("Hate Crime Count by Borough"),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Select ----
                              selectInput(inputId = "Period",
                                          label = "Select a Time Period:",
                                          choices = c("Pre First Wave", "Post First Wave", "All Time"))
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: tmPlot of boroughs ----
                              tmapOutput("my_tmap")
                              
                            )
                          )
                        )
                        
                        ),
                        
                        tabItem(
                          tabName = "complaints", fluidPage(
                            titlePanel("Complaints registered in Manhattan"),
                            
                            mainPanel(
                              plotOutput("hc")
                            )
                            
                          )
                        ),
                        
                        tabItem(tabName = "hatecrime", fluidPage(
                          titlePanel("Hate crimes based on County"),
                          mainPanel(
                            
                            leafletOutput("mclust") 
                          )
                        )
                        )
                        
                      )
                    )
)
