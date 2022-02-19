library(shinydashboard)

ui <- dashboardPage(skin = "purple",
                    title="Covid_19 and Crime in New York City",
                    dashboardHeader(title=span("Covid_19 and Crime in NYC",style="font-size: 16px")),

  dashboardSidebar(   sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Covid-19 Cases & Hate Crimes", tabName = "c_19_hc")
  )),
  
  
  ## Body content
  dashboardBody(
    tabItems(

      # Home tab content
      tabItem(tabName = "home",
              h2("How Covid_19 impacted crime in New York City"),
              h4("By Joel Mugyenyi, Rishav Agarwal, Shanyue Zeng, Lichun He"),
      ),
      
      #------------------New Business----------------------------
      tabItem(tabName = "c_19_hc", fluidPage(
        
        # App title ----
        titlePanel("Covid-19 Cases"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(
            
            # Input: Select for the borough ----
            selectInput(inputId = "motive",
                        label = "Select a bias motive:",
                        choices = c("ANTI-JEWISH", "ANTI-ASIAN", "ANTI-MALE HOMOSEXUAL (GAY)",
                                    "ANTI-BLACK","ANTI-WHITE","ANTI-TRANSGENDER","ANTI-MUSLIM"))
            
          ),
          
          # Main panel for displaying outputs ----
          mainPanel(
            
            # Output: tsPlot on borough ----
            plotOutput(outputId = "tsPlot0")
            
          )
        )
      )
      )
      
      
      
    )
  )
)