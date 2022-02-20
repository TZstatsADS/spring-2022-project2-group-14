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
      )
      
    )
  )
)