library(shinydashboard)

ui <- dashboardPage(skin = "purple",
                    title="Covid_19 and Crime in New York City",
                    dashboardHeader(title=span("Covid_19 and Crime in NYC",style="font-size: 16px")),

  dashboardSidebar(   sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Covid-19 Cases & Hate Crimes", tabName = "c_19_hc"),
    menuItem("Covid-19 Cases & Domestic Violence", tabName = "c_19_dv"),
    menuItem("Covid-19 Cases & Crime Victims", tabName = "c_19_crime_vics"),
    menuItem("Map", tabName = "map")
  )),
  
  
  ## Body content
  dashboardBody(
    tabItems(

      # Home tab content
      tabItem(tabName = "home",
              # h2("How Covid_19 impacted crime in New York City"),
              # h4("By Joel Mugyenyi, Rishav Agarwal, Shanyue Zeng, Lichun He"),
              h4(" dataset posted on 03.12.2020, by Simon De-Ville, Virginia Stovin, Christian Berretta, JOERG WERDIN, Simon Poë"),
              a(
                href="https://figshare.shef.ac.uk/articles/dataset/Hadfield_Green_Roof_5-year_Dataset/11876736", 
                "Click here for data!"
              ),
              h5("Data collected as part of the EU funded 'Collaborative research and development of green roof system technology' project from the Sheffield, UK, green roof testbeds."),
              h5("Data includes 5 years of:"),
              tags$li("Rainfall data (1-minute resolution)"),
              tags$li("Green roof runoff data for 9 roof configurations (1-minute resolution)"),
              tags$li("Soil moisture content at 3 depths for 4 roof configurations (5-minute resolution)"),
              tags$li("Climate data sufficient to calculate FAO-56 Penman-Monteith (1-hour resolution)"),
              h5("Due to difficulties in monitoring testbed runoff, there are occasions where runoff data is considered invalid. A separate data-file indicates individual storm events where runoff data is considered to be valid."),
              a(href="https://github.com/yld-weng/hadfield-green-roof", "Click here for the GitHub repo!")
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
      )
      
    )
  )
)