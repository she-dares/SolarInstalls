library(shinydashboard)
library(googleVis)
library(shiny)
shinyUI(dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Solar Panel Installation", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarUserPanel("NYCDSA", image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("Electricity Share & Data Sources", tabName = "tab0", icon = icon("bar-chart-o")),
      menuItem("Coal & Gas Consumption", tabName = "tab1", icon = icon("map")),
      menuItem("US Sunlight Distribution", tabName = "tab2", icon = icon("bar-chart-o")),
      menuItem("Solar Installation Cost", tabName = "tab3", icon = icon("table")),
      menuItem("Potential Carbon Offset Cities", tabName = "tab4", icon = icon("list-alt")),
      menuItem("Potential Solar Install", tabName = "tab5", icon = icon("map"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab0",fluidRow(
        titlePanel("Electricty Generation Percent and Data Sources"),
        plotlyOutput("plot")
      ),
      fluidRow(
        ("Data Sources: "),
        tags$div(class="header", checked=NA,
                 tags$a(href="https://www.kaggle.com/jboysen/google-project-sunroof", "Kaggle: Google Project Sunroof")
        ),
        tags$div(class="header", checked=NA,
                 tags$a(href="https://www.eia.gov/tools/", "U.S. Energy Information Administration")
        ),
        tags$div(class="header", checked=NA,
                 tags$a(href="https://www.nrel.gov/analysis/data-tools.html", "National Renewable Energy Laboratory")
        )
        
      )
      ),
      tabItem(tabName = "tab1", 
           
                  fluidRow(
                    titlePanel("Coal & Gas Consumption"),
                    column(12, align="center",
                           wellPanel(sliderInput("year","Year (click play)", min = 2001, max = 2017, value = 2001, 
                                                 ticks = FALSE, sep="", animate= animationOptions(interval=1700, loop = FALSE), width='50%'))
                    )
                  ),
                  fluidRow(
                    box(htmlOutput("map2")),
                    box(htmlOutput("map3"))
                  )
                 
      ),
      tabItem(tabName = "tab2",fluidRow(
             titlePanel("Sunlight Distribution Across USA in Kilo Watt Hour (KWH)"),
        
              column(5,
                     #wellPanel(
                      # sliderInput("kwh","Select window", min=0,max=500000000,value=c(0, 500000000), ticks= FALSE , width = "80%")
                       #),
                     box(width=15,htmlOutput("table"))
                     ),
              column(7,leafletOutput("map4")
                     )
        
      )
      ),
      tabItem(tabName = "tab3",fluidRow(
        titlePanel("Solar Installation Cost in $/watt"),
        box(dygraphOutput("dyg")
        )
        
      )
      ),
      tabItem(tabName = "tab4",fluidRow(
        titlePanel("Potential Carbon Offset Cities"),
        #box(
          leafletOutput("map5")
        #)
        
      )
      ),
      
      tabItem(tabName = "tab5",fluidRow(
        titlePanel("Potential Opportunities for Solar Installs"),
        plotOutput("bar1"),
        selectInput("n", label="Select top number of cities", choices=c(5,10,15,20,25), selected=15, multiple=FALSE)
        
      )
      )
    )
  )
)
)

