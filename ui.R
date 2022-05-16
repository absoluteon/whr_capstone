
header <- dashboardHeader(title = "World Happiness Report Analytics")
  
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Overview", tabName = "overview", icon = icon("globe-asia")),
      menuItem(text = "Through the Years", tabName = "thruyear", icon = icon("chart-line")),
      menuItem(text = "Factors", tabName = "factors", icon = icon("balance-scale"))
    )
    
  )
  
body <- dashboardBody(
    tabItems(
      
      tabItem(tabName = "overview",
              
              fluidPage(
                h2(tags$b("World Happiness Report")),
                br(),
                div(style = "text-align:justify", 
                    p("The World Happiness Report is a publication of the United Nations Sustainable Development Solutions Network. 
                      It contains articles and rankings of national happiness, based on respondent ratings of their own lives, 
                      which the report also correlates with various (quality of) life factors.
                      This dashboard is built based on the 2021 World Happiness Report."),
                    br()
                ) # end div
              ), # end fluidPage
              
              fluidPage(
                tabBox(width = 8,
                       title = tags$b(""),
                       id = "tabset1",
                       side = "right",
                       tabPanel(tags$b("Unhappiest Countries"), 
                                plotlyOutput("plot2")
                       ),
                       tabPanel(tags$b("Happiest Countries"), 
                                plotlyOutput("plot1")
                       )    
                ),
                
                valueBox(
                  value = happiest_country,
                  subtitle = "Happiest country",
                  icon = icon("smile"),
                  color = "light-blue"),
                
                valueBox(
                  value = "9 of 10",
                  subtitle = "happiest is in Europe",
                  # icon = icon("smile"),
                  color = "light-blue"),
                
                
                valueBox(
                  value = unhappiest_country,
                  subtitle = "Unhappiest country",
                  icon = icon("frown"),
                  color = "orange"),
                
                valueBox(
                  value = "7 out of 10",
                  subtitle = "unhappiest is in Africa",
                  # icon = icon("frown"),
                  color = "orange")
                
              ), # end fluidPage 
              
              fluidPage(
                box(width = 12,
                    solidHeader = T,
                    h3(tags$b("Happiness Score of Countries")),
                    leafletOutput("plot_leaflet"))
              ) # end fluidPage
              
      ), #end tabItem overview
      
      
      tabItem(tabName = "thruyear",      

              fluidPage(
                box(width = 9,
                    solidHeader = T,
                    h3(tags$b("Happiness Score Through the Years")),
                    h5("Comparison between countries"),
                    plotlyOutput("plot4")
                ), # end box
                
                box(width = 3,
                    solidHeader = T,
                    background = "light-blue",
                    height = 240,
                    selectInput(inputId = "country_1",
                                label = h4(tags$b("Select first country:")),
                                choices = selectCountry1,
                                selected = "Malaysia") # end selectInput
                ), # end box
                
                box(width = 3,
                    solidHeader = T,
                    background = "light-blue",
                    height = 240,
                    selectInput(inputId = "country_2",
                                label = h4(tags$b("Select second country:")),
                                choices = selectCountry2,
                                selected = "Indonesia") # end selectInput
                ) # end box
                
              ), # end fluidPage
              
              fluidPage(
                box(width = 9,
                    solidHeader = T,
                    h3(tags$b("Regions Through the Years")),
                    h5("Selected Top 15 Happiest Countries for each Region (if available)"),
                    plotlyOutput("plot5")
                ), # end box
                
                box(width = 3,
                    solidHeader = T,
                    # background = "light-blue",
                    height = 150,
                    sliderInput(inputId = "year_1",
                                label = h4(tags$b("Select year range:")),
                                sep = "",
                                ticks = TRUE,
                                round = TRUE,
                                step = 1,
                                min = 2010,
                                max = year_range[2],
                                value = c(2012, 2019)) # end sliderInput
                ), # end box   
                
                box(width = 3,
                    solidHeader = T,
                    background = "light-blue",
                    selectInput(inputId = "region_1",
                                label = h4(tags$b("Select region:")),
                                choices = selectRegion,
                                selected = "Europe & Central Asia") # end selectInput
                ) # end box
                
                
                
              ) # end fluidPage     
              
      ), #end tabItem thruyear
      

      tabItem(tabName = "factors",      
              
              fluidPage(
                tabBox(width = 12,
                       title = tags$b(""),
                       id = "tabset2",
                       side = "right",
                       tabPanel(tags$b("Unhappiest Countries"), 
                                plotlyOutput("plot7")
                       ),
                       tabPanel(tags$b("Happiest Countries"), 
                                plotlyOutput("plot6")
                       )    
                ), # end tabBox
              
              ), # end fluidPage
                
              fluidPage(
                box(width = 9,
                    solidHeader = T,
                    h3(tags$b("Happiness Score in Perspective")),
                    h5("Happiness, Life Expectancy, and GDP per Capita of Continents"),
                    plotlyOutput("plot8")
                ), # end box
                
                box(width = 3,
                    solidHeader = T,
                    background = "light-blue",
                    height = 240,
                    selectInput(inputId = "continent_1",
                                label = h4(tags$b("Select first continent:")),
                                choices = selectContinent1,
                                selected = "Europe") # end selectInput
                ), # end box
                
                box(width = 3,
                    solidHeader = T,
                    background = "light-blue",
                    height = 240,
                    selectInput(inputId = "continent_2",
                                label = h4(tags$b("Select second continent:")),
                                choices = selectContinent2,
                                selected = "Africa") # end selectInput
                
                ) # end box
                
              ) # end fluidPage     
              
      ) #end tabItem factor
      
    ) # end tabItems
  ) # end dashboardBody


dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar,
  skin = "blue"
)