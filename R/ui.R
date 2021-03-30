# DEFINE USER INTERFACE
ui <- fluidPage(
  
  # STYLES ------------------------------------------------------------------
  list(
    tags$head(tags$link(rel = "icon",
                        href = "https://img.pngio.com/sun-icon-png-50-px-free-png-sun-black-and-white-1600_1600.png",
                        type = "image/vnd.microsoft.icon"))),
  div(style = "padding: 0px 0px; width: '100%'; margin-top:1em;",
      titlePanel(windowTitle = "HPSNZ Weather App",
                 title = div("HPSNZ Venue Weather Trends",
                             img(src = "HPSNZ.png", height = 46, width = 156)))),
  tags$style(HTML("
    #.tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li > a[data-value='heat'] {background-color: #ff7eb6;   color:black}
    .tabbable > .nav > li > a[data-value='cold'] {background-color: #82cfff;  color:black}
    #.tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
  ")
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Controls",
                 br(),
                 radioButtons(inputId = "numCities", 
                              label = "Cities", 
                              choices = c(1, 2), 
                              inline = TRUE),
                 selectizeInput(inputId = "city1", 
                                label = "City 1", 
                                choices = NULL,
                                width = '300px', 
                                options = list(placeholder = 'select a city')),
                 conditionalPanel(condition = "input.numCities == 2",
                                  selectizeInput(inputId = "city2", 
                                                 label = "City 2", 
                                                 choices = NULL,
                                                 width = '300px')), 
                 h5("If your city is not in this list please go to New City tab.",
                    tags$br()),
                 dateInput(inputId = "start_date", 
                           label = "Start date", 
                           max = Sys.Date()+1, 
                           width = '200px', 
                           value = Sys.Date()-3),
                 dateInput(inputId = "end_date", 
                           label = "End date", 
                           max = Sys.Date()+1, 
                           width = '200px', 
                           value = Sys.Date()-1),
                 sliderInput(inputId = "numyears", 
                             label = "How many years?", 3, 
                             min = 1, 
                             max = 10, 
                             step = 1, 
                             width = '200px', 
                             ticks = FALSE),
                 checkboxGroupInput(inputId = "timeslot", 
                                    label = "Time of day", 
                                    choices = c("Early (5-8am)",
                                                "Morning (9-12pm)",
                                                "Afternoon (1-3pm)",
                                                "Evening (4-7pm)",
                                                "Night (8-12am)",
                                                "Overnight (1-4am)"),
                                    selected = c("Early (5-8am)",
                                                 "Morning (9-12pm)",
                                                 "Afternoon (1-3pm)",
                                                 "Evening (4-7pm)"),
                                    width = '300px'),
                 actionButton("submit", 
                              label = "Apply",
                              style = "color: #fff; 
                         background-color: #337ab7; 
                         border-color: #2e6da4;
                         font-size: 16px;"),
                 h5("", tags$br(),""),
                 checkboxGroupInput("show_vars", "Weather data to show:",
                                    c('App Temp Low (°C)',
                                      'App Temp High (°C)',
                                      'Temp Low (°C)',
                                      'Temp High (°C)',
                                      'Humidity Min (%)',
                                      'Humidity Max (%)',
                                      'Wind Chill Low (°C)',
                                      'Wind Chill High (°C)',
                                      'Wind Chill Avg (°C)',
                                      'Wind Speed avg (kph)',
                                      'Rainfall (mm)',
                                      '% Days Rained',
                                      '% Days HI 30 or over',
                                      '% Days HI 35 or over',
                                      '% Days HI 40 or over'
                                      ),
                                    selected = c('App Temp Low (°C)',
                                                 'App Temp High (°C)',
                                                 'Temp Low (°C)',
                                                 'Temp High (°C)',
                                                 #'Humidity Min (%)',
                                                 #'Humidity Max (%)',
                                                 #'Wind Chill Low (°C)',
                                                 #'Wind Chill High (°C)',
                                                 #'Wind Chill Avg (°C)',
                                                 'Wind Speed avg (kph)',
                                                 'Rainfall (mm)',
                                                 '% Days Rained'
                                                 #'% Days HI 30 or over',
                                                 #'% Days HI 35 or over',
                                                 #'% Days HI 40 or over'
                                                 )
                                    ),
                 width = 3),
        tabPanel("New city",
                 br(),
                 textInput(inputId = "newname",
                           label = "Name",
                           width = '70%'),
                 textInput(inputId = "newcountry",
                           label = "Country",
                           width = '70%'),
                 numericInput(inputId = "newlat",
                              label = "Latitude  (e.g. 77.5 S = -77.5)",
                              value = NULL,
                              min = -90,
                              max = 90,
                              width = '70%'),
                 numericInput(inputId = "newlon",
                              label = "Longitude  (e.g. 150.1 W = -150.1)",
                              value = NULL,
                              min = -180,
                              max = 180,
                              width = '70%'),
                 actionButton(inputId = "add_city",
                              label = "Add city",
                              style = "color: #fff; 
                         background-color: #337ab7; 
                         border-color: #2e6da4;
                         font-size: 16px;"),
                 h5('Note: your app will reload.')
                 ),
        tabPanel("Downloads",
                 br(),
                 downloadButton("downloadData", "Download weather data"),
                 br(),br(),
                 downloadButton("downloadPlot", "Download graph"),
                 br(),br(),
                 downloadButton("downloadList", "Download country list")
        )
      )
      ),
    
    # MAIN --------------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel(title = div("Heat", icon('sun')), value = 'heat',
                 h3("Find weather conditions for your venue. 
               Find historical data for your date range."),
                 
                 #text before output
                 h5("", tags$br(),
                    "Graph below shows apparent temperature high over the period:", 
                    tags$br(),
                    tags$br(),
                    ""),
                 h3('*NOTE - cold weather features in development*'),
                 
                 #outputs
                 plotlyOutput(outputId = "tempPlot", width = '100%', height = '500px'),
                 tags$br(),
                 tags$br(),
                 DT::dataTableOutput(outputId = "dataTable"),
                 h5(tags$br()),
                 textOutput("latlonText"),
                 textOutput("stationText"),
                 h5(tags$br()),
                 uiOutput("tab"),                
                 textOutput("srcText"),
                 h5(tags$br())
        ),
        tabPanel(title = div("Cold", icon('snowflake')), value = 'cold',
                 br()
        )
      )
    )
  )
)