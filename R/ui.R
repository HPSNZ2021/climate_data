# DEFINE USER INTERFACE
ui <- fluidPage(
  
  # Application title
  titlePanel("HPSNZ Climate Trends for Training and Competiton Venues",
             "HPSNZ Weather App"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Controls",
                 br(),
                 dateInput(inputId = "start_date", 
                           label = "Start date", 
                           max = Sys.Date()+1, 
                           width = '200px', 
                           value = Sys.Date()-2),
                 dateInput(inputId = "end_date", 
                           label = "End date", 
                           max = Sys.Date()+1, 
                           width = '200px', 
                           value = Sys.Date()-1),
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
                 h5("If your city is not in this list please contact Ben Day.",
                    tags$br()),
                 sliderInput(inputId = "numyears", 
                             label = "How many years?", 2, 
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
                 checkboxGroupInput("show_vars", "Climate data to show:",
                                    c('City',
                                      'Year',
                                      'App Temp Low',
                                      'App Temp High',
                                      'Temperature Low',
                                      'Temperature High',
                                      'Humidity min.',
                                      'Humidity max.',
                                      '% Days HI 30 or over',
                                      '% Days HI 35 or over',
                                      '% Days HI 40 or over',
                                      '% Days rained',
                                      'Rainfall (mm)',
                                      'Wind speed avg (kph)'
                                    ),
                                    selected = c('City',
                                                 'Year',
                                                 'App Temp Low',
                                                 'App Temp High',
                                                 'Temperature Low',
                                                 'Temperature High',
                                                 #'Humidity min.',
                                                 #'Humidity max.',
                                                 '% Days HI 30 or over',
                                                 '% Days HI 35 or over',
                                                 '% Days HI 40 or over',
                                                 '% Days rained',
                                                 'Rainfall (mm)',
                                                 'Wind speed avg (kph)')),
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
                 )
      )
      ),
    
    mainPanel(
      
      #HPSNZ logo
      img(src = "HPSNZ.png", height = 140, width = 500),
      
      h3("Find out what the weather conditions are like for your venue. 
               Find historical data for your date range."),
      
      #text before output
      h5("", tags$br(),
         "Graph below shows apparent temperature High over the period:", 
         tags$br(),
         tags$br(),
         ""),
      
      #outputs
      plotlyOutput(outputId = "tempPlot", width = '100%', height = '500px'),
      tags$br(),
      tags$br(),
      #dataTableOutput(outputId = "dataTable2"),
      DT::dataTableOutput(outputId = "dataTable"),
      h5(tags$br()),
      textOutput("stationText"),
      uiOutput("tab"),                
      textOutput("srcText"),
      h5(tags$br()),
      downloadButton("downloadData", "Download data"),
      downloadButton("downloadPlot", "Download graph"),
      downloadButton("downloadList", "Download country list")
      
    )
  )
)