# Shiny web app for returning historical weather data from the Darksky API
# Ben Day
# Created 2019/12/18
# Modified 2020/02/11


library(shiny)
library(rjson)
library(purrr)
library(repurrrsive)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggthemr)

# Lat and long from https://simplemaps.com/data/world-cities
worldcities <- as_tibble(readRDS(file = "worldcities.rds"))

worldcities <- worldcities %>%
    mutate(list = paste0(city_ascii, "  -  ", country))
worldcities[1, ] <- ""


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("HPSNZ Climate Trends for Training and Competiton Venues"),
    
    #inputs
    dateInput(inputId = "start_date", label = "Start date", max = Sys.Date()+1, width = '200px'),
    dateInput(inputId = "end_date", label = "End date", max = Sys.Date()+1, width = '200px'),
    selectInput(inputId = "city", label = "City", choices = worldcities$list, width = '300px', 
                selected = ''),
    numericInput(inputId = "numyears", label = "How many years?", 5, min = 1, max = 10, step = 1, width = '200px'),
    actionButton("submit", label = "Apply"),
    
    #text before output
    h5("", tags$br(),
       "If your city is not in this list please contact Ben Day to add it.",
       "", tags$br(),
       "", tags$br(),
       "Shows hourly data from 6am to 7pm.",
       "", tags$br(),
       "", tags$br(),
       "Temperature range (x axis) and humidity (circle size) over the period:", tags$br(),
       ""),
    
    #outputs
    #textOutput(outputId = "headerText"),
    plotOutput(outputId = "tempPlot"),
    dataTableOutput(outputId = "dataTable"),
    uiOutput("tab")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- eventReactive(input$submit, {
        
        #----------------------------------------------------------------------------------
        ### CODE BLOCK (darksky.R)
        
        # INITIALISATIONS -----------------------------------------------
        
        # Static API call components
        url_base        <- "https://api.darksky.net/forecast/"
        api_key         <- "0892828c9d27ce19b523d667698ac088/"
        time            <- "00:00:00"
        url_exclusions  <- "?units=si"
        
        
        # FUNCTIONS ------------------------------------------------------
        
        # Input city -> lat/long processing
        lat <- worldcities[worldcities$list == input$city, 3]
        long <- worldcities[worldcities$list == input$city, 4]
        lat_long <- paste0(lat, ",", long)
        
        
        # Calculate time interval
        int <- interval(input$start_date, input$end_date)
        int_days <- as.numeric(as.duration(int), "days")
        
        # Avoid future date issue
        if(year(input$start_date) > year(Sys.Date())){year(start_date) <- year(start_date) - 1}
        if(year(input$end_date) > year(Sys.Date())){year(end_date) <- year(end_date) - 1}
        
        # Initialise master df
        j <- 1; ifelse(j == 1, darksky_data <- data.frame(matrix(NA, nrow = 0, ncol = 14)),)
        exitt <- 0
        
        # Progress Timer
        withProgress(message = 'Retreiving data from API',
                     detail = 'Please wait...', value = 0, {
                     
        
        for (j in 1:input$numyears) {
            
            incProgress(1/5)
            
            # Initialise year df
            i <- 1; ifelse(i == 1, year_data <- data.frame(matrix(NA, nrow = 0, ncol = 14)),)
            
            for (i in 1:int_days) {
                
                # Construct date
                date <- ymd(input$start_date) + i
                year(date) <- as.numeric(year(input$start_date)) - input$numyears + j
                
                # Call API
                json_file <- paste0(url_base, api_key, paste0(lat_long, ",", date, "T", time, url_exclusions))
                json_data <- jsonlite::fromJSON(txt = json_file)
                
                # Turn data block (lists) into data frames
                hourly <- as_tibble(json_data$hourly$data)
                daily <- as_tibble(json_data$daily$data)
                
                
                
                # Change UNIX time to datetime
                #hourly$time <- as_datetime(hourly$time)
                hourly$time <- as_datetime(as.numeric(hourly$time))
                
                # Remove windGust as a variable
                df_hourly <- hourly %>% #select(-one_of("windGust")) %>%
                    select(
                        time,
                        #summary,
                        temperature,
                        apparentTemperature,
                        #dewPoint,
                        humidity
                        #pressure,
                        #precipIntensity,
                        #precipProbability,
                        #windSpeed,
                        #windBearing,
                        #cloudCover,
                        #uvIndex,
                        #visibility
                    ) %>%
                    filter(hour(time) > 5 & hour(time) < 20)
                
                # Add this day of data to sample
                year_data <- rbind(year_data, df_hourly)
            }
            
            # Append current year to dataframe
            year_data$yearz <- year(date)
            
            # Add year median to the dataframe
            year_data$median <- median(year_data$temperature)
            
            # Aggregate years
            darksky_data <- rbind(darksky_data, year_data)
            
            }
        
        })
        
        return(darksky_data)
        
    })
    
    
    # output$headerText <- renderText({
    #     darksky_data = data()
    #     
    #     # CHECK FOR TEMPERATURE
    #     ifelse(!("temperature" %in% colnames(darksky_data)),
    #            "No temperature for this city. Please select another!",
    #            input$city)
    # })
    
    output$tempPlot <- renderPlot({
        
        darksky_data = data()

        
        # Plot temperature
        ggplot(darksky_data) +
            geom_violin(aes(x = factor(yearz), y = temperature, colour = factor(yearz))) +
            geom_point(aes(x = factor(yearz), y = temperature, colour = factor(yearz), size = humidity)) +
            geom_point(aes(x = factor(yearz), y = median, size = 2)) +
            geom_line(aes(x = factor(yearz), y = temperature, colour = factor(yearz))) +
            theme_light() +
            theme(legend.position = "none") + 
            scale_size_continuous(range = c(2,10)) +
            scale_y_continuous(breaks=seq(0,50,5)) +
            theme(text = element_text(size=16)) +
            xlab('') + ylab('Temperature (C)')
        
    })
    
    output$dataTable <- renderDataTable({
        darksky_data = data()
        
        darksky_data %>%
            mutate(humidity = 100 * humidity,
                   temperature = round(temperature, 1),
                   apparentTemperature = round(apparentTemperature, 1),
                   yearz = year(time),
                   dayz = day(time)) %>%
            group_by(yearz) %>%
            summarise(atempMin = min(apparentTemperature),
                      atempMax = max(apparentTemperature),
                      tempMin = min(temperature),
                      tempMax = max(temperature),
                      humidMin = ceiling(min(humidity)),
                      humidMax = ceiling(max(humidity)),
                      over30 = (n_distinct(dayz[apparentTemperature >= 30])/n_distinct(dayz))*100,
                      over35 = (n_distinct(dayz[apparentTemperature >= 35])/n_distinct(dayz))*100,
                      over40 = (n_distinct(dayz[apparentTemperature >= 40])/n_distinct(dayz))*100
            ) %>%
            rename('Year' = yearz,
                   'App Temp Low' = atempMin,
                   'App Temp High' = atempMax,
                   'Temperature Low' = tempMin,
                   'Temperature High' = tempMax,
                   'Humidity min.' = humidMin,
                   'Humidity max.' = humidMax,
                   '% Days HI 30 or over' = over30,
                   '% Days HI 35 or over' = over35,
                   '% Days HI 40 or over' = over40
            ) %>%
            mutate_if(is.numeric, round, 1)
        
    }, options = list(dom  = '<"top">t<"bottom">',
                      searching = F), );
    
    

    url <- a("DarkSky API", href="https://darksky.net/dev/docs/sources")
    output$tab <- renderUI({
        tagList("Data Sources:", url)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
