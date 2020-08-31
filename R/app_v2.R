# Shiny web app for returning historical weather data from the Darksky API
# Ben Day
# 2019/11/19


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
    titlePanel("HPSNZ Environmental Trends for Training/Compeititon Venues"),
    
    #inputs
    dateInput(inputId = "start_date", label = "Start date"),
    dateInput(inputId = "end_date", label = "End date"),
    #numericInput(inputId = "lat", label = "Latitude", value = -36.8629409),
    #numericInput(inputId = "long", label = "Longitude", value = 174.7253886),
    selectInput(inputId = "city", label = "City", choices = worldcities$list, width = '450px', 
                selected = ''),
    actionButton("submit", label = "Apply"),
    
    #text before output
    h5("", tags$br(),
       "Temperature over the period:", tags$br(),
       ""),
    
    #outputs
    plotOutput(outputId = "tempPlot"),
    dataTableOutput(outputId = "dataTable")
    
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
        
        
        # Initialise output df
        i <- 1; ifelse(i == 1, darksky_data <- data.frame(matrix(NA, nrow = 0, ncol= 14)),)
        
        # Calculate time interval
        int <- interval(input$start_date, input$end_date)
        int_days <- as.numeric(as.duration(int), "days")
        
        for (i in 1:int_days) {
            
            date <- ymd(input$start_date) + i
            json_file <- paste0(url_base, api_key, paste0(lat_long, ",", date, "T", time, url_exclusions))
            json_data <- jsonlite::fromJSON(txt = json_file)
            
            # Turn data block (lists) into data frames
            hourly <- as_tibble(json_data$hourly$data)
            daily <- as_tibble(json_data$daily$data)
            
            # Change UNIX time to datetime
            hourly$time <- as_datetime(hourly$time)
            
            # Remove windGust as a variable
            df_hourly <- hourly %>% select(-windGust, ) %>%
                select(
                    time,
                    summary,
                    temperature,
                    apparentTemperature,
                    dewPoint,
                    humidity,
                    pressure,
                    precipIntensity,
                    precipProbability,
                    windSpeed,
                    windBearing,
                    cloudCover,
                    uvIndex,
                    visibility
                )
            
            # Add this day of data to sample
            darksky_data <- rbind(darksky_data, df_hourly)
        }
        
        return(darksky_data)
        
    })
    
    output$tempPlot <- renderPlot({
        darksky_data = data()
        
        # Plot temperature
        ggplot(darksky_data, aes(x = time,y = temperature)) +
            theme_light() +
            geom_point(aes(size = humidity, colour = as.factor(day(time)))) +
            theme(legend.position = "none") + 
            geom_line() +
            scale_size_continuous(range = c(2,10)) +
            scale_y_continuous(breaks=seq(0,50,5)) +
            theme(text = element_text(size=16)) +
            xlab('') + ylab('Temperature [C]')
        
    })
    
    output$dataTable <- renderDataTable({
        darksky_data = data()
        
        darksky_data %>%
            mutate(humidity = 100 * humidity,
                   temperature = round(temperature, 1),
                   dayz = day(time)) %>%
            group_by(dayz) %>%
            summarise(tempMin = min(temperature),
                      tempMax = max(temperature),
                      humidMin = ceiling(min(humidity)),
                      humidMax = ceiling(max(humidity))
            ) %>%
            rename('Day of period' = dayz,
                   'Temperature Low' = tempMin,
                   'Temperature High' = tempMax,
                   'Humidity min.' = humidMin,
                   'Humidity max.' = humidMax
            ) %>%
            mutate_if(is.numeric, round, 3)
        
    }, options = list(dom  = '<"top">t<"bottom">',
                      searching = F), )
}


# Run the application 
shinyApp(ui = ui, server = server)