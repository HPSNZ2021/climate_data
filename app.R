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

city_list <- as_tibble(readRDS(file = "city_list.rds"))

city_list <- city_list %>%
    mutate(list = paste0(city, "  -  ", airport))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("HPSNZ Environmental Trends for Training/Compeititon Venues"),

    #inputs
    dateInput(inputId = "start_date", label = "Start date"),
    dateInput(inputId = "end_date", label = "End date"),
    #numericInput(inputId = "lat", label = "Latitude", value = -36.8629409),
    #numericInput(inputId = "long", label = "Longitude", value = 174.7253886),
    selectInput(inputId = "city", label = "City", choices = city_list$list, width = '450px',
                selected = 'AUCKLAND  -  AUCKLAND INTERNATIONAL'),
    submitButton(),
    
    #text before output
    h5("", tags$br(),
       "Temperature over the period:", tags$br(),
       ""),
    
    #outputs
    plotOutput(outputId = "tempPlot"),
    tableOutput(outputId = "dataTable")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- reactive({

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
        lat <- city_list[city_list$list == input$city, 4]
        long <- city_list[city_list$list == input$city, 5]
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
            geom_point(aes(size = 2, colour = as.factor(day(time)))) +
            theme(legend.position = "none")
            
    })
    
    output$dataTable <- renderTable({
        darksky_data = data()
        
        darksky_data %>%
            mutate(humidity = 100 * humidity,
                   dayz = day(time)) %>%
            group_by(dayz) %>%
            summarise(tempMin = round(min(temperature),3),
                      tempMax = round(max(temperature),3),
                      humidMin = ceiling(min(humidity)),
                      humidMax = ceiling(max(humidity))
                      ) %>%
            rename('Day of period' = dayz,
                   'Temperature L' = tempMin,
                   'Temperature H' = tempMax,
                   'Humidity min.' = humidMin,
                   'Humidity max.' = humidMax
            ) %>%
            mutate_if(is.numeric, round, 3)

        }, width = '600px')
}


# Run the application 
shinyApp(ui = ui, server = server)
