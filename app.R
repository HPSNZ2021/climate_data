# Shiny web app for returning historical weather data from the Darksky API
# Ben Day
# 2019/11/19


library(shiny)
library(rjson)
library(purrr)
library(repurrrsive)
library(lubridate)
library(tidyverse)

city_list <- as_tibble(read.csv(file = "data/city_list.csv"))

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
    selectInput(inputId = "city", label = "City", choices = city_list$list, width = '450px'),
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

    output$tempPlot <- renderPlot({

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

        # Plot temperature               
        plot(darksky_data$time, darksky_data$temperature, type="l")
        title("Temperature over period (deg C)", 
              xlab = "Day of period", 
              ylab = "Temperature")
        
        return(darksky_data)
        
    })
    
    output$dataTable <- renderTable({
        
        output <- darksky_data
        
        maxTemps <- output %>%
            mutate(day = day(time)) %>%
            group_by(day) %>%
            summarise(value = max(temperature))
        
        minTemps <- output %>%
            mutate(day = day(time)) %>%
            group_by(day) %>%
            summarise(value = min(temperature))
        
        maxHumid <- output %>%
            mutate(day = day(time)) %>%
            group_by(day) %>%
            summarise(value = max(humidity))
        
        minHumid <- output %>%
            mutate(day = day(time)) %>%
            group_by(day) %>%
            summarise(value = min(humidity))
        
        outputApp <- as_tibble(cbind(minTemps$day, 
                                     minTemps$value, 
                                     maxTemps$value, 
                                     minHumid$value, 
                                     maxHumid$value), 
                               .name_repair = 'unique')
        
        outputApp <- outputApp %>% 
            mutate_all(., as.numeric) %>%
            rename("day" = "...1", 
                   "temp_min" = "...2", 
                   "temp_max" = "...3", 
                   "humidity_min" = "...4", 
                   "humidity_max" = "...5")
    
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
