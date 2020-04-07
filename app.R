# Shiny web app for returning historical weather data from the Darksky API
# Ben Day
# Created 2019/12/18
# Modified 2020/04/07


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

# Rounding function
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("HPSNZ Climate Trends for Training and Competiton Venues"),
    
    sidebarLayout(
        
        sidebarPanel(
            #inputs
            dateInput(inputId = "start_date", label = "Start date", max = Sys.Date()+1, width = '200px'),
            dateInput(inputId = "end_date", label = "End date", max = Sys.Date()+1, width = '200px'),
            selectInput(inputId = "city1", label = "City 1", choices = worldcities$list, width = '300px', 
                        selected = ''),
            selectInput(inputId = "city2", label = "City 2", choices = worldcities$list, width = '300px', 
                        selected = ''),
            numericInput(inputId = "numyears", label = "How many years?", 2, min = 1, max = 10, step = 1, width = '200px'),
            actionButton("submit", label = "Apply"),
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
                                 'Rainfall (mm)'
                                    ),
                               selected = c('City',
                                            'Year',
                                            'App Temp Low',
                                            'App Temp High',
                                            #'Temperature Low',
                                            #'Temperature High',
                                            #'Humidity min.',
                                            #'Humidity max.',
                                            '% Days HI 30 or over',
                                            '% Days HI 35 or over',
                                            '% Days HI 40 or over',
                                            '% Days rained',
                                            'Rainfall (mm)')),
            width = 3),
        
        mainPanel(
    
            #text before output
            h5("", tags$br(),
               "If your city is not in this list please contact Ben Day to add it.",
               "", tags$br(),
               "", tags$br(),
               #"Shows hourly data from 6am to 7pm.",
               #"", tags$br(),
               #"", tags$br(),
               "Apparent Temperature High over the period:", tags$br(),
               ""),
            
            #outputs
            #textOutput(outputId = "headerText"),
            plotOutput(outputId = "tempPlot"),
            #dataTableOutput(outputId = "dataTable2"),
            dataTableOutput(outputId = "dataTable"),
            uiOutput("tab"),
            
            # Button
            downloadButton("downloadData", "Download")
            
        )
    )
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
        # Calculate time interval
        int <- interval(input$start_date, input$end_date + 1)
        int_days <- as.numeric(as.duration(int), "days")
        
        # Avoid future date issue
        if(year(input$start_date) > year(Sys.Date())){year(start_date) <- year(start_date) - 1}
        if(year(input$end_date) > year(Sys.Date())){year(end_date) <- year(end_date) - 1}
        
        # Initialise master df
        j <- 1; ifelse(j == 1, darksky_data1 <- data.frame(matrix(NA, nrow = 0, ncol = 10)),)
        exitt <- 0
        
        # Progress Timer
        withProgress(message = 'Retreiving data from API',
                     detail = 'Please wait...', value = 0, 
            {
                         
                # Set dates vector
                dates <- seq(ymd(input$start_date), ymd(input$end_date), by = "days")
                dates <- sapply(dates, function(x) day(x)) 
                
                ## Input city1 and city2 -> lat/long processing
                #cities <- c(input$city1, input$city2)
                
                ########
                # City 1
                lat <- worldcities[worldcities$list == input$city1, 3]
                long <- worldcities[worldcities$list == input$city1, 4]
                lat_long <- paste0(lat, ",", long)
                
                
                for (j in 1:input$numyears) {
                    
                    try({
                        
                        incProgress(1/5)
                        
                        # Initialise year df
                        i <- 1; ifelse(i == 1, year_data <- data.frame(matrix(NA, nrow = 0, ncol = 10)),)
                        
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
                                    humidity,
                                    #pressure,
                                    precipIntensity
                                    #precipProbability,
                                    #windSpeed,
                                    #windBearing,
                                    #cloudCover,
                                    #uvIndex,
                                    #visibility
                                ) %>%
                                #filter(hour(time) > 5 & hour(time) < 20) #%>%          ### hours filter
                                mutate(day = dates[i],
                                       dayinperiod = i)
                            
                            # Add this day of data to sample
                            year_data <- rbind(year_data, df_hourly)
                        }
                        
                        # Append current year to dataframe
                        year_data$year <- year(date)
                        
                        # Add year median to the dataframe
                        year_data$median <- median(year_data$temperature)
                        
                        # Append dataframe with city
                        year_data$city <- input$city1
                        
                        # Aggregate years
                        darksky_data1 <- rbind(darksky_data1, year_data)
                        
                        #}, silent = TRUE)
                    })
                    
                }
                
                # ########
                # # City 2
                lat <- worldcities[worldcities$list == input$city2, 3]
                long <- worldcities[worldcities$list == input$city2, 4]
                lat_long <- paste0(lat, ",", long)

                j <- 1; ifelse(j == 1, darksky_data2 <- data.frame(matrix(NA, nrow = 0, ncol = 10)),)
                
                for (j in 1:input$numyears) {

                    try({

                        incProgress(2/5)

                        # Initialise year df
                        i <- 1; ifelse(i == 1, year_data <- data.frame(matrix(NA, nrow = 0, ncol = 10)),)

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
                                    humidity,
                                    #pressure,
                                    precipIntensity
                                    #precipProbability,
                                    #windSpeed,
                                    #windBearing,
                                    #cloudCover,
                                    #uvIndex,
                                    #visibility
                            ) %>%
                                #filter(hour(time) > 5 & hour(time) < 20) #%>%          ### hours filter
                                mutate(day = dates[i],
                                dayinperiod = i)

                            # Add this day of data to sample
                            year_data <- rbind(year_data, df_hourly)
                        }
                        
                        # Append current year to dataframe
                        year_data$year <- year(date)
                        
                        # Add year median to the dataframe
                        year_data$median <- median(year_data$temperature)
                        
                        # Append dataframe with city
                        year_data$city <- input$city2
                        
                        # Aggregate years
                        darksky_data2 <- rbind(darksky_data2, year_data)
                        
                        #}, silent = TRUE)
                    })

                }

                darksky_data <- rbind(darksky_data1, darksky_data2)
                #darksky_data <- darksky_data1

        })
        
        #return(year_data)
        return(darksky_data)
        
    })
    
    
    output$tempPlot <- renderPlot({
        
        darksky_data = data()
        
        # Plot temperature
        darksky_data %>%
            group_by(year, dayinperiod, city) %>%
            summarise(maxaTemp = max(apparentTemperature)) %>%
        ggplot(.) +
            #geom_violin(aes(x = factor(year), y = temperature, colour = factor(year))) +
            #geom_point(aes(x = factor(year), y = temperature, colour = factor(year), size = humidity)) +
            #geom_point(aes(x = factor(year), y = median, size = 2)) +
            geom_point(aes(x = dayinperiod, y = maxaTemp, 
                           colour = factor(city), shape = factor(year), size = 2)) +
            #geom_line(aes(x = dayinperiod, y = maxaTemp, 
            #              colour = factor(city)), linetype = "dashed") +
            theme_light() +
            theme(legend.position = "none") + 
            scale_size_continuous(range = c(2,10)) +
            scale_x_discrete(limits = as.character(unique(darksky_data$day))) +
            scale_y_continuous(limits = c(0, round_any(max(darksky_data$apparentTemperature), 10, f = ceiling)), breaks = seq(0,50,5)) +
            theme(text = element_text(size = 16)) +
            xlab('Day in period') + ylab('Apparent Temperature (°C)') +
            legend_top() + guides(size = FALSE) +
            scale_colour_discrete("City") +
            scale_shape_discrete("Year")
        
    })
    
    
    output$dataTable <- renderDataTable({
        
        darksky_data = data()
        
        darksky_data %>%
            mutate(humidity = 100 * humidity,
                   temperature = round(temperature, 1),
                   apparentTemperature = round(apparentTemperature, 1),
                   year = year(time),
                   dayz = day(time)) %>%
            group_by(year, city) %>%
            summarise(atempMin = min(apparentTemperature),
                      atempMax = max(apparentTemperature),
                      tempMin = min(temperature),
                      tempMax = max(temperature),
                      humidMin = ceiling(min(humidity)),
                      humidMax = ceiling(max(humidity)),
                      over30 = (n_distinct(dayz[apparentTemperature >= 30])/n_distinct(dayz))*100,
                      over35 = (n_distinct(dayz[apparentTemperature >= 35])/n_distinct(dayz))*100,
                      over40 = (n_distinct(dayz[apparentTemperature >= 40])/n_distinct(dayz))*100,
                      raindays = (n_distinct(dayz[precipIntensity > 0.1])/n_distinct(dayz))*100,
                      rainfall = sum(precipIntensity)
            ) %>%
            rename('City' = city,
                   'Year' = year,
                   'App Temp Low' = atempMin,
                   'App Temp High' = atempMax,
                   'Temperature Low' = tempMin,
                   'Temperature High' = tempMax,
                   'Humidity min.' = humidMin,
                   'Humidity max.' = humidMax,
                   '% Days HI 30 or over' = over30,
                   '% Days HI 35 or over' = over35,
                   '% Days HI 40 or over' = over40,
                   'Rainfall (mm)' = rainfall,
                   '% Days rained' = raindays
            ) %>%
            mutate_if(is.numeric, round, 1) %>%
            arrange(City) %>%
            select(input$show_vars)

    }, options = list(dom  = '<"top">t<"bottom">',
                      searching = F), );
    #});
    

    url <- a("DarkSky API", href="https://darksky.net/dev/docs/sources")
    output$tab <- renderUI({
        tagList("Data Sources:", url)
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$city1, "+", input$city2, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data(), file, row.names = FALSE)
        }
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
