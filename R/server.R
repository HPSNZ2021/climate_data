# DEFINE SERVER PROCESSING
server <- function(input, output, session) {
  
  # Load dataset
  worldcities <- as_tibble(readRDS(file = "worldcities.rds"))
  
  # Add a new city when button is pressed
  observeEvent(input$add_city,{
    
    if (length(list(input$newname, input$newlat, input$newlon, input$newcountry)) != 4) {
      showNotification("All fields required",
                       type = 'error')
    }
    
    else {
      
      add_city(city_name = paste0(input$newname), 
               latitude = input$newlat,
               longitude = input$newlon,
               cntry = paste0(input$newcountry))
      
      showNotification(paste0("Success! ", input$newname, " added!"),
                       type = 'message')
      showNotification("Your app will now refresh",
                       type = 'warning')
      
      # Cause a reload
      Sys.sleep(1.5)
      session$reload()
    }
    
  })
  
  observe({
    
    # Update city choices
    updateSelectizeInput(session, 'city1', choices = worldcities$list, server = TRUE)
    if (input$numCities == 2) {
      updateSelectizeInput(session, 'city2', choices = worldcities$list, server = TRUE)
    }
    
  })

  # Main API processing -----------------------------------------------------
  data <- eventReactive(input$submit, {
    
    if (input$city1 == '') {
      showNotification('Choose a city', type = 'warning')
      return()
    }
    else {
      
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
      j <- 1; if (j == 1) {darksky_data1 <- data.frame(matrix(NA, nrow = 0, ncol = 10))}
      exitt <- 0
      
      # Progress Timer
      withProgress(message = 'Retreiving data from API',
                   detail = 'Please wait...',
                   {
                     try({
                       
                       # Set dates vector
                       dates <- seq(ymd(input$start_date), ymd(input$end_date), by = "days")
                       dates <- sapply(dates, function(x) day(x)) 
                       
                       
                       ########
                       # City 1
                       lat <- worldcities[worldcities$list == input$city1, 3]
                       long <- worldcities[worldcities$list == input$city1, 4]
                       if (length(lat) > 1) {lat <- lat[1]} # Default to first entry if duplicate city
                       if (length(long) > 1) {long <- long[1]}
                       lat_long <- paste0(lat, ",", long)
                       
                       # Find timezone offset
                       tzcall <- paste0("http://api.geonames.org/timezoneJSON?formatted=true&lat=", 
                                        lat, "&lng=", long, "&username=bendaytoday&style=full")
                       tzdata <- jsonlite::fromJSON(txt = tzcall)
                       tzoffset <- tzdata$gmtOffset
                       
                     })
                     
                     for (j in 1:input$numyears) {
                       
                       if (input$numCities == 1) incProgress(1 / input$numyears)
                       else incProgress(0.5 / input$numyears)
                       
                       # Initialise year df
                       i <- 1; if (i == 1) {year_data <- data.frame(matrix(NA, nrow = 0, ncol = 10))}
                       
                       for (i in 1:int_days) {
                         
                         
                         # Construct date
                         date <- ymd(input$start_date) + i
                         year(date) <- as.numeric(year(input$start_date)) - input$numyears + j
                         
                         # Call API
                         json_file <- paste0(url_base, api_key, paste0(lat_long, ",", date, "T", time, url_exclusions))
                         json_data1 <- jsonlite::fromJSON(txt = json_file)
                         
                         # --------------------
                         # If no hourly data then skip
                         if ('hourly' %in% names(json_data1)) {
                           
                           
                           # Data sources/weather station
                           src1 <- json_data1$flags$sources
                           station1 <- json_data1$flags$`nearest-station`
                           
                           # Turn data block (lists) into data frames
                           hourly <- as_tibble(json_data1$hourly$data)
                           daily <- as_tibble(json_data1$daily$data)
                           
                           # Change UNIX time to datetime
                           hourly$time <- as_datetime(as.numeric(hourly$time))
                           
                           tryCatch({
                             
                             # Account for timezone
                             hour(hourly$time) <- hour(hourly$time) + floor(tzoffset)
                             
                           }, error = function(e){
                             cat("Timezone adjustment didn't work.")
                           })
                           
                           
                           
                           # Remove windGust as a variable
                           df_hourly <- hourly %>%
                             select(
                               time,
                               #summary,
                               temperature,
                               apparentTemperature,
                               #dewPoint,
                               contains("humidity"),
                               #pressure,
                               contains("precipIntensity"),
                               #precipProbability,
                               contains("windSpeed"),
                               #windBearing,
                               #cloudCover,
                               #uvIndex,
                               #visibility
                             ) %>%
                             mutate(day = dates[i],
                                    dayinperiod = i)
                           
                           if ("precipIntensity" %in% colnames(df_hourly)) {}
                           else {
                             df_hourly <- df_hourly %>%
                               mutate(precipIntensity = NA)}
                           
                           if ("humidity" %in% colnames(df_hourly)) {}
                           else {
                             df_hourly <- df_hourly %>%
                               mutate(humidity = NA)}
                           
                           if ("windSpeed" %in% colnames(df_hourly)) {}
                           else {
                             df_hourly <- df_hourly %>%
                               mutate(windSpeed = NA)}
                           
                           # Add this day of data to sample
                           year_data <- rbind(year_data, df_hourly)
                         }
                       }
                       
                       if (nrow(year_data) != 0) {
                         
                         # Append current year to dataframe
                         year_data$year <- year(date)
                         
                         # Add year median to the dataframe
                         year_data$median <- median(year_data$temperature)
                         
                         # Append dataframe with city
                         year_data$city <- as.character(pull(worldcities[worldcities$list == input$city1, "city"], city))
                         
                         # Aggregate years
                         darksky_data1 <- rbind(darksky_data1, year_data)
                       }
                       
                       else {
                         showNotification(paste0('Skipped year ', year(date), ' (incomplete data for city 1).'),
                                          type = 'warning', duration = 60)
                       }
                       
                       #})
                       
                     }
                     
                     #########
                     ## City 2
                     
                     if (input$numCities == 2) {
                       
                       lat2 <- worldcities[worldcities$list == input$city2, 3]
                       long2 <- worldcities[worldcities$list == input$city2, 4]
                       if (length(lat2) > 1) {lat2 <- lat2[1]} # Default to first entry if duplicate city
                       if (length(long2) > 1) {long2 <- long2[1]}
                       lat_long2 <- paste0(lat2, ",", long2)
                       
                       # Find timezone offset
                       tzcall2 <- paste0("http://api.geonames.org/timezoneJSON?formatted=true&lat=", 
                                         lat2, "&lng=", long2, "&username=bendaytoday&style=full")
                       tzdata2 <- jsonlite::fromJSON(txt = tzcall2)
                       tzoffset2 <- tzdata2$gmtOffset
                       
                       j <- 1; ifelse(j == 1, darksky_data2 <- data.frame(matrix(NA, nrow = 0, ncol = 10)),)
                       
                       
                       for (j in 1:input$numyears) {
                         
                         try({
                           
                           incProgress(0.5 / input$numyears)
                           
                           # Initialise year df
                           i <- 1; if (i == 1) {year_data <- data.frame(matrix(NA, nrow = 0, ncol = 10))}
                           
                           for (i in 1:int_days) {
                             
                             # Construct date
                             date <- ymd(input$start_date) + i
                             year(date) <- as.numeric(year(input$start_date)) - input$numyears + j
                             
                             # Call API
                             json_file2 <- paste0(url_base, api_key, paste0(lat_long2, ",", date, "T", time, url_exclusions))
                             json_data2 <- jsonlite::fromJSON(txt = json_file2)
                             
                             # --------------------
                             # If no hourly data then skip
                             if ('hourly' %in% names(json_data2)) {
                               
                               
                               # Data sources/weather station
                               src2 <- json_data2$flags$sources
                               station2 <- json_data2$flags$`nearest-station`
                               
                               # Turn data block (lists) into data frames
                               hourly <- as_tibble(json_data2$hourly$data)
                               daily <- as_tibble(json_data2$daily$data)
                               
                               # Change UNIX time to datetime
                               hourly$time <- as_datetime(as.numeric(hourly$time))
                               
                               tryCatch({
                                 
                                 # Account for timezone
                                 hour(hourly$time) <- hour(hourly$time) + floor(tzoffset2)
                                 
                               }, error = function(e){
                                 cat("Timezone adjustment didn't work.")
                               })
                               
                               
                               # Remove windGust as a variable
                               df_hourly <- hourly %>%
                                 select(
                                   time,
                                   #summary,
                                   temperature,
                                   apparentTemperature,
                                   #dewPoint,
                                   contains("humidity"),
                                   #pressure,
                                   contains("precipIntensity"),
                                   #precipProbability,
                                   contains("windSpeed"),                                   
                                   #windBearing,
                                   #cloudCover,
                                   #uvIndex,
                                   #visibility
                                 ) %>%
                                 #filter(hour(time) > 5 & hour(time) < 20) #%>%          ### hours filter
                                 mutate(day = dates[i],
                                        dayinperiod = i)
                               
                               if ("precipIntensity" %in% colnames(df_hourly)) {}
                               else {
                                 df_hourly <- df_hourly %>%
                                   mutate(precipIntensity = NA)}
                               
                               if ("humidity" %in% colnames(df_hourly)) {}
                               else {
                                 df_hourly <- df_hourly %>%
                                   mutate(humidity = NA)}
                               
                               if ("windSpeed" %in% colnames(df_hourly)) {}
                               else {
                                 df_hourly <- df_hourly %>%
                                   mutate(windSpeed = NA)}
                               
                               # Add this day of data to sample
                               year_data <- rbind(year_data, df_hourly)
                             }
                           }
                           
                           # ------------------
                           # Skip that year if there is a problem!
                           if (nrow(year_data) != 0) {
                             
                             # Append current year to dataframe
                             year_data$year <- year(date)
                             
                             # Add year median to the dataframe
                             year_data$median <- median(year_data$temperature)
                             
                             # Append dataframe with city
                             year_data$city <- as.character(pull(worldcities[worldcities$list == input$city2, "city"], city))
                             
                             # Aggregate years
                             darksky_data2 <- rbind(darksky_data2, year_data)
                             
                           }
                           
                           else {
                             showNotification(paste0('Skipped year ', year(date), ' (incomplete data for city 2).'),
                                              type = 'warning', duration = 60)
                           }
                           
                         })
                         
                       }
                       
                       darksky_data <- rbind(darksky_data1, darksky_data2)
                       
                     }
                     
                     else {darksky_data <- darksky_data1}
                     
                   })
      
      

      # Filter based on input$timeslot ------------------------------------------

      # Append parts of day
      darksky_data$timeslot <- NA
      darksky_data$timeslot[hour(darksky_data$time) %in% c(5:8)] <- "early"
      darksky_data$timeslot[hour(darksky_data$time) %in% c(9:12)] <- "morning"
      darksky_data$timeslot[hour(darksky_data$time) %in% c(13:15)] <- "afternoon"
      darksky_data$timeslot[hour(darksky_data$time) %in% c(16:19)] <- "evening"
      darksky_data$timeslot[hour(darksky_data$time) %in% c(20:23)] <- "night"
      darksky_data$timeslot[hour(darksky_data$time) %in% c(0:4)] <- "overnight"
      
      if ("Early (5-8am)" %!in% input$timeslot) {
        darksky_data <- darksky_data %>% filter(timeslot != "early")
      }
      if ("Morning (9-12pm)" %!in% input$timeslot) {
        darksky_data <- darksky_data %>% filter(timeslot != "morning")
      }
      if ("Afternoon (1-3pm)" %!in% input$timeslot) {
        darksky_data <- darksky_data %>% filter(timeslot != "afternoon")
      }
      if ("Evening (4-7pm)" %!in% input$timeslot) {
        darksky_data <- darksky_data %>% filter(timeslot != "evening")
      }
      if ("Night (8-12am)" %!in% input$timeslot) {
        darksky_data <- darksky_data %>% filter(timeslot != "night")
      }
      if ("Overnight (1-4am)" %!in% input$timeslot) {
        darksky_data <- darksky_data %>% filter(timeslot != "overnight")
      }
      


      # Return datasets ---------------------------------------------------------

      if (input$numCities == 2) {
        out <- list(darksky_data = darksky_data,
                    src1 = src1,
                    src2 = src2,
                    station1 = station1,
                    station2 = station2,
                    lat = lat,
                    long = long,
                    lat2 = lat2,
                    long2 = long2)
      }
      else {
        out <- list(darksky_data = darksky_data,
                    src1 = src1,
                    station1 = station1,
                    lat = lat,
                    long = long)
      }
      
      return(out)
      
    }
  })

  # Parse processed data to reactive plot object ----------------------------
  rplot <- reactive({
    
    if (!is.null(data())){
    
      out = data()
      darksky_data <- as.data.frame(out$darksky_data)
      
      # Plot temperature
      d <- darksky_data %>%
        mutate(apparentTemperature = as.numeric(apparentTemperature),
               city = as.factor(city),
               year = as.factor(year(time))) %>%
        group_by(year, city, dayinperiod) %>%
        summarise(maxaTemp = max(apparentTemperature)) %>%
        group_by(city, dayinperiod) %>%
        mutate(city_median = median(maxaTemp)) %>%
        rename('Max feels like Temp' = maxaTemp,
               'Day in period' = dayinperiod)
      
      d <- d %>% ggplot(.) +
        geom_point(aes(x = `Day in period`, y = `Max feels like Temp`, 
                       colour = city, shape = year), size = 2.75) +
        geom_line(aes(x = `Day in period`, y = city_median, 
                      colour = city), linetype = "dashed", size = 1.5) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank()) + 
        scale_size_continuous(range = c(2,10)) +
        scale_x_discrete(limits = as.character(unique(data()$darksky_data$day))) +
        scale_y_continuous(limits = c(0, round_any(max(as.numeric(data()$darksky_data$apparentTemperature)), 10, ceiling)), 
                           breaks = seq(0,50,5)) +
        theme(text = element_text(size = 16)) +
        xlab('Day in period') + ylab('Apparent Temperature °C') +
        guides(size = FALSE) +
        scale_colour_discrete("City") +
        scale_shape_discrete("Year")
      
      d <- ggplotly(d) %>% layout(legend = list(orientation = "h", y = -0.3))
      d
      
      # Tried to neaten the tooltip, but can't separate years/cities easily
      # d %>% style(text = paste0("Day:", d$x$data[[2]]$x, 
      #                         "</br></br>", 
      #                         "Max feels like temp:", d$x$data[[2]]$y))
      
    }
    
  })
  
  output$tempPlot <- renderPlotly(
    if (!is.null(rplot())) rplot()
  )
  
  # Summary data table -----------------------------------------------------
  output$dataTable <- DT::renderDataTable({
      
    if (!is.null(data())) {
    
      darksky_data <- data()$darksky_data
      
      darksky_data %>%
        mutate(humidity = 100 * humidity,
               temperature = round(temperature, 1),
               apparentTemperature = round(apparentTemperature, 1),
               year = year(time),
               dayz = day(time)) %>%
        group_by(year, city) %>%
        summarise(atempMin = min(apparentTemperature, na.rm = T),
                  atempMax = max(apparentTemperature, na.rm = T),
                  tempMin = min(temperature, na.rm = T),
                  tempMax = max(temperature, na.rm = T),
                  humidMin = ceiling(min(humidity, na.rm = T)),
                  humidMax = ceiling(max(humidity, na.rm = T)),
                  over30 = (n_distinct(dayz[apparentTemperature >= 30])/n_distinct(dayz))*100,
                  over35 = (n_distinct(dayz[apparentTemperature >= 35])/n_distinct(dayz))*100,
                  over40 = (n_distinct(dayz[apparentTemperature >= 40])/n_distinct(dayz))*100,
                  raindays = (n_distinct(dayz[precipIntensity > 0.1])/n_distinct(dayz))*100,
                  rainfall = sum(precipIntensity, na.rm = T),
                  wind = median(windSpeed)
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
               '% Days rained' = raindays,
               'Wind speed avg (kph)' = wind
        ) %>%
        mutate_if(is.numeric, round, 1) %>%
        arrange(City) %>%
        select(City, Year, input$show_vars)
    }
    
  }, options = list(dom  = '<"top">t<"bottom">',
                    searching = F), );
  #});
  
  
  # Weather stations --------------------------------------------------------
  output$stationText <- renderText({

    if (!is.null(data())) {
      
      out = data()
      station1 <- out$station1
      
      if (input$city2 == 2) {
        station2 <- out$station2
        HTML(paste0("Nearest weather stations: ", station1, "kms, ", station2, "kms"))
      }
      else HTML(paste0("Nearest weather stations: ", station1, "kms"))
      
      }
    })
  
  # Data sources ------------------------------------------------------------
  url <- a("DarkSky API", href = "https://darksky.net/dev/docs/sources")

  output$tab <- renderUI({
    tagList(url, " data sources:")
  })
  
  output$srcText <- renderText({
    
    if (!is.null(data())) {
      
      out = data()
      src1 <- out$src1
      
      if (input$numCities == 2) {
        src2 <- out$src2
        # Combine character lists and show uniques
        srcs <- as.character(c(src1, src2)) %>% unique()
      }
      else {
        srcs <- as.character(src1) %>% unique()
      }
      
      HTML(paste(srcs, sep = ' '))
      
    }
    
  })
  

  # Show lat/lon ------------------------------------------------------------
  output$latlonText <- renderText({
    
    if (!is.null(data())) {
      
      out = data()
      lat = out$lat
      long = out$long
      
      if (input$numCities == 2) {
        
        lat2 = out$lat2
        long2 = out$long2
        
        # Combine character lists and show uniques
        srcs <- paste0('Lat1: ', lat, ', Long1: ', long,
                       ' and Lat2: ', lat2, ', Long2: ', long2)
      }
      else {
        srcs <- paste0('Lat: ', lat, ', Long: ', long)
      }
      
      HTML(srcs)
      
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      
      if (input$numCities == 2) {
        paste(input$city1, "+", input$city2, ".csv", sep = "")
      }
      else {
        paste(input$city1, ".csv", sep = "")}
    },
    content = function(file) {
      write.csv(data()$darksky_data, file, row.names = FALSE)
    }
  )
  
  # Downloadable list of countries ----
  output$downloadList <- downloadHandler(
    filename = "countrylist.csv",
    content = function(file) {
      write.csv(worldcities, file, row.names = FALSE)
    }
  )
  
}