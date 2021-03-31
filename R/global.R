library(shiny)
library(rjson)
library(purrr)
library(repurrrsive)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggthemr)
library(plotly)

# Example API call
#'https://api.darksky.net/forecast/0892828c9d27ce19b523d667698ac088/-12.4257239,130.863097,2018-07-23T15:00:00'

# Rounding function
round_any = function(x, accuracy, f = round) {f(x / accuracy) * accuracy}

#'Not in' function
`%!in%` = Negate(`%in%`)

# Add city function
add_city <- function(city_name, latitude, longitude, cntry){
  #add_city("Banyoles", 42.1168503,2.7488544, "Spain")
  
  # Load dataset
  worldcities <- as_tibble(readRDS(file = "worldcities.rds"))
  
  worldcities <- worldcities %>% 
    add_row(
      city = city_name,
      city_ascii = city_name,
      lat = latitude,
      lng = longitude,
      country = cntry,
      iso2 = "",
      iso3 = "",
      admin_name = "",
      capital = "",
      population = "",
      id = ""
    )

  worldcities <- worldcities %>%
    mutate(list = paste0(city_ascii, "  -  ", country))
  
  saveRDS(worldcities, file = "worldcities.rds")
  
}

# Windchill function
windchill <- Vectorize(function(temp, wind_kph) {
  
  if (!is.numeric(temp)) {
    warning('Temperature input not a number')
    return(NA)
  }
  if (!is.numeric(wind_kph) | is.null(wind_kph)) {
    warning('Wind input not a number')
    return(NA)
  }
  
  if (!is.na(temp) & !is.na(wind_kph)) {
    
    # Windchill defined only for wind above 4.8kph and temps below 10C
    if (wind_kph <= 4.8 | temp >= 10) {
      return(temp)
    }
    # Otherwise use formula
    else if (wind_kph > 0) {
      wc <- 13.12 + 0.6215 * temp - 11.37 * (wind_kph ^ 0.16) + 0.3965 * temp * (wind_kph ^ 0.16)
      return(wc)
    }
    
  }
  
  else return(NA)
}
)