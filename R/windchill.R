# Function to calculate wind chill from temperature and wind speed --------
# Ben Day
# 2021/03/25
# Based on UK formula found here
# https://en.wikipedia.org/wiki/Wind_chill#/media/File:Windchill_effect_en.svg

windchill <- Vectorize(function(temp, wind_kph) {
  
  if (!is.numeric(temp)) {
    warning('Temperature input not a number')
    return(NA)
    }
  if (!is.numeric(wind_kph)) {
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
