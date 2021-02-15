## Function to add a new city to worldcities.rds
## Ben Day
## 2020/01/31

add_city <- function(city_name, latitude, longitude, cntry){
  
  library(tidyverse)
  
  setwd('..')
  
  worldcities <- as.tibble(read.csv("data/worldcities.csv"))
  worldcities <- worldcities[,2:12]
  worldcities <- worldcities %>% 
    add_row(
      city = city_name,
      city_ascii = city_name,
      lat = latitude,
      lng = longitude,
      country = cntry,
      iso2 = "",
      iso3= "",
      admin_name = "",
      capital = "",
      population = "",
      id = ""
    )
  
  # Lat and long from https://simplemaps.com/data/world-cities
 
  worldcities <- worldcities %>%
    mutate(list = paste0(city_ascii, "  -  ", country))
  
  #worldcities[1, ] <- ""
  
  saveRDS(worldcities, file = "R/worldcities.rds")
  write.csv(worldcities, "data/worldcities.csv")
  
  setwd('R/')
  
}



#add_city("Sunshine Coast", -26.6501569, 153.0558082, "Australia")
#add_city("Komatsu", 36.367952, 136.4327591, "Japan")
#add_city("Banyoles", 42.1168503,2.7488544, "Spain")
