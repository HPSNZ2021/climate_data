## Function to add a new city to worldcities.rds
## Ben Day
## Created 2020/01/31
## Modified 2021/02/17

add_city <- function(city_name, latitude, longitude, cntry){
  
  library(tidyverse)
  
  wd <- getwd()
  
  # Determine most recent data source
  rds <- file.info('worldcities.rds')$mtime
  setwd('..')
  csv <- file.info('data/worldcities.csv')$mtime
  
  if (rds >= csv) {
    setwd(paste0(wd, '/R'))
    worldcities <- readRDS('worldcities.rds')
  }
  else if (csv > rds) {
    worldcities <- as.tibble(read.csv("data/worldcities.csv"))
    worldcities <- worldcities[,2:12]
    setwd(paste0(wd, '/R'))
  }
  
  # Adapt most recent dataset
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
  
  # Lat and long from https://simplemaps.com/data/world-cities
 
  worldcities <- worldcities %>%
    mutate(list = paste0(city_ascii, "  -  ", country))
  
  # Save new data sources
  setwd(wd)
  saveRDS(worldcities, file = "R/worldcities.rds")
  write.csv(worldcities, "data/worldcities.csv")
  
  # Return to wd
  setwd(paste0(wd, '/R'))
  
}


#add_city("Sunshine Coast", -26.6501569, 153.0558082, "Australia")
#add_city("Komatsu", 36.367952, 136.4327591, "Japan")
#add_city("Banyoles", 42.1168503,2.7488544, "Spain")
