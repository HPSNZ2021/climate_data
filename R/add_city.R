# Function to add a new city to worldcities.rds
# Ben Day
# Created 2020/01/31
# Modified 2021/03/25

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
#add_city("xxxx", 12.3456, -12.345, "Yyyy")

# Manually add multiple cities --------------------------------------------

rows <- tribble(~city, ~city_ascii, ~lat, ~lng, ~country,
                #'Alpensia Sports Park', 'Alpensia Sports Park', 37.657, 128.660, 'South Korea',
                #'RusSki Gorki Ski Jumping Center', 'RusSki Gorki Ski Jumping Center', 43.680, 40.262, 'Russia',
                #'Whistler Olympic Park', 'Whistler Olympic Park', 50.141, -123.124, 'Canada',
                #'Sestriere Borgata', 'Sestriere Borgata', 44.958, 6.867, 'Italy',
                #'Park City Mountain', 'Park City Mountain', 40.650, -111.518, 'United States'
                'Taiwu Ski Resort', 'Taiwu Ski Resort', 40.8917279, 115.4431891, 'China'
    ) %>%
  mutate(iso2 = "",
         iso3 = "",
         admin_name = "",
         capital = "",
         population = "",
         id = ""
      )

rows <- rows %>%
  mutate(list = paste0(city_ascii, "  -  ", country))

worldcities <- worldcities %>%
  bind_rows(rows)

saveRDS(worldcities, file = "R/worldcities.rds")
write.csv(worldcities, "data/worldcities.csv")
