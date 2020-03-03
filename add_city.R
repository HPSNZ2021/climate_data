## Function to add a new city to worldcities.rds
## Ben Day
## 2020/01/31

add_city <- function(city_name, latitude, longitude, cntry){
  
  library(tidyverse)
  
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
  
  saveRDS(worldcities, file = "worldcities.rds")
  write.csv(worldcities, "data/worldcities.csv")
  
}



add_city("Orlando", 30.4481075,-84.3031829, "United States")
