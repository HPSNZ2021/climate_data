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

library(tidyverse)
library(RColorBrewer)

temps <- c(seq(-40, 15, 1))
winds <- c(seq(0, 50, 10))
df <- data.frame(matrix(data = NA, nrow = 0, ncol = 0))

for (i in 1:length(winds)) {
  rows <- tibble(wind = rep(winds[i], length(temps)),
                 temp = temps)
  df <- df %>% bind_rows(., rows)
}

df <- df %>% mutate(
  windchill = map2_dbl(.x = temp, .y = wind, .f = windchill),
  wind = factor(wind)
) %>%
  filter(temp != 10)

g <- ggplot(df) + geom_line(aes(x = temp, y = windchill, color = wind), size = 2) + 
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 'dotted', colour = 'grey', size = 1) +
  labs(title = "Wind chill temperature equation vs wind speed (kph)"
       #subtitle = "Temperature high and low over period",
       #caption = "Games years denoted by points. Lines show year high/low"
       ) +
  ylab('Wind Chill Temperature (°C)') + xlab('Temperature (°C)') +
  scale_color_brewer(palette = 'Blues')
g

ggsave(filename = 'windchill.png', device = 'png', plot = g, 
       width = 10, height = 6)
