# climate_data
Building a tool to display venue climate data from the DarkSky weather API

## Purpose
Travelling to international venues is a natural part of elite sport. An enduring question in relation to the planning of an international trip for competition or training is "_what will the weather be like in X at Y time of year?_"
With the DarkSky API we are able to answer this question quickly and accurately.

## Inputs
Simple selection buttons are used on a Shiny web app UI
- Date range
- City
- Number of years of past data

## Outputs
Hourly observations are plotted and tabularised for user

Plot shows hourly observations over that date range. Table shows:
- Temperature (min and max, by year)
- Humidity (min and max, by year)

## Potential improvements
1. Data export feature
2. "Nearest station" output to describe proximity of nearest weather station (multiple may be used in data output).

## Implementation
1. HPSNZ Intelligence/Innovation/PTA in *December 2019* following the 'holy trinity' forum at Castaways
2. Cycling NZ in *January 2020* in initial Intelligence connect with CNZ PTA/HPAD/Coaching/Operations
