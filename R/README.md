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
1. **Phase 1 - December 2019** - Proof of concept, introduction to the [*Dark Sky API*](https://darksky.net/dev/docs/faq)
1. *December 2019* - HPSNZ Intelligence/Innovation/PTA following the 'holy trinity' forum at Castaways
2. *January 2020* - Cycling NZ in initial Intelligence connect with CNZ PTA/HPAD/Coaching/Operations
3. *September 2020* - App circulated to wide group of HPSNZ and externals
4. *October 2020* - Ongoing use by the HPSNZ Physiology team
5. **Phase 2 - March 2020** - Andy Kilding (Head of Physiology) requests new features to support *cold investigation for the 2022 Winter Games*
6. *March 2020 to present* - Ben working on implementing COLD & HEAT user modes for all-in-one web app

*****

## Notes 2021-03-10

Attendees: Ben, Chris, Andy

**MOVING INTO PHASE 2 OF PROJECT**

Brief meeting to discuss potential of integrating some cold weather features into the existing weather app. This would help support the upcoming Winter Games in 2022 and be a great tool for the soon-to-be-appointed 'cold guru'.

MINUTES
- Cold weather specialist to join physiology team soon
- Existing app is a great tool for heat-specific venue information
- Can we in some way do the same for cold-specific venue information?
- Ben's initial thoughts are yes: build more features into the existing app and utilise same data workflow

NEXT STEPS
1. Ben to first investigte information we could get from the Dark Sky API for prior Winter Games venues
2. Present these findings in a report
3. Begin build of these features into existing HPSNZ Weather App