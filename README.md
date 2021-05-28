# HPSNZ Weather App

*What is the weather like at training venue X at this time of year?* 

This type of question is commonly asked of (particularly) the Performance Physiology team. While it is straightforward to gather current or forecasted weather information for a specific city, acquiring this information historically, and in consistent ways, was an ongoing challenge.

After Ben provided the data analysis for the Tokyo 2020 Environmental Report for NSOs, it followed that HPSNZ needed a reliable and repeatable method of acquiring this type of information about training and competition venues.

Chris Rawlings discovered the [**Dark Sky API**](https://darksky.net/dev) and Intelligence thereafter tested and validated the output for various Olympic venues (which we had weather station measurements from). This then was programmed into functions written in R, whereby an R Shiny web application could proide an interface for users to find historical weather information for the venue and time period of choice.

The resulting tool is available to HPSNZ and NSO staff through sign-up to [www.shinyapps.io] and is positioned to offer flexible outputs in the form of data exports and visualisations.

### Project Timeline

1. **Jan - Mar 2020** - **Phase 1**: leverage the **Dark Sky API**; build R Shiny web app to query historical weather data for competition venues
2. **Aug 2020** - deploy v1 [HPSNZ Weather App](https://hpsnz.shinyapps.io/climate_data/)
3. **Mar - May 2021** - **Phase 2**: enhance existing app to support **heat/cold modes**; improve functionality
4. **May 5 - May 20 2021**: deploy Beta app to test new features
5. **May 21 2021**: deploy v2 [HPSNZ Weather App](https://hpsnz.shinyapps.io/climate_data/)

******

## 2021

## Version 2 features

Once the v2 Shiny app was deployed it was decided to communicate this to the wider Physiology team through the Andy Kilding, the Head of Discipline. The following features were pointed out:

+ **'Heat mode'** and **'Cold mode'** for venue weather
+ Various chart views for better user control
  - Includes **wind chill temperature, temperature only,** and **wind only** metrics
  - Includes hour-by-hour view
+ Ability to add new cities to list

Finally, Ben gave a warning about some missing years' data for some Beijing venues. "*Unfortunately this happened for Tokyo venues too; our best workaround is to look back on more years' data to understand weather trends.*"



## Meeting Notes 2021-03-11

Andy requested new features in the existing HPSNZ Weather App to accomodate work for the upcoming Winter Games in 2022. In a similar way that the Physiology grew capacity for **heat preparation for Tokyo 2020**, they are looking to focus on **cold preparation for 2022**.

Attendees: Ben, Chris, Andy

A short meeting to understand the request and imagine what design additions may be needed. Over the weeks to come Ben worked with Andy to test various data views and options to best provide for the needs of the Physiology team. This involved a beta web app (R Shiny) that worked well through the testing phase.

Andy was then asked to communicate that **version 2** of the app is now available. 

******

## 2020

## Using v1 HPSNZ Weather App

On 1 September 2020 Ben Day notified a wide group of HPSNZ staff across PTA, Physiology, Strength & Conditioning, Goldmine, and Intelligence of the readiness of the v1 Weather App. This was communicated by email (below).

Following this there was signficiant interest in the app. **Consequently there have been 18 individuals who signed up to [shinyapps.io](shinyapps.io) to use the app. These details are stored within a spreadsheet named *Shiny Users.xlsx*.**

> Kia ora koutou katoa
> 
> Please find below links to two web applications we have been developing for our teams.
> 
> Their purpose is to provide easy access to information for campaign planning, with a focus on 1) weather conditions at training/competition venues, and 2) international travel restrictions. They provide shortcuts to information that may accelerate decision making.
> 
> It is easy to opt-in for access to these. Reply to this email to be added to the list, at which point you will need to do a short signup step (1 time only for both apps). You are welcome to pass this on if you feel it would be of benefit to others e.g. in sports you work with.