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

# Load dataset
worldcities <- as_tibble(readRDS(file = "worldcities.rds"))