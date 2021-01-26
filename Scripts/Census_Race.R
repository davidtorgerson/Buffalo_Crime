library(tidyverse)
library(tidycensus)
library(ggplot2)
library(lubridate)
library(stringr)


get_race = function() {
  census_api_key("f772229772e065b3798947c88d409d9c2f4f1afe")
  
  race_vars = c("B02001_002",
                "B02001_003",
                "B02001_005",
                "B03001_003")
  
  out = get_acs(
    geography = "tract",
    state = "NY",
    variables = race_vars,
    year = 2018,
    survey = "acs5",
    geometry = FALSE,
    county = 'Erie'
  ) %>%
    select(GEOID, variable, estimate) %>%
    spread(variable, estimate) %>%
    rename(WHITE = B02001_002,
           AFRICAN_AMER = B02001_003,
           ASIAN = B02001_005,
           HISPANIC = B03001_003)
  
  return(out)
}

get_race()
