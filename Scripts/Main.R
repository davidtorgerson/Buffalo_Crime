library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidycensus)
library(stringr)

######################### Reading in the Data ####################

crime = read_csv("Data/Crime_Incidents.csv")

######################## Cleaning the Data #######################

str(crime)

#Filling in the Missing police districts by zip
district_a = c(14203,14204,14206,14210,14220)
district_b = c(14201,14202,14208,14209,14213,14222)
district_c = c(14205,14212)
district_d = c(14207,14214,14216)
district_e = c(14211,14215)
#Districts share some zip codes - those are only attributed to one district
#zip codes obtained from 211wny.org

crime_clean = crime %>%
  mutate(address_1 = toupper(address_1)) %>%
  mutate(incident_datetime = mdy_hms(incident_datetime)) %>%
  mutate(incident_type_primary = toupper(incident_type_primary)) %>%
  mutate(day_of_week = str_to_sentence(day_of_week)) %>%
  mutate(new_police_district = case_when(
           zip %in% district_a ~ 'District A',
           zip %in% district_b ~ 'District B',
           zip %in% district_c ~ 'District C',
           zip %in% district_d ~ 'District D',
           zip %in% district_e ~ 'District E',
           is.na(zip) ~ `Police District 1`
         ))

str(crime_clean)

########################### Exploratory Data Analysis ######################

#Plotting trend of crime counts
crime_clean %>%
  ggplot() + 
  geom_freqpoly(aes(x = incident_datetime, color = incident_type_primary)) #Shows overall trend of crime

crime_clean %>%
  filter(year(incident_datetime) >= '2009') %>% #When data is listed to start at
  ggplot() +
  facet_wrap(~incident_type_primary, scales = "free_y") +
  geom_freqpoly(aes(x = incident_datetime, color = incident_type_primary)) #Shows trends of individual crimes

#Filtering data to start at year of regular collection
new_crime = crime_clean %>%
  filter(year(incident_datetime) >= '2009')

#Exploratory graphs to understand the data
new_crime %>%
  ggplot() + geom_bar(aes(x = year(incident_datetime)), fill = 'lightblue') #Shows total crime is steadily decreasing

new_crime %>%
  ggplot() + geom_bar(aes(x = month(incident_datetime)), fill = 'orange') #Crime spikes in July/August

new_crime %>%
  ggplot() + geom_bar(aes(x = day_of_week), fill = 'maroon') #Not much noticeable difference for day of week

new_crime %>%
  ggplot() + geom_bar(aes(x = incident), fill = 'purple') +
  coord_flip() #This shows the top crimes are: Larceny/Theft, Assault, Robbery, Burglary, UUV

#Collapsing crimes 
new_crime = new_crime %>%
  mutate(incident = case_when(
    incident_type_primary %in% c('THEFT OF SERVICES','THEFT OF VEHICLE') ~ 'THEFT',
    incident_type_primary %in% c('AGG ASSAULT ON P/OFFICER', 'AGGR ASSAULT') ~ 'ASSAULT',
    TRUE ~ incident_type_primary
  ))

############## Aggregating data for model building ###################

#Crimes by day and type
daily_incidents = new_crime %>%
  mutate(incident_date = as.Date(incident_datetime)) %>%
  count(incident_date, incident) %>%
  arrange(incident, incident_date)

daily_incidents %>%
  ggplot() + 
  geom_line(aes(x = incident_date, y = n, color = incident)) +
  facet_wrap(~ incident, scales = "free_y")

##################### Forecasting Model Building ##################

# 7 day Horizon Forecast







