library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidycensus)
library(stringr)
library(tibble)
library(purrr)
library(rsample)


######################### Reading in the Data ####################
#This Analysis is of crime data in Buffalo, NY
#The data used in this analysis can be found here:
#https://data.buffalony.gov/Public-Safety/Crime-Incidents/d6g9-xbgu

crime = read_csv("Data/Crime_Incidents.csv")


######################## Cleaning the Data #######################

str(crime)

#Filling in the Missing police districts by zip
district_a = c(14203,14204,14206,14210,14220)
district_b = c(14201,14202,14208,14209,14213,14222)
district_c = c(14205,14212)
district_d = c(14207,14214,14216)
district_e = c(14211,14215)
#Districts share some zip codes - those codes are only attributed to one district
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
crime_since_2009 = crime_clean %>%
  filter(year(incident_datetime) >= '2009') %>%
  mutate(incident = case_when(
    incident_type_primary %in% c('THEFT OF SERVICES','THEFT OF VEHICLE') ~ 'THEFT', #Aggregating crimes
    incident_type_primary %in% c('AGG ASSAULT ON P/OFFICER', 'AGGR ASSAULT') ~ 'ASSAULT', #Aggregating crimes
    TRUE ~ incident_type_primary)) %>%
  filter(!incident %in% c('BREAKING & ENTERING','HOMICIDE','CRIM NEGLIGENT HOMICIDE',
                       'MANSLAUGHTER','OTHER SEXUAL OFFENSE','SEXUAL ASSAULT')) #Removing crimes with low volume (< 100)

#Verifying Incidents were removed
crime_since_2009 %>%
  count(incident)

#Exploratory graphs to understand the filtered data
crime_since_2009 %>%
  ggplot() + geom_bar(aes(x = year(incident_datetime)), fill = 'lightblue') #Shows total crime is steadily decreasing

crime_since_2009 %>%
  ggplot() + geom_bar(aes(x = month(incident_datetime)), fill = 'orange') #Crime spikes in July/August

crime_since_2009 %>%
  ggplot() + geom_bar(aes(x = day_of_week), fill = 'maroon') #Not much noticeable difference for day of week

crime_since_2009 %>%
  ggplot() + geom_bar(aes(x = incident), fill = 'purple') +
  coord_flip() #This shows the top crimes are: Larceny/Theft, Assault, Robbery, Burglary, UUV


############## Aggregating data for model building ###################

#Crimes by day and type
daily_incident_counts = crime_since_2009 %>%
  mutate(incident_date = as.Date(incident_datetime)) %>%
  count(incident_date, incident) %>%
  arrange(incident, incident_date)

daily_incident_counts %>%
  ggplot() + 
  geom_line(aes(x = incident_date, y = n, color = incident)) +
  facet_wrap(~ incident, scales = "free_y")


##################### Forecasting Model Building ##################

##Prepping the data and features
#Making sure that there are no skips in dates
seq_dates = seq.Date(
  from = as.Date("2009-01-01"),
  to = as.Date("2021-01-24"),
  by = 1) %>%
  enframe(name = NULL, value = "incident_date")

all_daily_incidents <- daily_incident_counts %>%
  split(.$incident) %>%
  map(., function(i) {
    left_join(
      x = seq_dates,
      y = i,
      by = "incident_date"
    ) %>%
      fill(incident, .direction = "downup") %>%
      replace_na(list(n = 0))
  }) %>%
  bind_rows()
#The code above splits the dataframe by incidents map the dates together
#Filling in all the missing dates with a count of 0
#Assumption: If date is missing from original data, that incident did not occur (count of 0)

#Extracting the other date-based data features
complete_daily_incidents = all_daily_incidents %>%
  mutate(month = month(incident_date)) %>%
  mutate(year = year(incident_date)) %>%
  mutate(week = week(incident_date)) %>%
  mutate(weekday = wday(incident_date)) %>%
  rename(crime_count = n)

##Creating Testing Data sets
#Using last 5 years for training data
recent_daily_incidents = complete_daily_incidents %>%
  filter(incident_date >= '2015-01-01')

# --- Is this part still necessary? Or should I still follow through with this path? ---

# #Split training data by crime
# crime_splits = split(recent_daily_incidents, recent_daily_incidents$incident)
# #The code above splits the data frame into multiple tibbles separated by incident
# 
# #Applying validation design to each crime individually
# rocv_by_crime = map2(crime_splits, names(crime_splits), function(df, nm){
#   train_test_index = rolling_origin(
#     data = df, #Using recent data from 2015 and later
#     initial = (365*4), #Using four years for the initial training sample
#     assess = 14, #Forecast Horizon - how far in the future do I want to predict. 14 days because it allows for staffing adjustments/coverage and still maintains accuracy which you begin to lose the further out your predicting
#     cumulative = TRUE,
#     skip = 14 #How large is the window moving forward after each split. IDK why 14 days, does it matter how big the window is?
#   ) %>%
#     mutate(Iteration = row_number())
# })
# #We use map2 here since we have two arguments, the splits and the name of those splits.
# #Those are then used in the function following to go through each iteration (crime)
# 
# #Combining within-crime ROCV and then split by iteration
# rocv_by_iteration = rocv_by_crime %>%
#   bind_rows() %>%
#   split(.$Iteration)
# ------------------------------------------------------------------------

#################### ANOTHER WAY TO APPLY ROCV for Train/Test Data ######################
z_scale = function(x) {
  (x - mean(x))/sd(x) #Creating a function that turns counts in z scores
}

recent_daily_incidents_wide = recent_daily_incidents %>%
  spread(incident, crime_count) %>% #This de-duplicates the incident dates
  mutate_at(vars(ASSAULT:UUV), list(z_scale)) #Turning crime counts into z scores to be able to compare them to each other

rocv_by_crime2 = recent_daily_incidents_wide %>%
  rolling_origin(
    data = ., #Using recent data from 2015 and later
    initial = (365*4), #Using four years for the initial training sample
    assess = 14, #Forecast Horizon - how far in the future do I want to predict. 14 days because it allows for staffing adjustments/coverage and still maintains accuracy which you begin to lose the further out your predicting
    cumulative = TRUE,
    skip = 14 #How large is the window moving forward after each split. IDK why 14 days, does it matter how big the window is?
  )

train_test_splits = map(rocv_by_crime2$splits, function(split){
  train = analysis(split) %>% #Using analysis will create our training data
    gather(incident, crime_count, -incident_date, -month, -year, -week, -weekday) #Converting data back to long format without the date features

  test = assessment(split) %>% #Using assessment will create our testing data
    gather(incident, crime_count, -incident_date, -month, -year, -week, -weekday) #Converting data back to long format without the date features
  
  out = list(train = train, test = test)
  
  return(out)
})

train_test_splits[1] #Pulling out the first set of training and testing data

################## END Creating Train/Test Data #################