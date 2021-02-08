library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidycensus)
library(stringr)
library(tibble)
library(purrr)
library(rsample)
library(tis)
library(prophet)

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

bank_holidays = holidays(seq(2009,2021, 1)) %>%
  enframe(name = 'Holiday', value = 'Date') %>%
  mutate(New_Date = as.Date(as.character(Date), '%Y%m%d'))

complete_incidents_with_holidays = left_join(complete_daily_incidents, bank_holidays, by = c("incident_date" = "New_Date")) %>%
  select(-Date)
  
##Creating Testing Data sets
#Using last 5 years for training data
recent_daily_incidents = complete_daily_incidents %>%
  filter(incident_date >= '2015-01-01')

#################### ANOTHER WAY TO APPLY ROCV for Train/Test Data ######################
z_scale = function(x) {
  (x - mean(x))/sd(x) #Creating a function that turns counts in z scores
}

recent_daily_incidents_wide = recent_daily_incidents %>%
  spread(incident, crime_count) #This de-duplicates the incident dates
  #mutate_at(vars(ASSAULT:UUV), list(z_scale)) #Turning crime counts into z scores to be able to compare them to each other

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

#Creating the training and testing data this way allows for the rolling origin function
#To be applied to each crime at a specific date rather than going through the entire
#Long data. The ensures dates are not duplicated and all crimes are represented.
#This is a more efficient and effective way to create our train/test data.

################## END Creating Train/Test Data #################

####################################################
################# Building Model ###################
####################################################

library(h2o)

h2o.init()

sample_rocv = train_test_splits[[1]]

################# General Linear Model #####################
map(sample_rocv, function(x){
  
  train = sample_rocv$train %>%
    mutate(incident = factor(incident)) #Converting crimes to categories
  
  test = sample_rocv$test %>%
    mutate(incident = factor(incident)) #Converting crimes to categories
  
  features = train %>%
    select(-crime_count, -incident_date) %>%
    colnames()
  
  target = "crime_count"
  
  model = h2o.glm(
    x = features,
    y = target,
    training_frame = as.h2o(train),
    validation_frame = as.h2o(test),
    lambda_search = TRUE
  )
  
  #Performance Metrics
  performance_metrics = h2o.performance(model, valid = TRUE) #}) This was to look at regression metrics
  
  performance_metrics_tbl = tibble(
    RMSE = performance_metrics@metrics$RMSE,
    R2 = performance_metrics@metrics$r2
  )
  
  #Variable Importance
  h2o.varimp(model)
})

###################### Gradient Boosted Machine (GBM) ###################
map(sample_rocv, function(x){
  
  train = sample_rocv$train %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  test = sample_rocv$test %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  features = train %>%
    select(-crime_count, -incident_date) %>%
    colnames()
  
  target = "crime_count"
  
  model = h2o.gbm(
    x = features,
    y = target,
    training_frame = as.h2o(train),
    validation_frame = as.h2o(test),
    ntrees = 50, #How many trees do you want the model to build, Default set at 50
    learn_rate = 0.1, #Default set at 0.1
    max_depth = 5, #Default set at 5
    categorical_encoding = "AUTO", #Default set at AUTO
    distribution = "poisson" #Used for non-negative count data
  )
  
  #Performance Metrics
  performance_metrics = h2o.performance(model, valid = TRUE)
  
  performance_metrics_tbl = tibble(
    RMSE = performance_metrics@metrics$RMSE,
    R2 = performance_metrics@metrics$r2
  ) #})
  
  #Variable Importance
  h2o.varimp(model)
  
  predictions = h2o.predict(model, newdata = as.h2o(test)) %>%
    as.vector()
  
  test %>%
    mutate(Predictions = predictions)
})

################## Random Forest Model ################
map(sample_rocv, function(x){
  
  train = sample_rocv$train %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  test = sample_rocv$test %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  features = train %>%
    select(-crime_count, -incident_date) %>%
    colnames()
  
  target = "crime_count"
  
  model = h2o.randomForest(
    x = features,
    y = target,
    training_frame = as.h2o(train),
    validation_frame = as.h2o(test),
    ntrees = 50, #How many trees do you want the model to build, Default set at 50
    max_depth = 20, #Default set at 20
    categorical_encoding = "AUTO", #Default set at AUTO
  ) 

  #Performance Metrics
  performance_metrics = h2o.performance(model, valid = TRUE)
  
  performance_metrics_tbl = tibble(
    RMSE = performance_metrics@metrics$RMSE,
    R2 = performance_metrics@metrics$r2
  ) #})
  
  #Variable Importance
  h2o.varimp(model)
  
  predictions = h2o.predict(model, newdata = as.h2o(test)) %>%
    as.vector()
  
  test %>%
    mutate(Predictions = predictions)
})

##### GBM has a lower RMSE so we will move forward with that model

############# Model Implementation ##############
rocv_models = map(train_test_splits, function(x){
  
  train = x$train %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  test = x$test %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  features = train %>%
    select(-crime_count, -incident_date) %>%
    colnames()
  
  train_h2o = as.h2o(train)
  
  test_h2o = as.h2o(test)
  
  target = "crime_count"
  
  model = h2o.gbm(
    x = features,
    y = target,
    training_frame = train_h2o,
    validation_frame = test_h2o,
    ntrees = 50, #How many trees do you want the model to build, Default set at 50
    learn_rate = 0.1, #Default set at 0.1
    max_depth = 5, #Default set at 5
    categorical_encoding = "AUTO", #Default set at AUTO
    distribution = "poisson" #Used for non-negative count data
  )
  
  #Simple Predictions Calculation
  simple_model = train %>%
    arrange(incident, incident_date) %>% #Sorting by incident and date
    group_by(incident) %>%
    slice(tail(row_number(), 14)) %>% #Grab the last 14 rows of each incident
    ungroup() %>%
    mutate(incident_date = incident_date + days(14)) %>% #Adding 14 days to merge with testing data
    select(incident_date, incident, Simple = crime_count)
  
  #Predictions from GBM Model
  predictions = h2o.predict(model, newdata = test_h2o) %>%
    as.vector()
  
  #MASE Calculation
  MASE_by_incident = test %>%
    mutate(Predictions = predictions) %>% #Adding predictions from gbm 
    inner_join(simple_model, by = c("incident","incident_date")) %>% #adding simple predictions
    group_by(incident) %>%
    summarise(
      GBM_MAE = mean(abs(Predictions - crime_count)), #Getting Mean Average Error for GBM
      Simple_MAE = mean(abs(Simple - crime_count)) #Getting Mean Average Error for Simple
    ) %>%
    mutate(MASE = GBM_MAE/Simple_MAE) #Calculating MASE
  
  #Clean out the h2o cluster.
  h2o.removeAll()
  
  #Reporting out Metrics
  out = MASE_by_incident
  
  return(out)
})

rocv_df = map2(rocv_models, seq_along(rocv_models), function(model, idx){ #Creates a df of all the rocv models and labeling them with an iteration number
  
  out = model %>% mutate(rocv_idx = idx)
  
  return(out)
}) %>%
  bind_rows()

rocv_df %>%
  group_by(incident) %>%
  filter(MASE != 'Inf') %>%
  summarise(N = n(),
            Avg_MASE = mean(MASE),
            SD_MASE = sd(MASE),
            Lower_CI = t.test(MASE)$conf.int[1],
            Upper_CI = t.test(MASE)$conf.int[2])

################### Model Plots ################
rocv_df %>%
  ggplot() + geom_line(aes(x = rocv_idx, y = MASE)) +
  facet_wrap(~ incident)
