
# Main.R

# Line 151 - The data argument should not be recent_daily_incidents. That
#            is your entire data set. The data argument should point to 
#            incident type (aliased as 'df', e.g. data = df). This will
#            apply rolling_origin to each incident type separately instead
#            of the full data.


# Line 153 - assess = 14. Why 14 day horizons? Is this arbitrary or have you
#            thought of an explanation.

# Line 154 - cumulative = TRUE. This is neither right or wrong, but ask
#            yourself if this is really necessary. What are you gaining by
#            retaining all availalbe training data after each ROCV iteration?
#            Perhaps retaining all availalbe training data will lead to 
#            better accuracy, but maybe not. Accumulating the training
#            data will definitely lead to longer training times. Also,
#            what if you scheduled your model to automatically retrain 
#            each day? Would you just continue to allow the data to 
#            accumulate?

# Line 155 - skip = 14. Why skip forward 14 days after each split?

# The ROCV parameters (initial, assess, skip) result in a total of 
# 50 iterations. This means you'll be training 50 independent models. 
# That will give you a sample size of 50 for judging the expected accuracy
# of the model. That's a good thing, but it may take a while to train.

# Next Steps:
#  - settle on the ROCV design
#  - play with various H2O models (example below) and get familiar with
#    the basic functions.

###########################################
## An alternative way to design the ROCV ##
###########################################

# This is arguably a cleaner way to design the ROCV

# The idea is to transfrom recent_daily_incidents from long to wide
# so that there is one row per date. The incident types are converted to
# individual columns, and the values correspond to crime counts.
# Then apply ROCV to the entire wide data. Having one row per date
# ensure that the rolling_origin function works as expected. Finally,
# within each ROCV split convert the data back to the original long
# form and split into training/testing.

recent_daily_incidents_wide <- recent_daily_incidents %>%
  spread(incident, crime_count) %>%
  mutate_at(vars(ASSAULT:UUV), list(z_scale))

rocv_splits <- recent_daily_incidents_wide %>%
  rolling_origin(
    data = ., #Using recent data from 2015 and later
    initial = (365*4), #Using four years for the initial training sample
    assess = 14, #Forecast Horizon - how far in the future do I want to predict
    cumulative = TRUE,
    skip = 14
  ) 

train_test_splits <- map(rocv_splits$splits, function(split) {
  
  # use the analysis() function from rsample to get the training data
  # use the gather() function from tidyr to convert to long format
  train <- analysis(split) %>%
    gather(incident, crime_count, -incident_date, -month, -year, -week, -weekday)
  
  # use the assessment() function from rsample to get the testing data
  test <- assessment(split) %>%
    gather(incident, crime_count, -incident_date, -month, -year, -week, -weekday) 
  
  out <- list(train = train, test = test)
  
  return(out)
  
})

train_test_splits[1]


####################################################
## Sample code for building and evaluating models ##
####################################################

library(h2o) # install.packages("h2o")

sample_rocv_iteration <- train_test_splits[[1]]

map(sample_rocv_iteration, function(i) {
  
  # Use these placeholders for developing the code:
  #
  # train <- sample_rocv_iteration$train %>% mutate(incident = factor(incident))
  # test <- sample_rocv_iteration$test %>% mutate(incident = factor(incident))
  
  # extract training data from ROCV iteration
  # convert incident to factor for H2O to work properly
  train <- i$train %>%
    mutate(incident = factor(incident))
  
  # extract testing data from ROCV iteration
  # convert incident to factor for H2O to work properly
  test <- i$test %>%
    mutate(incident = factor(incident))
  
  # get column names of features
  feature_names <- train %>%
    select(-crime_count) %>%
    colnames()
  
  # get target
  target_name <- "crime_count"
  
  # train/test a Generalized Linear Model
  # since target is numeric, this means Linear Regression
  # other regression models:
  # - help(h2o.gbm)
  # - help(h2o.randomForest)
  # - help(h2o.deeplearning)
  model <- h2o.glm(
    x = feature_names,
    y = target_name,
    training_frame = as.h2o(train),
    validation_frame = as.h2o(test),
    lambda_search = T
  )
  
  # performance metrics
  perf_metrics <- h2o.performance(model, valid = T)
  
  perf_metrics_tbl <- tibble(
    RMSE = perf_metrics@metrics$RMSE,
    R2 = perf_metrics@metrics$r2
  )
  
  # variable importances
  h2o.varimp(model)
  
})

