

## Run Main.R up to the point of train_test_splits

library(h2o)
h2o.init()

# function to build a model given features, target, and train/test data
run_h2o_model <- function(features, target = 'crime_count', train, test) {
  model = h2o.gbm(
    x = features,
    y = target,
    training_frame = train,
    validation_frame = test,
    ntrees = 50, #How many trees do you want the model to build, Default set at 50
    learn_rate = 0.1, #Default set at 0.1
    max_depth = 5, #Default set at 5
    categorical_encoding = "AUTO", #Default set at AUTO
    distribution = "poisson" #Used for non-negative count data
  )
  return(model)
}

# function to get a vector of predictions given model and test data
get_predictions <- function(model, test) {
  predictions <- h2o.predict(model, newdata = test) %>%
    as.vector()
  return(predictions)
}

rocv_models2 = map(train_test_splits, function(x){
  
  # Use this for testing a single ROCV iteration:
  #
  # x <- train_test_splits[[1]]
  
  ############################
  # Original train/test data #
  ############################
  
  train = x$train %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  test = x$test %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  #######################
  # H2O train/test data #
  #######################
  
  train_h2o = as.h2o(train)
  test_h2o = as.h2o(test)
  
  ############
  # Features #
  ############
  
  features_with_holidays = train %>%
    select(-crime_count, -incident_date) %>%
    colnames()
  
  features_without_holidays = train %>%
    select(-crime_count, -incident_date, -contains("Holiday")) %>%
    colnames()
  
  ##########
  # Models #
  ##########
  
  h2o_with_holidays <- run_h2o_model(
    features = features_with_holidays,
    train = train_h2o,
    test = test_h2o
  )
  
  h2o_without_holidays <- run_h2o_model(
    features = features_without_holidays,
    train = train_h2o,
    test = test_h2o
  )
  
  simple_model = train %>%
    arrange(incident, incident_date) %>% #Sorting by incident and date
    group_by(incident) %>%
    slice(tail(row_number(), 14)) %>% #Grab the last 14 rows of each incident
    ungroup() %>%
    mutate(incident_date = incident_date + days(14)) %>% #Adding 14 days to merge with testing data
    select(incident_date, incident, Simple = crime_count)
  
  ###############
  # Predictions #
  ###############
  
  pred_with_holidays <- get_predictions(
    model = h2o_with_holidays,
    test = test_h2o
  )
  
  pred_without_holidays <- get_predictions(
    model = h2o_without_holidays,
    test = test_h2o
  )
  
  ###########
  # Outputs #
  ###########
  
  test_with_predictions = test %>%
    mutate(Pred_with_Holidays = pred_with_holidays) %>%
    mutate(Pred_without_Holidays = pred_without_holidays) %>%
    inner_join(simple_model, by = c("incident","incident_date"))
  
  #MASE Calculation
  MASE_by_incident = test_with_predictions %>%
    group_by(incident) %>%
    summarise(
      Holidays_MAE = mean(abs(Pred_with_Holidays - crime_count)),
      No_Holidays_MAE = mean(abs(Pred_without_Holidays - crime_count)),
      Simple_MAE = mean(abs(Simple - crime_count)) 
    ) %>%
    mutate(Holidays_MASE = Holidays_MAE/Simple_MAE) %>%
    mutate(No_Holidays_MASE = No_Holidays_MAE/Simple_MAE)
  
  #Clean out the h2o cluster.
  h2o.removeAll()
  
  #Reporting out Metrics
  out = list(MASE_by_incident = MASE_by_incident,
             test_with_predictions = test_with_predictions)
  
  return(out)
})

##########################
# Global MASE Assessment #
##########################

rocv_mase = map2(rocv_models2, seq_along(rocv_models2), function(model, idx){
  out = model$MASE_by_incident %>% mutate(rocv_idx = idx)
  return(out)
}) %>%
  bind_rows()

# T-test comparing the MASE statistics for Holidays/No_Holidays
# by incident type
# install.packages("rstatix")
rocv_mase %>%
  filter(Holidays_MASE != 'Inf' & No_Holidays_MASE != "Info") %>%
  select(incident, Holidays_MASE, No_Holidays_MASE) %>%
  gather(Features, MASE, -incident) %>%
  group_by(incident) %>%
  rstatix::pairwise_t_test(MASE ~ Features)

#######################################
## MASE Assessment for Holidays only ##
#######################################

rocv_holiday_predictions = map2(rocv_models2, seq_along(rocv_models2), function(model, idx){
  out = model$test_with_predictions %>% mutate(rocv_idx = idx)
  return(out)
}) %>%
  bind_rows() %>%
  inner_join(bank_holidays, by = c("incident_date" = "New_Date"))

MASE_holiday_dates = rocv_holiday_predictions %>%
  group_by(incident) %>%
  summarise(
    N_Holidays = n(),
    Holidays_MAE = mean(abs(Pred_with_Holidays - crime_count)),
    No_Holidays_MAE = mean(abs(Pred_without_Holidays - crime_count)),
    Simple_MAE = mean(abs(Simple - crime_count)) 
  ) %>%
  mutate(Holidays_MASE = Holidays_MAE/Simple_MAE) %>%
  mutate(No_Holidays_MASE = No_Holidays_MAE/Simple_MAE)

MASE_holiday_dates %>%
  select(incident, N_Holidays, Holidays_MASE, No_Holidays_MASE)
