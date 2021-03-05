## Run Main.R until train_test_splits ##

library(h2o)
h2o.init()

run_h2o_model = function(features, target = 'crime_count', train, test) {
  model = h2o.gbm(
    x = features,
    y = target,
    training_frame = train,
    validation_frame = test,
    ntrees = 50, #How many trees to build (default @ 50)
    learn_rate = 0.1, #Default @ 0.1
    max_depth = 5, #Default at 5
    categorical_encoding = "AUTO", #Default at AUTO
    distribution = "poisson" #Used for non-negative counts
  )
  return(model)
}

#Function to get vector of predictions given model and testing data
get_predictions = function(model, test) {
  predictions = h2o.predict(model, newdata = test) %>%
    as.vector()
  
  return(predictions)
}

rocv_models2 = map(train_test_splits, function(x){
  
  ## Original train/test data ##
  
  train = x$train %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  test = x$test %>%
    mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories
  
  
  ## h2o train/test data ##
  
  train_h2o = as.h2o(train)
  test_h2o = as.h2o(test)
  
  ## Features ##
  features_with_holidays = train %>%
    select(-crime_count, -incident_date) %>%
    colnames()
  
  features_without_holidays = train %>%
    select(-crime_count, -incident_date, -contains("Holiday")) %>%
    colnames()
  
  ## Models ##
  
  h2o_with_holidays = run_h2o_model(
    features = features_with_holidays,
    train = train_h2o,
    test = test_h2o)
  
  h2o_without_holidays = run_h2o_model(
    features = features_without_holidays,
    train = train_h2o,
    test = test_h2o)
  
  simple_model = train %>%
    arrange(incident, incident_date) %>% #Sort by incident and date
    group_by(incident) %>%
    slice(tail(row_number(),14)) %>% #Grabbing the last 14 days per incident
    ungroup() %>%
    mutate(incident_date = incident_date + days(14)) %>% #Adding 14 days to merge with test
    select(incident_date, incident, Simple = crime_count)
  
  ## Predictions ##
  
  pred_with_holidays = get_predictions(
    model = h2o_with_holidays,
    test = test_h2o)
  
  pred_without_holidays = get_predictions(
    model = h2o_without_holidays,
    test = test_h2o)
  
  ## Outputs ##
  
  test_with_predictions = test %>%
    mutate(pred_with_holidays = pred_with_holidays) %>%
    mutate(pred_without_holidays = pred_without_holidays) %>%
    inner_join(simple_model, by = c("incident","incident_date"))
  
  # MASE Calculation #
  
  MASE_by_incident = test_with_predictions %>%
    group_by(incident) %>%
    summarise(
      Holidays_MAE = mean(abs(pred_with_holidays - crime_count)),
      No_Holidays_MAE = mean(abs(pred_without_holidays - crime_count)),
      Simple_MAE = mean(abs(Simple - crime_count))
    ) %>%
    mutate(Holidays_MASE = Holidays_MAE/Simple_MAE) %>%
    mutate(No_Holidays_MASE = No_Holidays_MAE/Simple_MAE)
  
  # Clean h2o cluster #
  
  h2o.removeAll()
  
  # Retrieve metrics #
  
  out = list(MASE_by_incident = MASE_by_incident,
             test_with_predictions = test_with_predictions)
  
  return(out)
})