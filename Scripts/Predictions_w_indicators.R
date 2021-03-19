### Run Main.R prior to running this ###

#This output just shows a dataframe with the actual predictions of all days


############### Testing predictions for Holiday Indicators ############
rocv_models2 = map(train_test_splits, function(x){
  
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
  
  test_with_predictions = test %>%
    mutate(Predictions = predictions) %>%
    inner_join(simple_model, by = c("incident","incident_date"))
  
  #MASE Calculation
  MASE_by_incident = test_with_predictions %>%
    group_by(incident) %>%
    summarise(
      GBM_MAE = mean(abs(Predictions - crime_count)), #Getting Mean Average Error for GBM
      Simple_MAE = mean(abs(Simple - crime_count)) #Getting Mean Average Error for Simple
    ) %>%
    mutate(MASE = GBM_MAE/Simple_MAE) #Calculating MASE
  
  #Clean out the h2o cluster.
  h2o.removeAll()
  
  #Reporting out Metrics
  out = list(MASE_by_incident = MASE_by_incident,
             test_with_predictions = test_with_predictions)
  
  return(out)
})

rocv_mase = map2(rocv_models2, seq_along(rocv_models2), function(model, idx){
  out = model$MASE_by_incident %>% mutate(rocv_idx = idx)
  return(out)
}) %>%
  bind_rows()

rocv_predictions = map2(rocv_models2, seq_along(rocv_models2), function(model, idx){
  out = model$test_with_predictions %>% mutate(rocv_idx = idx)
  return(out)
}) %>%
  bind_rows()
