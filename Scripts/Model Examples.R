################ Examples of exploring different models ################

#Run Main.R in order to run these models.

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
    select(-crime_count, -incident_date, -contains("Holiday")) %>%
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