#### Run Main.R prior to running this ####

library(h2o)

h2o.init()

rocv_hp_tuning = map(train_test_splits, function(x){
  
  # Use this for testing a single ROCV iteration:
  #
  # x <- train_test_splits[[1]]
  
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
  
  gbm_params = list(
    learn_rate = c(0.01,0.1),
    max_depth = c(3,5,9),
    sample_rate = c(0.8,1.0),
    col_sample_rate = c(0.2,0.5,1.0)
  )
  
  gbm_grid = h2o.grid("gbm", 
                      x = features, y = target,
                      training_frame = train_h2o,
                      validation_frame = test_h2o,
                      ntrees = 15,
                      seed = 1,
                      hyper_params = gbm_params)
  
  #################################################
  # Grid search results sorted best to worst RMSE #
  #################################################
  
  grid_sort = h2o.getGrid(
    grid_id = gbm_grid@grid_id,
    sort_by = "rmse",
    decreasing = FALSE
  )
  
  ##############
  # Best model #
  ##############
  
  best_model_id = grid_sort@model_ids[[1]]
  
  best_gbm = h2o.getModel(best_model_id)
  
  ###############################################
  # Hyperparameters from best (and worst) model #
  ###############################################
  
  best_params <- grid_sort@summary_table %>%
    as_tibble() %>%
    mutate_at(vars(-model_ids), list(as.numeric)) %>%
    filter(model_ids == best_model_id)
  
  worst_params <- grid_sort@summary_table %>%
    as_tibble() %>%
    mutate_at(vars(-model_ids), list(as.numeric)) %>%
    arrange(desc(rmse)) %>%
    slice(1)
  
  other_params_not_tuned <- best_gbm@model$model_summary %>%
    as_tibble() %>%
    select(-any_of(colnames(best_params)))
  
  ##########
  # Output #
  ##########
  
  best_params_with_metric <- best_params %>%
    bind_cols(other_params_not_tuned)
  
  out <- list(
    best_params_with_metric = best_params_with_metric,
    worst_params = worst_params
  )
  
  
  #Clean out the h2o cluster.
  h2o.removeAll()
  
  return(out)
})

# tibble  of best hyperparameters for each ROCV iteration
best_params <- map(rocv_hp_tuning, function(x) x[["best_params_with_metric"]]) %>%
  bind_rows() %>%
  unite(col = "Param_String", col_sample_rate:sample_rate, remove = F)

# hyperparameters that "won" most often
most_frequent_best_params <- best_params %>%
  count(Param_String) %>%
  filter(n == max(n))

## YOUR BEST PARAMS!!!
best_params %>%
  semi_join(most_frequent_best_params, by = "Param_String") %>%
  select(col_sample_rate:sample_rate, number_of_trees, min_depth) %>%
  distinct()

