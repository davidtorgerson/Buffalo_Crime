#### Run Main.R prior to running this ####

library(h2o)

h2o.init()

rocv_tuning = map(train_test_splits, function(x){
  
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
  
  gbm_grid = h2o.grid("gbm", x = features, y = target,
                      training_frame = train_h2o,
                      validation_frame = test_h2o,
                      ntrees = 15,
                      seed = 1,
                      hyper_params = gbm_params)
  
  gbm_grid_sort = h2o.getGrid(grid_id = gbm_grid@grid_id, #Gets auto generated grid_id - this could also be manually listed in the h2o.grid call and passed through to this call.
                              sort_by = "rmse",
                              decreasing = TRUE)
  
  best_model_id = gbm_grid_sort@model_ids[[1]]
  
  best_gbm = h2o.getModel(best_model_id)
  
  best_params = gbm_grid_sort@summary_table %>%
    as_tibble() %>%
    mutate_at(vars(-model_ids), list(as.numeric)) %>%
    filter(model_ids == best_model_id)
  
  worst_params = gbm_grid_sort@summary_table %>%
    as_tibble() %>%
    mutate_at(vars(-model_ids), list(as.numeric)) %>%
    arrange(desc(rmse)) %>%
    slice(1)
  
  other_params_not_tuned = best_gbm@model$model_summary %>%
    as_tibble() %>%
    select(-any_of(colnames(best_params)))
  
  best_params_with_metrics = best_params %>%
    bind_cols(other_params_not_tuned)
  
  #Clean out the h2o cluster.
  h2o.removeAll()
  
  #Reporting out Metrics
  out = list(best_params_with_metrics = best_params_with_metrics,
             worst_params = worst_params)
  
  return(out)
})

best_params = map(rocv_tuning, function(x) x[["best_params_with_metrics"]]) %>%
  bind_rows() %>%
  unite(col = "Param_String", col_sample_rate:sample_rate, remove = F)

most_frequent_best_params = best_params %>%
  count(Param_String) %>%
  filter(n == max(n))

best_params %>%
  semi_join(most_frequent_best_params, by = "Param_String") %>%
  select(col_sample_rate:sample_rate, number_of_trees, min_depth) %>%
  distinct()
