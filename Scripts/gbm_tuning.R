#### Run Main.R prior to running this ####

library(h2o)

h2o.init()

train = train_test_splits$train %>%
  mutate_at(vars(month:incident), list(as.factor)) #Converting crimes to categories

test = train_test_splits$test %>%
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

gbm_best_grid = h2o.getGrid(grid_id = "gbm_grid",
                            sort_by = "auc",
                            decreasing = TRUE)

print(gbm_best_grid)

best_gbm = h2o.getModel(gbm_best_grid@model_ids[[1]])