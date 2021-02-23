library(h2o)

h2o.init()

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