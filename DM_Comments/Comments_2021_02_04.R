
# Line 200 - consider removing "incident_date" from the feature list
#            in addition to crime_count.

# Experiment with a tree-based model instead, e.g. GBM
# GBM = Gradient Boosted Machine
# other options: h2o.randomForest(), h2o.deeplearning()
model = h2o.gbm(
  x = features,
  y = target,
  training_frame = as.h2o(train),
  validation_frame = as.h2o(test),
  ntrees = 50, # default
  learn_rate = 0.1, # default
  max_depth = 5, # default
  categorical_encoding = "AUTO", # default
  distribution = "poisson" # common for non-negative count data
)

# The values of the "hyperparameters" (e.g. ntrees, learn_rate, max_depth) can 
# significantly affect the overall model performance. If not set properly, your
# model may "underfit" or "overfit" the training data and not generalize well
# to the unseen testing data. There are some simple and complicated ways of doing
# hyperparameter tuning. Let me know when you are ready to cross this bridge.

# Consider treating your date-based features as categorical instead
# of numeric. If numeric, some algorithms (e.g. GLM and deep learning)
# will attempt to find linear correlations with crime counts (e.g. the
# higher the weekday, the more crimes) which don't make sense. To treat
# them as categorical, just convert them to "factor" data types and H2O
# will handle them accordingly.
# Sample code: sample_rocv$train %>% mutate_at(vars(month:incident), list(as.factor))

# Line 151 - Consider abandoning the Z-scale transformation. While it helps when comparing
#            model accuracy between incident types, there are other ways to do this and
#            the use of Z-transformations adds some other unnecessary complexities. We
#            can discuss this.

# Consider coming up a repeatable methodology for comparing the predictions to the
# known values in the test set. Sample code...
predictions <- h2o.predict(model, newdata = as.h2o(test)) %>% as.vector()
test %>% mutate(Prediction = predictions)

################
## Next Steps ##
################

# - Settle on an algorithm that is likely to work better than the linear model.
# - Decide how to measure the quality of your model using a repeatable function.
# - Move away from the sample train/test and implement your model training/evaluation with ROCV

