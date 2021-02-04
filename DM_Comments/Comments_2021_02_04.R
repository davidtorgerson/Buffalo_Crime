

# Experiment with a tree-based model instead, e.g.
# GBM = Gradient Boosted Machine
model = h2o.gbm(
  x = features,
  y = target,
  training_frame = as.h2o(train),
  validation_frame = as.h2o(test)
)