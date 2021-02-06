

# Lines 319 - 373
# 
# A subtle yet important detail regarding the H2O software
# When you use as.h2o(train) or as.h2o(test), H2O will
# create another copy of the train/test data in the H2O "cluster".
# To make the matter worse, the ROCV repeats 50 times and within
# each ROCV iteration as.h2o() is currently being used 3 times
# on lines 336, 337, and 335. 50*3 = 150 copies of the train/test data.
# The cluster is simply a virtual environment where all of the
# H2O computations happen. It's important to keep this cluster
# clean and avoid copying the same data multiple times to prevent
# the cluster from running out of storage space. To solve this problem,
# simply place `train_h2o = as.h2o(train)` and `test_h2o = as.h2o(test)`
# after line 326 but before line 333. Then, anywhere you use as.h2o() 
# replace it with either train_h2o or test_h2o. Finally, at the very 
# end of the function (e.g. line 371) insert the function h2o.removeAll()
# to safely clean out the cluster.

# Regarding your model, *if* you're interested there are two areas that 
# I feel can be improved. 1) New features, and 2) GBM hyperparamter tuning.
# New features like rolling 14-day average and binary indicators for holidays
# would very likely improve your MASE. Hyperparameter tuning requires
# no additional data but the code implementation is admittedly tough. You
# almost certainly would improve your model's performance.

# If you're satisfied with the model, then I would start creating some
# visualizations to illustrate the results of your experiment. This could
# include line charts of the MASE for each ROCV iteration, histograms
# showing the distribution of MASE by incident type, etc. ggplot2
# would be a good start, or you could use plotly to make your ggplot2
# graphics interactive (i.e hover tool tips, zoom).
