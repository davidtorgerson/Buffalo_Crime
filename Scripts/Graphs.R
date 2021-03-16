## Run Main.R, Holiday_Assessment.R, gbm_tuning.R prior to running graphs ##

library(ggplot2)
library(tidyverse)

# MASE Statistic by ROCV Iteration #
rocv_df %>%
  ggplot() + geom_line(aes(x = rocv_idx, y = MASE, color = incident)) +
  facet_wrap(~ incident) +
  xlab("ROCV Iteration") +
  ylab("MASE") +
  ggtitle("MASE for each ROCV Iteration by Incident Type")