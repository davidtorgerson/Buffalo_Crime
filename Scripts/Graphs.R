## Run Main.R, Holiday_Assessment.R, gbm_tuning.R prior to running graphs ##

library(ggplot2)
library(tidyverse)

# MASE Statistic by ROCV Iteration # (Main.R)
rocv_df %>%
  ggplot() + geom_line(aes(x = rocv_idx, y = MASE, color = incident)) +
  facet_wrap(~ incident) +
  xlab("ROCV Iteration") +
  ylab("MASE") +
  ggtitle("MASE for each ROCV Iteration by Incident Type")

#Average MASE for Holiday and No Holiday models. # (Holidays Assessment)

Average_MASE_graph = rocv_mase %>%
  filter(Holidays_MASE != 'Inf' & No_Holidays_MASE != "Info") %>%
  group_by(incident) %>%
  summarise(Holidays_MASE_Avg = mean(Holidays_MASE),
            No_Holidays_MASE_Avg = mean(No_Holidays_MASE)) %>%
  gather(key = "Measure", value = "value", -incident)

ggplot(Average_MASE_graph, aes(x = Measure, y = value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ incident) +
  scale_fill_brewer(palette = "Set1")
  
