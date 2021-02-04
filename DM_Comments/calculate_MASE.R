
# Project forward last 14 days of crime counts for each incident type in training data
naive_model <- train %>%
  arrange(incident, incident_date) %>%
  group_by(incident) %>%
  slice(tail(row_number(), 14)) %>%  # get last 14 rows for each incident
  ungroup() %>%
  mutate(incident_date = incident_date + days(14)) %>%  # add 14 days for merging with test data
  select(incident_date, incident, Naive = crime_count)

# predictions from H2O model
predictions <- h2o.predict(model, newdata = as.h2o(test)) %>% as.vector()

test %>%
  mutate(H2O_Model = predictions) %>%  # add predictions from H2O
  inner_join(naive_model, by = c("incident", "incident_date")) %>% # add naive predictions
  group_by(incident) %>%
  summarise(
    H2O_MAE = mean(abs(H2O_Model - crime_count)), # H2o MAE
    Naive_MAE = mean(abs(Naive - crime_count))    # Naive MAE
  ) %>%
  mutate(MASE = H2O_MAE/Naive_MAE)  # MASE

# A tibble: 9 x 4
# incident      H2O_MAE Naive_MAE  MASE
# * <fct>           <dbl>     <dbl> <dbl>
# 1 ASSAULT         2.77      3.36  0.824
# 2 BURGLARY        2.99      3.36  0.891
# 3 LARCENY/THEFT   4.97      4.79  1.04 
# 4 MURDER          0.226     0.286 0.791
# 5 RAPE            0.495     0.429 1.15 
# 6 ROBBERY         1.50      2.29  0.658
# 7 SEXUAL ABUSE    0.938     1.07  0.876
# 8 THEFT           0.833     0.857 0.971
# 9 UUV             1.44      2.07  0.695