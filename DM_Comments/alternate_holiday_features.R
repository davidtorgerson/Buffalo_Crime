

## Use the spread() function instead of individual mutate() functions ##
## to create individual holiday indicators                            ##

bank_holidays = holidays(seq(2009,2021, 1)) %>%
  enframe(name = 'Holiday', value = 'Date') %>%
  mutate(New_Date = as.Date(as.character(Date), '%Y%m%d')) %>%
  mutate(Is_Holiday = 1)

left_join(complete_daily_incidents, 
          bank_holidays, 
          by = c("incident_date" = "New_Date")) %>%
  select(-Date) %>%
  mutate(Holiday = ifelse(is.na(Holiday), "None", Holiday)) %>%
  spread(Holiday, Is_Holiday) %>%
  mutate(Easter = isEaster(incident_date))


#######################################################################
## Is_Holiday indicator with leading/lagging indicators up to 2 days ##
#######################################################################

# This creates a single dummy indicator if any date represents a holiday
# Individual holiday dummies are ignored

# Holiday_Lead_1 and Holiday_Lead_2 reflect the days leading up to the holiday
#   - Examples: Christmas Eve, New Years Eve

# HOliday_Lag_1 and Holiday_Lag_2 reflect the days following the holiday
#   - Examples: Black Friday

complete_daily_incidents %>%
  mutate(Is_Holiday = as.integer(isHoliday(incident_date))) %>%
  mutate(Holiday_Lead_1 = dplyr::lead(Is_Holiday, n = 1)) %>%
  mutate(Holiday_Lead_2 = dplyr::lead(Is_Holiday, n = 2)) %>%
  mutate(Holiday_Lag_1 = dplyr::lag(Is_Holiday, n = 1)) %>%
  mutate(Holiday_Lag_2 = dplyr::lag(Is_Holiday, n = 2)) %>%
  drop_na()
