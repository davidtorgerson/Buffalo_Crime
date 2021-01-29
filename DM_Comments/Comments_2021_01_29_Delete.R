
# Main.R

# So far so good! Very clean, readable, and reproducible code

# -  new_crime object name used first on line 58 then overwritten
#    on line 76. Avoid reusing object names. 
#    Suggestion: Relocate the mutate() clause on lines 77-81
#                to lines 60-64 and delete lines 76-81. This
#                will limit new_crime to a single use.

# -  daily_incidents object name (lines 86 and 106) used twice

# -  Some incident types have virtually no data. Consider removing
#    these from the data (e.g. breaking & entering, homicide).
#    Sample code: new_crime %>% count(incidient)

# -  The left_join on line 106 is not actually filling in missing 
#    dates for each incident type. It's searching for the sequence of dates
#    within the full data, and technically each date exists, but not for each
#    incident type. My recommendation is to split the data by incident type,
#    apply the left_join *within each split*, then recombine the results.
#    Sample code...

library(purrr) # contains the map() function for splitting a dataframe
library(tidyr) # contains functions fill() and replace_na()

incidents_complete <- daily_incidents %>%
  split(.$incident) %>%
  map(., function(i) {
    left_join(
      x = seq_dates,
      y = i,
      by = "incident_date"
    ) %>%
      fill(incident, .direction = "downup") %>%
      replace_na(list(n = 0))
  }) %>%
  bind_rows()

#  - consider renaming some of your objects to make their meaning clearer
#    e.g. new_crime --> crime_since_2009
