#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Elliott Metzler
# Project: Lott and Mustard Extension
# Created: 2/16/2022
# Last Modified: 2/16/2022
# 
# Purpose: 
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Read in raw data
# df_raw <- read_dta(here("data",
#                         "raw",
#                         "UpdatedCountyCrimeData-2010.dta"))

# fipsid is panel unit ID
# fipsstat is basically state -  but also has IDs for guam and shit
# fipscoun is the county ID but there are dups so basically we need combo

df <- df_raw %>% 
  clean_names() %>% 
  
  # Filter for years up through 1992, consistent with original paper
  filter(year <= 1992) %>% 
  group_by(year) %>% 
  summarise(across(c(rap), ~mean(.x, na.rm=T),
                   .names = "mean_{.col}"))

df %>% ggplot(aes(x = year, y = mean_rap)) + 
  geom_line(color = "navy",
            size = 2.5) + 
  labs(x = "Year",
       y = "Mean Murder Rate",
       title = "Mean Murder Rate over Time")
  



# 1. Aggregate to National level and get violent crimes

