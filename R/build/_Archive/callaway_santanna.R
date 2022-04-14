gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean.csv"),
               show_col_types = F) %>% 
  mutate(state = factor(state)) %>% 
  select(ends_with("crime_rate_log"), year, fipsstat, shalll) %>% 
  group_by(fipsstat, shalll) %>% 
  mutate(
    treat_year = min(year * shalll)) %>% 
  ungroup()

# For one reason or another, this does not work... will have to ask other folks
# about this one
atts <- att_gt(yname = "property_crime_rate_log",
               tname = "year",
               idname = "fipsstat",
               gname = "treat_year",
               data = df,
               xformla = NULL,
               # xformla = ~ INSERT_COVARIATE,
               # DEFAULT est_method = "dr",
               control_group = "notyettreated", #alternative is "nevertreated"
               # DEFAULT bstrap = T,
               # DEFAULT biters = 1000,
               # DEFAULT print_details = F,
               clustervars = "fipsstat",
               panel = TRUE)

summary(atts)

agg_effects <- aggte(atts, 
                     type = "group",
                     balance_e = T,
                     na.rm=T)
summary(agg_effects)
