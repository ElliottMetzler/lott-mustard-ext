gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean.csv"),
               show_col_types = F) %>% 
  mutate(state = factor(state)) %>% 
  select(ends_with("crime_rate_log"),
         shalll, 
         state, 
         year)

# feols(murder_crime_rate_log ~ shalll | state + year, df) %>% coef
# Note to self: Through all that annoying searching, we figured out the key here.
# To calculate the TWFE Coefficient from the bacon deal we run the model and then
# are calculating a weighted average of the estimates for each item. Meaning, using the
# raw weights and estimates, we calculate the product to get a full vector. To summarize and
# present we take the average of the estimates, the sum of the weights, and the sum of the weighted
# estimates. 

for (i in 1:9) {
  y <- colnames(df[i])
  formula <- as.formula(paste0(y, "~ shalll"))
  
  
  
  curr_bacon <- bacon(formula,
                      df,
                      id_var = "state",
                      time_var = "year",
                      quietly=T) %>% 
    mutate(weighted_est = weight * estimate) %>% 
    group_by(type) %>% 
    summarise(average_estimate = mean(estimate),
              group_weight = sum(weight),
              weighted_estimate = sum(weighted_est))
  
  total_coef <- sum(curr_bacon$weighted_estimate)
  total_row <- data.frame(type = "Total TWFE",
                          average_estimate = NaN,
                          group_weight = NaN,
                          weighted_estimate = total_coef)
  table <- rbind(curr_bacon, total_row)

  clean_var_name <- y  %>% str_replace_all("_", " ") %>% str_to_title()
  clean_label_name <- clean_var_name %>% 
    str_replace_all(" ", "") %>% 
    str_replace_all("CrimeRateLog","")
  
  caption <- paste0("Bacon Decomposition: ", clean_var_name)
  label <- paste0("tab:bacondecomposition", clean_label_name)
  file <- paste0("bacon_decomposition_", clean_label_name, ".tex")
  
  table %>% 
    kbl(
      caption = caption,
      col.names = c("Type",
                    "Average Estimate", 
                    "Group Weight", 
                    "Weighted Estimate"),
      booktabs = T,
      format = "latex",
      label = label
    ) %>% 
    kable_styling(latex_options = c("striped", "HOLD_position")) %>% 
    write_lines(here("tables", file))
  
}

