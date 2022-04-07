gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean2.csv"),
               show_col_types = F) %>% 
  mutate(state = factor(state)) %>% 
  select(ends_with("crime_rate_log"),
         shalll, 
         state, 
         year)


for (i in 1:9) {
  y <- colnames(df[i])
  formula <- as.formula(paste0(y, "~ shalll"))
  
  curr_bacon <- bacon(formula,
                      df,
                      id_var = "state",
                      time_var = "year",
                      quietly=T) %>% 
    mutate(weighted_estimate = estimate * weight) %>% 
    group_by(type) %>% 
    summarise(weighted_estimate = sum(weighted_estimate))
  
  curr_coef <- sum(curr_bacon$weighted_estimate)
  
  coef_row <- data.frame(type = "Total TWFE",
                         weighted_estimate = curr_coef)
  
  table <- rbind(curr_bacon,coef_row)
  colnames(table)[2] <- y
  
  if (i==1) {
    full_table <- table
  }
  else {

    full_table <- full_table %>% left_join(table, by = "type")
  }

}

column_names <- colnames(full_table) %>% 
  str_to_title() %>% 
  str_replace_all("_"," ")

panels <- 3
panel_names <- c("A", "B", "C")

for (panel in 1:panels) {
  caption <- paste0("Bacon Decomposition: Panel ",panel_names[panel])
  label <- paste0("tab:bacondecomposition",
                 str_to_lower(panel_names[panel]))
  file <- paste0("bacon_decomposition",
                 str_to_lower(panel_names[panel]),
                 ".tex")
  
  end_col = panel * 3 + 1
  start_col = end_col - 2
  
  this_table <- cbind(full_table[,1], full_table[,start_col:end_col])
  these_cols <- c(column_names[1], column_names[start_col:end_col])
  
  this_table %>% 
    kbl(
      caption = caption,
      col.names = these_cols,
      booktabs = T,
      format = "latex",
      label = label
    ) %>% 
    kable_styling(latex_options = c("striped", "HOLD_position")) %>% 
    write_lines(here("tables", file))
}

