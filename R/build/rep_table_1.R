gc()
rm(list = ls())

read_csv(here("data", "clean", "state_data_clean.csv"),
         show_col_types = F) %>% 
  select(state, 
         ends_with("crime_rate"),
         ends_with("arrest_rate")) %>% 
  group_by(state) %>% 
  summarise(across(everything(),
                   list(state_means = ~ mean(.x,na.rm=T),
                        state_sds = ~sd(.x,na.rm=T)),
                   .names = "{.col}_{.fn}")) %>% 
  ungroup() %>% 
  summarise(across(ends_with("_means"),
                   ~ sd(.x, na.rm=T),
                   .names = "{.col}_sd"),
            across(ends_with("_sds"),
                  ~ mean(.x, na.rm=T),
                  .names = "{.col}_mean")) %>% 
  round(2) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(value = V1,
         stat = rowname) %>% 
  separate(stat, c("Variable", "Statistic"), sep = "_state_") %>% 
  pivot_wider(id_cols = Variable,
              names_from = Statistic,
              values_from = value) %>% 
  mutate(Variable = str_replace_all(Variable, "_", " "),
         Variable = str_to_title(Variable)) %>% 
  kbl(
    caption = "Replication of Table 1",
    col.names = c("Variable",
                  "Standard Deviation of State Means",
                  "Mean of Within-State Standard Deviations"),
    booktabs = T,
    format = 'latex',
    label = 'tab:replicatetable2'
  ) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position")) %>% 
  write_lines(here("tables", "rep_table_1.tex"))
