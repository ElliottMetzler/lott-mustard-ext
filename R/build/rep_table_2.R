gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean.csv"),
               show_col_types = F) %>% 
  select(shalll:state_population)

ns <- df %>% summarise(across(everything(), ~sum(!is.na(.x))))
means <- df %>% summarise(across(everything(), ~mean(.x, na.rm=T))) %>% round(2)
sds <- df %>% summarise(across(everything(), ~sd(.x, na.rm=T))) %>% round(2)

stats <- list(ns, means, sds) %>% 
  do.call("rbind", .) %>% 
  t() %>%
  as.data.frame %>% 
  rownames_to_column()

colnames(stats) <- c("Variable",
                     "N",
                     "Mean", 
                     "Standard Deviation")

stats %>%
  mutate(Variable = str_replace_all(Variable, "_", " "),
         Variable = str_to_title(Variable)) %>% 
  kbl(
    caption = "Replication of Table 2",
    booktabs = T,
    format = 'latex',
    label = 'tab:replicatetable2'
  ) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position")) %>% 
  write_lines(here("tables", "rep_table_2.tex"))

