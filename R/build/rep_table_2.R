gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean.csv"),
               show_col_types = F) %>% 
  select(shalll:ppnf65o)

ns <- df %>% summarise(across(everything(), ~sum(!is.na(.x))))
means <- df %>% summarise(across(everything(), ~mean(.x, na.rm=T))) %>% round(3)
sds <- df %>% summarise(across(everything(), ~sd(.x, na.rm=T))) %>% round(3)

stats <- list(ns, means, sds) %>% 
  do.call("rbind", .) %>% 
  t() %>%
  as.data.frame %>% 
  rownames_to_column()

colnames(stats) <- c("Variable",
                     "N",
                     "Mean", 
                     "Standard Deviation")

panel_a <- stats %>% slice(1:25)
panel_b <- stats %>% slice(26:61)

panel_a %>%
  mutate(Variable = str_replace_all(Variable, "_", " "),
         Variable = str_to_title(Variable)) %>% 
  kbl(
    caption = "Main Variables Summary",
    booktabs = T,
    format = 'latex',
    label = 'tab:replicatetable2a'
  ) %>%
  kable_styling(latex_options = c("striped", "HOLD_position")) %>%
  write_lines(here("tables", "rep_table_2a.tex"))


panel_b %>% 
  mutate(
    Variable = str_replace(Variable, "ppw", "White "),
    Variable = str_replace(Variable, "ppb", "Black "),
    Variable = str_replace(Variable, "ppn", "Other "),
    Variable = str_replace(Variable, "m", "Male "),
    Variable = str_replace(Variable, "f", "Female ")
  ) %>% 
  kbl(
    caption = "Demographic Variables Summary",
    booktabs = T,
    format = 'latex',
    label = 'tab:replicatetable2b'
  ) %>%
    kable_styling(latex_options = c("striped", "HOLD_position")) %>%
    write_lines(here("tables", "rep_table_2b.tex"))
