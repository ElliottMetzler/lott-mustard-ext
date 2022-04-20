gc()
rm(list = ls())

data <- read_csv(here("data", "clean", "state_data_clean.csv"),
         show_col_types = F) %>% 
  select(state, year, shalll) %>% 
  group_by(state, shalll) %>% 
  summarise(year = min(year)) %>% 
  ungroup() %>% 
  filter(shalll == 1) %>% 
  select(-shalll) %>% 
  arrange(year)

data %>% 
  kbl(caption = "Shall Issue Law Rollout By State",
      col.names = c("State", "Year"),
      booktabs = T,
      format = "latex",
      label = "tab:rollout") %>% 
  kable_styling(latex_options = c("striped", "HOLD_position")) %>% 
  write_lines(here("tables", "rollout.tex"))
