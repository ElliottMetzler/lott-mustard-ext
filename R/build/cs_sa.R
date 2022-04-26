gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean.csv"),
               show_col_types = F) %>% 
  mutate(state = factor(state)) %>% 
  select(fipsstat, year, shalll,
         ends_with("crime_rate_log"),
         ends_with("arrest_rate")) %>% 
  group_by(fipsstat) %>% 
  mutate(
    shalll_ever = if_else(sum(shalll)>0,1,0)
  ) %>% 
  ungroup()

treat_years <- df %>% 
  select(fipsstat, year, shalll) %>% 
  mutate(product = shalll * year) %>% 
  group_by(fipsstat, shalll) %>% 
  summarise(treat_year = min(product)) %>% 
  ungroup() %>% 
  filter(shalll == 1) %>% 
  select(-shalll)

df <- df %>% left_join(treat_years, by = "fipsstat") %>%
  mutate(treat_year = replace_na(treat_year, 0))

rm(treat_years)

# Run the Callaway and Sant'anna and Sun and Abraham

y_vars <- df %>% select(ends_with("crime_rate_log")) %>% colnames
x_vars <- df %>% select(ends_with("arrest_rate")) %>% colnames

# cs_atts_out <- list()
cs_agg_out <- list()
cs_group_coefs <- list()
# cs_agg_es_out <- list()
sa_out <- list()


for (i in 1:length(y_vars)) {
  
  # Callaway and Sant'anna
  y <- y_vars[i]
  x <- x_vars[i]
  x_form <- as.formula(paste0("~", x))
  
  atts <- att_gt(yname = y,
                 tname = "year",
                 idname = "fipsstat",
                 gname = "treat_year",
                 data = df,
                 # xformla = NULL,
                 xformla = x_form,
                 # DEFAULT est_method = "dr",
                 control_group = "notyettreated", #alternative is "nevertreated"
                 # DEFAULT bstrap = T,
                 # DEFAULT biters = 1000,
                 # DEFAULT print_details = F,
                 clustervars = "fipsstat",
                 panel = TRUE)
  # cs_atts_out[[y]] <- atts
  
  agg <- aggte(atts,
               type = "group",
               balance_e = T,
               na.rm = T)
  cs_agg_out[[y]] <- agg
  
  cs_group_coefs[[y]] <- c(agg$overall.att,
                           agg$overall.se)
  
  # agg_es <- aggte(atts, 
  #                 type = "dynamic",
  #                 balance_e = T,
  #                 na.rm = T)
  # cs_agg_es_out[[y]] <- agg_es
  
  # Sun and Abraham
  sa_form <- paste0(y,
                    " ~ ",
                    x,
                    " + sunab(treat_year, year) | fipsstat + year") %>% 
    as.formula()
  
  sa <- feols(fml = sa_form,
              data = df)
  sa_out[[y]] <- sa
  
}

# Create CS Table
do.call("rbind", cs_group_coefs) %>% 
  as.data.frame %>% 
  rownames_to_column() %>% 
  mutate(rowname = rowname %>% str_replace_all("_", " ") %>% str_to_title(),
         across(V1:V2, ~round(.x,3))) %>% 
  kbl(
    caption = "Callaway and Sant'anna Overall ATTs",
    col.names = c("Outcome Variable",
                 "Overall ATT",
                 "Std. Error"),
    booktabs = T,
    format = "latex",
    label = "tab:cs"
  ) %>% 
  kable_styling(latex_option = c("striped", "HOLD_position")) %>% 
  write_lines(here("tables", "cs.tex"))

# Create SA Figures
rm(i)
for (i in 1:length(y_vars)) {
  
  y <- y_vars[i]
  short_y <- y %>% str_remove("_crime_rate_log")
  path <- paste0("figures/", short_y, ".png")
  png(here(path), width = 700, height = 350)
  iplot(sa_out[[y]], ref.line = -1, main = "")
  dev.off()
}

