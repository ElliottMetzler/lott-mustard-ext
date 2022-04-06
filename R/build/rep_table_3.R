gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean2.csv"),
               show_col_types = F) %>% 
  mutate(state = factor(state))

# Segregate Variables

# Log Crime Rate Variables
log_crime_rate_var <- df %>% 
  select(ends_with("crime_rate_log"))

state_year_fe_var <- df %>% 
  select(state,
         year)

# Shall Issue Dummy Variable
shall_issue_var <- df %>% 
  select(shalll)

# Arrest Rate Variables
arrest_rates_var <- df %>% 
  select(ends_with("arrest_rate"))

# Control Variables
control_var <- df %>% 
  select(density, # Population Density
         personal_income_rpc:retirement_payments_rpc, # RPC Income Data
         state_population, # State Population data
         ppwm1019:ppnf65o) # Race and Age Data

# Run Regressions Across Vars

models_list <- list()

for (reg_index in 1:9) {

  reg_df <- cbind(
    log_crime_rate_var[,reg_index],
    shall_issue_var,
    arrest_rates_var[,reg_index],
    control_var,
    state_year_fe_var
  )
  
  y <- colnames(reg_df)[1]
  colnames(reg_df)[3] <- "Relevant_Arrest_Rate"
  X <- colnames(reg_df)[2:45] %>% paste(.,collapse = " + ")
  X_fe <- colnames(reg_df)[46:47] %>% paste(., collapse = " + ")
  
  formula <- as.formula(paste(y,
                              " ~ ",
                              X,
                              " | ",
                              X_fe,
                              sep=""))
  
  this_model <- feols(fml = formula,
                      data = reg_df)
  
  models_list[[y]] <- this_model

}

# Export
etable(models_list,
       title = "Replication of Table 3: Fixed Effects Regressions",
       file = here("tables", "rep_table_3.tex"),
       replace = T,
       label = "tab:replicatetable3")
