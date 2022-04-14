gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "state_data_clean.csv"),
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
                      data = reg_df,
                      vcov = "HC1")
  
  models_list[[y]] <- this_model

}


# Export as 3 panels
panels <- 3
panel_names <- c("A", "B", "C")
control_variables <- paste(colnames(control_var), collapse = ", ")

for (panel in 1:panels) {
  title <- paste0("Replication of Table 3 Panel ", 
                 panel_names[panel],
                 " : Fixed Effects Regressions")
  
  file <- paste0("tables/", 
                "rep_table_3", 
                str_to_lower(panel_names[panel]),
                ".tex")
  
  label <- paste0("tab:replicatetable3",
                 str_to_lower(panel_names[panel]))
  
  end_col = panel * 3
  start_col = end_col - 2
  
  etable(models_list[start_col:end_col],
         title = title,
         file = here(file),
         fontsize = "small",
         drop = c(colnames(control_var)),
         replace = T,
         label = label,
         notes = paste0("Control variables ommited from table, 
                        though they were included in the analysis. 
                        Consistent with the original paper, 
                        control variables include: ", control_variables, "."))
}
