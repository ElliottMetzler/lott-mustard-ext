gc()
rm(list = ls())

data <- read_dta(here("data", "raw", "UpdatedStateLevelData-2010.dta")) %>% 
  clean_names() %>% 
  
  # Filter to only include the years of interest
  filter(year <= 1992,
         year >= 1977) %>%
  
  # Reorder / rename some key variables
  select(
    fipsstat:year,
    
    shalll, # Shall issue dummy?
    
    # ARREST RATE DATA
    # Index Crimes
    violent_arrest_rate = aovio, # Arrests/Offenses - Violent
    property_arrest_rate = aopro, # Arrests/Offenses - Property
    murder_arrest_rate = aomur, # Arrests/Offenses - Murder
    rape_arrest_rate = aorap,  # Arrests/Offenses - Rape
    assault_arrest_rate = aoaga, # Arrests/Offenses - Aggravated Assault
    robery_arrest_rate = aorob, # Arrests/Offenses - Robbery
    burglary_arrest_rate = aobur, # Arrests/Offenses - Burglary
    larceny_arrest_rate = aolar, # Arrests/Offenses - Larceny
    autotheft_arrest_rate = aoaut, # Arrests/Offenses - Auto Theft
    
    # CRIME RATE DATA (per 100k people)
    # Index Crimes
    violent_crime_rate = ratvio, # Crimes per 100k ppl - Violent
    property_crime_rate = ratpro, # Crimes per 100k ppl - Property
    murder_crime_rate = ratmur, # Crimes per 100k ppl - Murder
    # Murder With Guns Missing
    rape_crime_rate = ratrap, # Crimes per 100k ppl - Rape
    assault_crime_rate = rataga, # Crimes per 100k ppl - Aggravated Assault
    robbery_crime_rate =ratrob, # Crimes per 100k ppl - Robbery
    burglary_crime_rate = ratbur, # Crimes per 100k ppl - Burglary
    larceny_crime_rate = ratlar, # Crimes per 100k ppl - Larceny
    autotheft_crime_rate = rataut, # Crimes per 100k ppl - Auto Theft
    
    # Causes of Accidental Deaths and Murder Rates per 100,000 ppl
    # seems to be missing?
    
    # Real per capital income data (real 1983 dollars)
    personal_income_rpc = rpcpi, # Personal Income
    unemployment_insurance_rpc = rpcui, # Unemployment Insurance
    income_maintenance_rpc = rpcim, # Income Maintenance
    retirement_payments_rpc = rpcrpo, # Retirement Payments per Person over 65
    
    # Population Characteristics
    # No County Population
    state_population = popstate,
    density,
    # No NRA membership
    # No % republican votes
    
    
    everything()) %>% 
  
  mutate(
    across(ends_with("_crime_rate"),
           .fns = list(log = ~log(.x)),
           .names = "{.col}_{.fn}")
  ) %>%
  
  # Final Variable selection and ordering
  select(fipsstat:density,
         ppwm1019:ppnf65o,
         violent_crime_rate_log:autotheft_crime_rate_log)


data %>% write_csv(here("data", "clean", "state_data_clean.csv"))
