# Load required packages
library(tidyverse)
library(panelvar)
library(plm)
library(janitor)

# ESTIMATION ====

# Import and clean data - keep year as numeric
data <- read_csv("final_data/master/cons_price_vol_with_rural_urban.csv") %>%
  clean_names()

str(data)

# Create panel data structure
panel_data <- data %>%
  # Select relevant variables
  select(
    local_authority,
    year,
    domestic_cons_elec,
    elec_price_domestic,
    v_p_d_e,
    rural_urban_classification_2011_3_fold
  ) %>%
  # Add data quality checks
  filter(
    !is.na(domestic_cons_elec),
    !is.na(elec_price_domestic),
    !is.na(v_p_d_e),
    domestic_cons_elec > 0,
    elec_price_domestic > 0
  ) %>%
  # Create log variables and standardize volatility
  mutate(
    log_consumption = log(domestic_cons_elec),
    log_price = log(elec_price_domestic),
    volatility = scale(v_p_d_e)[,1]
  )

# Function to check data structure before panel conversion
check_data_structure <- function(data, group_name) {
  # Filter for group
  group_data <- data %>%
    filter(rural_urban_classification_2011_3_fold == group_name)
  
  # Basic checks
  cat("\nData structure for", group_name, ":\n")
  cat("----------------------------------------\n")
  cat("Total observations:", nrow(group_data), "\n")
  cat("Unique local authorities:", length(unique(group_data$local_authority)), "\n")
  cat("Time period:", min(group_data$year), "to", max(group_data$year), "\n")
  
  # Check for balanced panel
  n_years <- length(unique(group_data$year))
  n_authorities <- length(unique(group_data$local_authority))
  expected_obs <- n_years * n_authorities
  
  # Create cross-tabulation
  panel_table <- table(group_data$local_authority, group_data$year)
  
  cat("\nPanel structure:\n")
  cat("Number of years:", n_years, "\n")
  cat("Number of authorities:", n_authorities, "\n")
  cat("Expected observations if balanced:", expected_obs, "\n")
  cat("Actual observations:", nrow(group_data), "\n")
  cat("Panel is", ifelse(nrow(group_data) == expected_obs, "balanced", "unbalanced"), "\n")
  
  # Check for gaps in time series
  has_gaps <- any(apply(panel_table, 1, function(x) any(diff(which(x > 0)) > 1)))
  cat("Time series has gaps:", has_gaps, "\n")
  
  # Check for missing values
  cat("\nMissing values:\n")
  print(colSums(is.na(group_data)))
  
  # Variable summaries
  cat("\nVariable summaries:\n")
  print(summary(group_data[c("log_consumption", "log_price", "volatility")]))
  
  # Return structure information
  return(list(
    n_obs = nrow(group_data),
    n_auth = n_authorities,
    n_years = n_years,
    balanced = nrow(group_data) == expected_obs,
    has_gaps = has_gaps,
    panel_table = panel_table
  ))
}

# Check each group
cat("\nChecking data structure for each group...\n")
urban_check <- check_data_structure(panel_data, "Predominantly Urban")
rural_check <- check_data_structure(panel_data, "Predominantly Rural")
mixed_check <- check_data_structure(panel_data, "Urban with Significant Rural")

# Now convert to panel data format
panel_data <- pdata.frame(panel_data, index = c("local_authority", "year"))

# Modified estimation function
estimate_group_model <- function(data, group) {
  # Extract group data
  group_data <- data[data$rural_urban_classification_2011_3_fold == group,]
  
  cat("\nEstimating model for group:", group, "\n")
  cat("Number of observations:", nrow(group_data), "\n")
  cat("Number of unique authorities:", length(unique(group_data$local_authority)), "\n")
  
  tryCatch({
    model <- pvargmm(
      # Include all variables as endogenous
      dependent_vars = c("log_consumption", "log_price", "volatility"),
      lags = 1,
      transformation = "fd",
      data = group_data,
      panel_identifier = c("local_authority", "year"),
      steps = "twostep",
      system_instruments = TRUE,
      max_instr_dependent_vars = 2,
      min_instr_dependent_vars = 1,
      collapse = TRUE
    )
    
    cat("Model estimation successful\n")
    return(model)
    
  }, error = function(e) {
    cat("Error in model estimation:", e$message, "\n")
    return(NULL)
  })
}

# Try to estimate models
cat("\nAttempting model estimation...\n")
urban_model <- estimate_group_model(panel_data, "Predominantly Urban")
rural_model <- estimate_group_model(panel_data, "Predominantly Rural")
mixed_model <- estimate_group_model(panel_data, "Urban with Significant Rural")

# ANALYSIS ====

summary(urban_model)
summary(rural_model)
summary(mixed_model)
