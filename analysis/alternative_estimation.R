# Load required packages
library(tidyverse)
library(panelvar)
library(plm)
library(janitor)
library(reshape2)

# ESTIMATION ====

# Import and clean data - keep year as numeric
data <- read_csv("final_data/master/cons_price_vol_with_rural_urban.csv") %>%
  clean_names()

str(data)

## Basic Panel VAR with Fixed Effects ----

### Functions ----

#### Estimation ----

# Data preparation function
prepare_pvar_data <- function(data) {
  # Create basic dataframe with necessary variables
  pvar_data <- data %>%
    as.data.frame() %>%
    select(
      id = code,
      year,
      consumption = domestic_cons_elec,
      price = elec_price_domestic,
      volatility = v_p_d_e,
      rural_urban = rural_urban_classification_2011_3_fold
    ) %>%
    # Ensure numeric variables
    mutate(
      year = as.numeric(year),
      consumption = log(as.numeric(consumption)),
      price = as.numeric(price),
      volatility = as.numeric(volatility),
      id = as.character(id)
    ) %>%
    # Remove missing values
    drop_na()
  
  return(pvar_data)
}

# Function to estimate basic panel VAR
estimate_basic_pvar <- function(data, rural_urban_group = NULL) {
  # If group is specified, filter data
  if (!is.null(rural_urban_group)) {
    data <- data[data$rural_urban == rural_urban_group,]
  }
  
  # Keep only variables needed for the model
  model_data <- data %>%
    select(id, year, volatility, price, consumption)
  
  # Estimate panel VAR with fixed effects
  tryCatch({
    model <- pvarfeols(
      dependent_vars = c("volatility", "price", "consumption"),
      lags = 1,
      transformation = "demean",
      data = model_data,
      panel_identifier = c("id", "year")
    )
    return(model)
  }, error = function(e) {
    message("Error in model estimation: ", e$message)
    return(NULL)
  })
}

#### Visualisation ----

# Function to create IRF plots with confidence intervals
plot_irf <- function(irf_object, shock_var, response_var, 
                     ci_lower = NULL, ci_upper = NULL, title = NULL) {
  # Get IRF values
  irf_values <- irf_object[[shock_var]][, which(colnames(irf_object[[shock_var]]) == response_var)]
  n_periods <- length(irf_values)
  
  # Create data frame for plotting
  plot_data <- data.frame(
    period = 0:(n_periods-1),
    irf = irf_values
  )
  
  # Add confidence intervals if provided
  if(!is.null(ci_lower) && !is.null(ci_upper)) {
    plot_data$ci_lower <- ci_lower[[shock_var]][, which(colnames(ci_lower[[shock_var]]) == response_var)]
    plot_data$ci_upper <- ci_upper[[shock_var]][, which(colnames(ci_upper[[shock_var]]) == response_var)]
  }
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = period, y = irf)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red")
  
  # Add confidence intervals if available
  if(!is.null(ci_lower) && !is.null(ci_upper)) {
    p <- p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                         alpha = 0.2, fill = "blue")
  }
  
  # Add labels and theme
  p <- p + labs(
    title = ifelse(is.null(title), 
                   paste("Response of", response_var, "to", shock_var, "shock"),
                   title),
    x = "Periods",
    y = "Response"
  ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Function to compare IRFs across models with confidence intervals
compare_irfs <- function(model1_irf, model2_irf, shock_var, response_var, 
                         model1_ci = NULL, model2_ci = NULL,
                         model1_name = "Model 1", model2_name = "Model 2") {
  
  # Get IRF values for both models
  irf1_values <- model1_irf[[shock_var]][, which(colnames(model1_irf[[shock_var]]) == response_var)]
  irf2_values <- model2_irf[[shock_var]][, which(colnames(model2_irf[[shock_var]]) == response_var)]
  n_periods <- length(irf1_values)
  
  # Create data frame for plotting
  plot_data <- data.frame(
    period = rep(0:(n_periods-1), 2),
    irf = c(irf1_values, irf2_values),
    model = factor(c(rep(model1_name, length(irf1_values)),
                     rep(model2_name, length(irf2_values))))
  )
  
  # Add confidence intervals if provided
  if(!is.null(model1_ci) && !is.null(model2_ci)) {
    ci1_lower <- model1_ci$Lower[[shock_var]][, which(colnames(model1_ci$Lower[[shock_var]]) == response_var)]
    ci1_upper <- model1_ci$Upper[[shock_var]][, which(colnames(model1_ci$Upper[[shock_var]]) == response_var)]
    ci2_lower <- model2_ci$Lower[[shock_var]][, which(colnames(model2_ci$Lower[[shock_var]]) == response_var)]
    ci2_upper <- model2_ci$Upper[[shock_var]][, which(colnames(model2_ci$Upper[[shock_var]]) == response_var)]
    
    plot_data$ci_lower <- c(ci1_lower, ci2_lower)
    plot_data$ci_upper <- c(ci1_upper, ci2_upper)
  }
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = period, y = irf, color = model)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  
  # Add confidence intervals if available
  if(!is.null(model1_ci) && !is.null(model2_ci)) {
    p <- p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                             fill = model), alpha = 0.2)
  }
  
  # Add labels and theme
  p <- p + labs(
    title = paste("Response of", response_var, "to", shock_var, "shock"),
    x = "Periods",
    y = "Response",
    color = "Model",
    fill = "Model"
  ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  return(p)
}

### Execution Block ====

# Format panel data
pvar_data <- prepare_pvar_data(data)
print(str(pvar_data))

# Estimate models for each group
all_areas_model <- estimate_basic_pvar(pvar_data)
urban_model <- estimate_basic_pvar(pvar_data, "Predominantly Urban")
rural_model <- estimate_basic_pvar(pvar_data, "Predominantly Rural")

# # Analyze full sample results
all_areas_plots <- analyze_model_results(all_areas_results, "Full Sample")

irf_results <- oirf(all_areas_model, n.ahead = 20)

# Calculate confidence intervals using bootstrap
ci_results <- bootstrap_irf(all_areas_model, 
                            typeof_irf = "OIRF",
                            n.ahead = 20,
                            nof_Nstar_draws = 500,
                            confidence.band = 0.95,
                            mc.cores = parallel::detectCores() - 1)

# Create plot with confidence intervals
plot_irf(irf_results, "price", "consumption",
         ci_results$Lower, ci_results$Upper)

# Model Comparison: Urban vs Rural

# First calculate IRFs for both models
irf_model1 <- oirf(urban_model, n.ahead = 10)
irf_model2 <- oirf(rural_model, n.ahead = 10)  

# Calculate confidence intervals for both models if you haven't already
ci_model1 <- bootstrap_irf(urban_model, 
                           typeof_irf = "OIRF",
                           n.ahead = 10,
                           nof_Nstar_draws = 500,
                           confidence.band = 0.95,
                           mc.cores = parallel::detectCores() - 1)

ci_model2 <- bootstrap_irf(rural_model,
                           typeof_irf = "OIRF", 
                           n.ahead = 10,
                           nof_Nstar_draws = 500,
                           confidence.band = 0.95,
                           mc.cores = parallel::detectCores() - 1)

# Compare IRFs with confidence intervals
compare_irfs(irf_model1, irf_model2, 
             shock_var = "price",
             response_var = "consumption",
             model1_ci = ci_model1,
             model2_ci = ci_model2,
             model1_name = "Urban",
             model2_name = "Rural")

compare_irfs(irf_model1, irf_model2, 
             shock_var = "volatility",
             response_var = "consumption",
             model1_ci = ci_model1,
             model2_ci = ci_model2,
             model1_name = "Urban",
             model2_name = "Rural")


## GMM Estimation ====

### Functions ----

# Modified GMM estimation function with more detailed error handling
estimate_gmm_pvar <- function(data, rural_urban_group = NULL) {
  # If group is specified, filter data
  if (!is.null(rural_urban_group)) {
    data <- data[data$rural_urban == rural_urban_group,]
  }
  
  # Keep only variables needed for the model
  model_data <- data %>%
    select(id, year, volatility, price, consumption)
  
  # Print dimensions for debugging
  print(paste("Dimensions of data for", 
              ifelse(is.null(rural_urban_group), "all areas", rural_urban_group)))
  print(paste("N =", length(unique(model_data$id))))
  print(paste("T =", length(unique(model_data$year))))
  
  # Estimate panel VAR with GMM - more conservative settings
  tryCatch({
    model <- pvargmm(
      dependent_vars = c("volatility", "price", "consumption"),
      lags = 1,
      transformation = "fd",
      data = model_data,
      panel_identifier = c("id", "year"),
      steps = "twostep",
      system_instruments = FALSE,
      collapse = TRUE,
      max_instr_dependent_vars = 2, 
      min_instr_dependent_vars = 2L,
      tol = 1e-07
    )
    return(model)
  }, error = function(e) {
    message("Error in GMM model estimation: ", e$message)
    return(NULL)
  })
}

# Modified function to calculate IRFs with safety checks
calculate_irfs <- function(model, n.ahead = 10, bootstrap = FALSE) {
  if (is.null(model)) {
    message("Model is NULL, cannot calculate IRFs")
    return(NULL)
  }
  
  # First try to calculate basic IRFs
  tryCatch({
    irf_results <- oirf(model, n.ahead = n.ahead)
    
    if (!bootstrap) {
      return(list(irf = irf_results, ci = NULL))
    }
    
    # If bootstrap requested, try with minimal settings first
    ci_results <- bootstrap_irf(model, 
                                typeof_irf = "OIRF",
                                n.ahead = n.ahead,
                                nof_Nstar_draws = 50,  # Reduced number of draws
                                confidence.band = 0.95,
                                mc.cores = 1)  # Single core for testing
    
    return(list(irf = irf_results, ci = ci_results))
    
  }, error = function(e) {
    message("Error in IRF calculation: ", e$message)
    return(NULL)
  })
}

### Execution Block ----

# Estimate models
gmm_all_areas <- estimate_gmm_pvar(pvar_data)
gmm_urban <- estimate_gmm_pvar(pvar_data, "Predominantly Urban")
gmm_rural <- estimate_gmm_pvar(pvar_data, "Predominantly Rural")

# Calculate IRFs - first without bootstrap to check basic functionality
irfs_all <- calculate_irfs(gmm_all_areas, n.ahead = 10, bootstrap = FALSE)
irfs_urban <- calculate_irfs(gmm_urban, n.ahead = 10, bootstrap = FALSE)
irfs_rural <- calculate_irfs(gmm_rural, n.ahead = 10, bootstrap = FALSE)

# If basic IRFs work, try with bootstrap (one model at a time)
if (!is.null(irfs_all$irf)) {
  print("Attempting bootstrap for all areas model...")
  irfs_all <- calculate_irfs(gmm_all_areas, n.ahead = 10, bootstrap = TRUE)
}

if (!is.null(irfs_urban$irf)) {
  print("Attempting bootstrap for urban model...")
  irfs_urban <- calculate_irfs(gmm_urban, n.ahead = 10, bootstrap = TRUE)
}

if (!is.null(irfs_rural$irf)) {
  print("Attempting bootstrap for rural model...")
  irfs_rural <- calculate_irfs(gmm_rural, n.ahead = 10, bootstrap = TRUE)
}


# Price Shock
if (!is.null(irfs_urban$irf) && !is.null(irfs_rural$irf)) {
  # For basic IRFs without confidence intervals
  compare_irfs(irfs_urban$irf, irfs_rural$irf, 
               shock_var = "price",
               response_var = "consumption",
               model1_name = "Urban",
               model2_name = "Rural")
  
  # If we have bootstrap results, create plots with confidence intervals
  if (!is.null(irfs_urban$ci) && !is.null(irfs_rural$ci)) {
    compare_irfs(irfs_urban$irf, irfs_rural$irf, 
                 shock_var = "price",
                 response_var = "consumption",
                 model1_ci = irfs_urban$ci,
                 model2_ci = irfs_rural$ci,
                 model1_name = "Urban",
                 model2_name = "Rural")
  }
}

# Volatility Shock
if (!is.null(irfs_urban$irf) && !is.null(irfs_rural$irf)) {
  # For basic IRFs without confidence intervals
  compare_irfs(irfs_urban$irf, irfs_rural$irf, 
               shock_var = "volatility",
               response_var = "consumption",
               model1_name = "Urban",
               model2_name = "Rural")
  
  # If we have bootstrap results, create plots with confidence intervals
  if (!is.null(irfs_urban$ci) && !is.null(irfs_rural$ci)) {
    compare_irfs(irfs_urban$irf, irfs_rural$irf, 
                 shock_var = "volatility",
                 response_var = "consumption",
                 model1_ci = irfs_urban$ci,
                 model2_ci = irfs_rural$ci,
                 model1_name = "Urban",
                 model2_name = "Rural")
  }
}



