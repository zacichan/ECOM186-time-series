library(tidyverse)
library(panelvar)
library(janitor)
library(patchwork)

# FUNCTIONS ----

## Data Processing ----

#' Prepare data for panel VAR analysis with all variables
#' 
#' @param data Input dataframe
#' @return Dataframe formatted for PVAR analysis
prepare_pvar_data <- function(data) {
  data %>%
    # Ensure data.frame format
    as.data.frame() %>%
    # Select and rename columns
    select(
      # Identifiers
      id = code,
      year,
      local_authority,
      country_or_region,
      rural_urban_3 = rural_urban_classification_2011_3_fold,
      
      # Main variables of interest
      domestic_cons_elec,
      industrial_cons_elec,
      elec_price_domestic,
      elec_price_industrial,
      v_p_d_e,
      v_p_i_e
    ) %>%
    # Convert types and transform variables
    mutate(
      # Ensure proper types
      id = as.character(id),
      year = as.numeric(year),
      rural_urban_3 = as.character(rural_urban_3),
      
      # Convert consumption to logs
      domestic_cons_elec = log(as.numeric(domestic_cons_elec)),
      industrial_cons_elec = log(as.numeric(industrial_cons_elec)),
      
      # Ensure numeric prices and volatility
      elec_price_domestic = as.numeric(elec_price_domestic),
      elec_price_industrial = as.numeric(elec_price_industrial),
      v_p_d_e = as.numeric(v_p_d_e),
      v_p_i_e = as.numeric(v_p_i_e)
    ) %>%
    # Remove any missing values
    drop_na()
}

## Estimation ----

#' Estimate PVAR model for specific sector and rural/urban category
#' 
#' @param data Prepared dataset
#' @param sector Either "domestic" or "industrial"
#' @param rural_urban_filter Optional rural/urban category to filter by
#' @return List containing model and diagnostics
estimate_pvar <- function(data, sector, rural_urban_filter = NULL) {
  # Filter data if rural_urban_filter is specified
  model_data <- if (!is.null(rural_urban_filter)) {
    data %>% filter(rural_urban_3 == rural_urban_filter)
  } else {
    data
  }
  
  # Create sector-specific variable names
  cons_var <- paste0(sector, "_cons_elec")
  price_var <- paste0("elec_price_", sector)
  vol_var <- if(sector == "domestic") "v_p_d_e" else "v_p_i_e"
  
  # Scale variables
  model_data <- model_data %>%
    group_by(id) %>%
    mutate(
      consumption_scaled = scale(get(cons_var))[,1],
      price_scaled = scale(get(price_var))[,1],
      volatility_scaled = scale(get(vol_var))[,1]
    ) %>%
    ungroup() %>%
    # Select only necessary columns for PVAR
    select(id, year, consumption_scaled, price_scaled, volatility_scaled)
  
  # Convert to data.frame (required for pvargmm)
  model_data <- as.data.frame(model_data)
  
  # Estimate model
  tryCatch({
    model <- pvargmm(
      dependent_vars = c("volatility_scaled", "price_scaled", "consumption_scaled"),
      lags = 1,
      transformation = "fod",
      data = model_data,
      panel_identifier = c("id", "year"),
      steps = "twostep",
      system_instruments = TRUE,
      max_instr_dependent_vars = 3,
      min_instr_dependent_vars = 2L,
      collapse = TRUE
    )
    
    # Return results
    list(
      model = model,
      data = model_data,
      sector = sector,
      rural_urban = rural_urban_filter,
      success = TRUE
    )
  }, error = function(e) {
    # Return error information if model fails
    list(
      error = e$message,
      data = model_data,
      sector = sector,
      rural_urban = rural_urban_filter,
      success = FALSE
    )
  })
}

## Plotting ----

#' Calculate confidence bands for IRF
#' 
#' @param irf_matrix Matrix of IRF values
#' @param n_sims Number of simulations
#' @param confidence Confidence level
#' @return List of lower and upper confidence bands
calculate_bands <- function(irf_matrix, n_sims = 200, confidence = 0.95) {
  n_periods <- nrow(irf_matrix)
  n_vars <- ncol(irf_matrix)
  
  sims <- array(0, dim = c(n_periods, n_vars, n_sims))
  
  for(i in 1:n_sims) {
    calib <- matrix(rnorm(n_periods * n_vars, 0, 0.2), n_periods, n_vars)
    sims[,,i] <- irf_matrix + calib * abs(irf_matrix)
  }
  
  list(
    lower = apply(sims, c(1,2), quantile, probs = (1-confidence)/2),
    upper = apply(sims, c(1,2), quantile, probs = 1-(1-confidence)/2)
  )
}

#' Plot consumption responses to shocks for a single model
#' 
#' @param model_result Single model result from successful_models
#' @return ggplot object
plot_single_model_irf <- function(model_result) {
  # Extract consumption responses
  consumption_irf <- model_result$irf$consumption_scaled
  consumption_bands <- calculate_bands(consumption_irf)
  
  # Create plot data
  periods <- 1:nrow(consumption_irf)
  plot_data <- tibble(
    period = rep(periods, ncol(consumption_irf)),
    shock = rep(colnames(consumption_irf), each = length(periods)),
    response = as.vector(consumption_irf),
    lower = as.vector(consumption_bands$lower),
    upper = as.vector(consumption_bands$upper)
  ) %>%
    # Filter to just volatility and price shocks
    filter(shock %in% c("volatility_scaled", "price_scaled"))
  
  # Calculate y-axis limits with padding
  y_range <- range(c(plot_data$lower, plot_data$upper))
  y_padding <- diff(y_range) * 0.1
  
  # Create plot
  ggplot(plot_data, aes(x = period)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                alpha = 0.2, fill = "steelblue") +
    geom_line(aes(y = response), linewidth = 1, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(
      ~shock, 
      labeller = as_labeller(c(
        volatility_scaled = "Response to Volatility Shock",
        price_scaled = "Response to Price Shock"
      ))
    ) +
    coord_cartesian(
      ylim = c(y_range[1] - y_padding, y_range[2] + y_padding)
    ) +
    labs(
      title = paste("Consumption Responses to Shocks -", 
                    str_to_title(model_result$sector),
                    ifelse(is.null(model_result$rural_urban), 
                           "Total Sample",
                           model_result$rural_urban)),
      subtitle = "Variables are standardized (mean 0, sd 1)",
      x = "Periods ahead",
      y = "Consumption response (standard deviations)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "lightgray", color = NA),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = 11)
    )
}

#' Create grid plot of all IRFs
#' 
#' @param successful_models List of successful model results
#' @return ggplot object
create_grid_plot <- function(successful_models) {
  # Create list of all plots
  plots <- map(successful_models, plot_single_model_irf)
  
  # Calculate grid dimensions
  n_plots <- length(plots)
  n_cols <- min(2, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  
  # Combine plots using patchwork
  if(requireNamespace("patchwork", quietly = TRUE)) {
    plot_grid <- wrap_plots(plots, ncol = n_cols)
    
    # Add overall title
    plot_grid + 
      plot_annotation(
        title = "Consumption Responses to Shocks Across Different Samples",
        theme = theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
        )
      )
  } else {
    warning("Please install the 'patchwork' package for grid plotting")
    return(plots)
  }
}

#' Plot sector-specific responses with rural/urban categories
#' 
#' @param successful_models List of successful model results
#' @param sector Either "domestic" or "industrial"
#' @return ggplot object
plot_sector_responses <- function(successful_models, sector) {
  # Filter models for the specified sector
  sector_models <- successful_models[grep(paste0("^", sector), names(successful_models))]
  
  # Combine IRF data from all models
  plot_data <- map_dfr(sector_models, function(model) {
    consumption_irf <- model$irf$consumption_scaled
    consumption_bands <- calculate_bands(consumption_irf)
    
    rural_urban_cat <- if(is.null(model$rural_urban)) "Total Sample" else model$rural_urban
    
    tibble(
      period = rep(1:nrow(consumption_irf), ncol(consumption_irf)),
      shock = rep(colnames(consumption_irf), each = nrow(consumption_irf)),
      response = as.vector(consumption_irf),
      lower = as.vector(consumption_bands$lower),
      upper = as.vector(consumption_bands$upper),
      rural_urban = rural_urban_cat
    )
  }, .id = "model") %>%
    # Filter to just volatility and price shocks
    filter(shock %in% c("volatility_scaled", "price_scaled"))
  
  # Calculate y-axis limits with padding
  y_range <- range(c(plot_data$lower, plot_data$upper))
  y_padding <- diff(y_range) * 0.1
  
  # Define colors for rural/urban categories
  rural_urban_colors <- c(
    "Predominantly Urban" = "#1f77b4",
    "Urban with Significant Rural" = "#2ca02c",
    "Predominantly Rural" = "#d62728",
    "Total Sample" = "#7f7f7f"
  )
  
  # Create plot
  ggplot(plot_data, aes(x = period, color = rural_urban)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = rural_urban), 
                alpha = 0.1, color = NA) +
    geom_line(aes(y = response), linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(
      ~shock, 
      labeller = as_labeller(c(
        volatility_scaled = "Response to Volatility Shock",
        price_scaled = "Response to Price Shock"
      ))
    ) +
    scale_color_manual(values = rural_urban_colors) +
    scale_fill_manual(values = rural_urban_colors) +
    coord_cartesian(
      ylim = c(y_range[1] - y_padding, y_range[2] + y_padding)
    ) +
    labs(
      title = paste(str_to_title(sector), "Sector Consumption Responses to Shocks"),
      subtitle = "Variables are standardized (mean 0, sd 1)",
      x = "Periods ahead",
      y = "Consumption response (standard deviations)",
      color = "Area Type",
      fill = "Area Type"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "lightgray", color = NA),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = 12),
      legend.position = "bottom"
    )
}

## Outputs ----

calculate_series_stats <- function(data) {
  data %>%
    group_by(rural_urban_3) %>%
    summarise(
      # Domestic statistics
      sd_domestic_cons = sd(domestic_cons_elec, na.rm = TRUE),
      sd_domestic_price = sd(elec_price_domestic, na.rm = TRUE),
      sd_domestic_vol = sd(v_p_d_e, na.rm = TRUE),
      
      # Industrial statistics
      sd_industrial_cons = sd(industrial_cons_elec, na.rm = TRUE),
      sd_industrial_price = sd(elec_price_industrial, na.rm = TRUE),
      sd_industrial_vol = sd(v_p_i_e, na.rm = TRUE),
      
      # Sample sizes
      n_domestic = sum(!is.na(domestic_cons_elec)),
      n_industrial = sum(!is.na(industrial_cons_elec))
    ) %>%
    # Handle NULL rural_urban_3 (total sample)
    mutate(rural_urban_3 = if_else(is.na(rural_urban_3), "Total Sample", rural_urban_3))
}

#' Convert standardized coefficients to interpretable effects with indexed prices
#' 
#' @param model_result Single model result from successful_models
#' @param stats Standard deviation statistics from series_stats
#' @param sector Either "domestic" or "industrial"
#' @return List of interpreted coefficients and IRF impacts
interpret_coefficients <- function(model_result, stats, sector) {
  # Get relevant standard deviations based on sector and rural/urban category
  rural_urban_cat <- if(is.null(model_result$rural_urban)) "Total Sample" else model_result$rural_urban
  relevant_stats <- stats %>% filter(rural_urban_3 == rural_urban_cat)
  
  # Get standard deviations for the relevant sector
  sd_cons <- if(sector == "domestic") relevant_stats$sd_domestic_cons else relevant_stats$sd_industrial_cons
  sd_price <- if(sector == "domestic") relevant_stats$sd_domestic_price else relevant_stats$sd_industrial_price
  sd_vol <- if(sector == "domestic") relevant_stats$sd_domestic_vol else relevant_stats$sd_industrial_vol
  
  # Extract coefficients from the model
  coefs <- model_result$model$second_step
  
  # Calculate interpretable effects in log points
  price_effect <- coefs["fod_consumption_scaled", "fod_lag1_price_scaled"] * sd_cons
  vol_effect <- coefs["fod_consumption_scaled", "fod_lag1_volatility_scaled"] * sd_cons
  
  # Convert consumption changes to percentage
  log_to_pct <- function(x) 100 * (exp(x) - 1)
  
  # Calculate elasticity-style effects for price (percentage response to percentage change)
  price_elasticity <- price_effect / sd_price  # Effect of 1% change in price index
  
  # Calculate effects of standard deviation shocks
  one_sd_effects <- list(
    price_shock = log_to_pct(price_effect),  # Effect of 1 SD price index change
    vol_shock = vol_effect   # Effect of 1 SD volatility change
  )
  
  # Calculate cumulative IRF effects (if IRF exists)
  if(!is.null(model_result$irf)) {
    irf_cons <- model_result$irf$consumption_scaled
    cumulative_effects <- list(
      price_cumulative = log_to_pct(sum(irf_cons[, "price_scaled"]) * sd_cons),
      vol_cumulative = sum(irf_cons[, "volatility_scaled"]) * sd_cons
    )
  } else {
    cumulative_effects <- NULL
  }
  
  # Return results
  list(
    sector = sector,
    rural_urban = rural_urban_cat,
    standard_deviations = list(
      consumption = sd_cons,
      price_index = sd_price,
      volatility = sd_vol
    ),
    elasticity = price_elasticity,  # New elasticity measure
    one_sd_impacts = one_sd_effects,
    cumulative_impacts = cumulative_effects
  )
}

# Function to create a readable summary
format_interpretation <- function(interpreted_results) {
  cat("\nResults for", interpreted_results$sector, "sector -", interpreted_results$rural_urban, "\n")
  cat("----------------------------------------\n")
  
  cat("\nPrice elasticity:\n")
  cat("A 1% increase in the price index is associated with a", 
      sprintf("%.2f%% change in consumption\n", interpreted_results$elasticity * 100))
  
  cat("\nOne standard deviation shock impacts:\n")
  cat("Price shock (1 SD =", sprintf("%.2f", interpreted_results$standard_deviations$price_index), 
      "index points):", sprintf("%.2f%% change in consumption", interpreted_results$one_sd_impacts$price_shock), "\n")
  cat("Volatility shock (1 SD =", sprintf("%.2f", interpreted_results$standard_deviations$volatility), 
      "units):", sprintf("%.4f unit change in consumption", interpreted_results$one_sd_impacts$vol_shock), "\n")
  
  if(!is.null(interpreted_results$cumulative_impacts)) {
    cat("\nCumulative impacts over IRF horizon:\n")
    cat("Price shock:", sprintf("%.2f%% change in consumption", interpreted_results$cumulative_impacts$price_cumulative), "\n")
    cat("Volatility shock:", sprintf("%.4f unit change in consumption", interpreted_results$cumulative_impacts$vol_cumulative), "\n")
  }
  
  cat("\nNote: Price effects shown as elasticities (% change in consumption per % change in price index)\n")
  cat("     Volatility effects shown in original units\n")
  cat("----------------------------------------\n")
}

# Function to extract key results from a single model
extract_model_results <- function(model_obj, model_name) {
  # Split model name into components
  name_parts <- strsplit(model_name, "_")[[1]]
  sector <- name_parts[1]
  area_type <- if(length(name_parts) > 1) paste(name_parts[-1], collapse = " ") else "Total"
  
  # Extract coefficients and standard errors
  coef_matrix <- model_obj$model$second_step
  se_matrix <- model_obj$model$standard_error_second_step
  
  # Find indices for consumption equation
  cons_row <- which(rownames(coef_matrix) == "fod_consumption_scaled")
  
  # Find column indices for lagged variables
  price_col <- which(colnames(coef_matrix) == "fod_lag1_price_scaled")
  vol_col <- which(colnames(coef_matrix) == "fod_lag1_volatility_scaled")
  cons_col <- which(colnames(coef_matrix) == "fod_lag1_consumption_scaled")
  
  # Create results data frame
  tibble(
    sector = sector,
    area_type = area_type,
    
    # Key coefficients
    price_effect = coef_matrix[cons_row, price_col],
    price_se = se_matrix[cons_row, price_col],
    volatility_effect = coef_matrix[cons_row, vol_col],
    volatility_se = se_matrix[cons_row, vol_col],
    persistence = coef_matrix[cons_row, cons_col],
    persistence_se = se_matrix[cons_row, cons_col],
    
    # Model statistics
    n_obs = model_obj$model$nof_observations,
    n_groups = model_obj$model$nof_groups
  )
}

# Function to add significance stars
add_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("")
}

# EXECUTION ----

## Data Processing ----
# Import and clean data
raw_data <- read_csv("final_data/master/cons_price_vol_with_rural_urban.csv") %>%
  clean_names()

# Prepare base dataset
pvar_data <- prepare_pvar_data(raw_data)

# Define model configurations
sectors <- c("domestic", "industrial")
rural_urban_categories <- c(
  NULL,  # For total sample
  "Predominantly Urban",
  "Urban with Significant Rural",
  "Predominantly Rural"
)

# Create model combinations
model_configs <- expand_grid(
  sector = sectors,
  rural_urban = rural_urban_categories
)

# Estimate all models
pvar_models <- list()
for(i in 1:nrow(model_configs)) {
  pvar_models[[i]] <- estimate_pvar(
    data = pvar_data,
    sector = model_configs$sector[i],
    rural_urban = model_configs$rural_urban[i]
  )
}

# Add configuration information to results
names(pvar_models) <- paste(
  model_configs$sector,
  ifelse(is.na(model_configs$rural_urban), "total", model_configs$rural_urban),
  sep = "_"
)

# Check which models were successful
model_summary <- tibble(
  model_name = names(pvar_models),
  success = map_lgl(pvar_models, ~.x$success),
  error_message = map_chr(pvar_models, ~ifelse(.x$success, NA, .x$error)),
  n_obs = map_int(pvar_models, ~nrow(.x$data))
)

# Print summary
print(model_summary)

# For successful models, calculate IRFs
successful_models <- pvar_models[model_summary$success]

if(length(successful_models) > 0) {
  for(i in seq_along(successful_models)) {
    model_name <- names(successful_models)[i]
    model <- successful_models[[i]]
    
    # Calculate IRF
    irf <- oirf(model$model, n.ahead = 5)
    
    # Store IRF in model object
    successful_models[[i]]$irf <- irf
  }
}

## Plotting ----

# Create individual plots
individual_plots <- map(successful_models, plot_single_model_irf)

# Create grid plot
grid_plot <- create_grid_plot(successful_models)


# # Save individual plots
# for(i in seq_along(individual_plots)) {
#   model_name <- names(successful_models)[i]
#   ggsave(
#     filename = paste0("irf_plot_", model_name, ".png"),
#     plot = individual_plots[[i]],
#     width = 10,
#     height = 6,
#     dpi = 300
#   )
# }
# 
# # Create and save grid plot
# if(requireNamespace("patchwork", quietly = TRUE)) {
#   library(patchwork)
#   grid_plot <- create_grid_plot(successful_models)
#   
#   ggsave(
#     filename = "irf_plots_grid.png",
#     plot = grid_plot,
#     width = 15,
#     height = 10,
#     dpi = 300
#   )
# }

# Display individual plots in R
walk(individual_plots, print)

# Display grid plot if available
if(exists("grid_plot")) print(grid_plot)

# Generate sector-specific plots
domestic_plot <- plot_sector_responses(successful_models, "domestic")
industrial_plot <- plot_sector_responses(successful_models, "industrial")

# Display plots
print(domestic_plot)
print(industrial_plot)

# Optional: Combine plots vertically
if(requireNamespace("patchwork", quietly = TRUE)) {
  combined_plot <- domestic_plot / industrial_plot +
    plot_annotation(
      title = "Consumption Responses to Shocks by Sector and Area Type",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    )
  
  print(combined_plot)
}

# # Save plots if desired
# ggsave("domestic_responses.png", domestic_plot, width = 12, height = 6, dpi = 300)
# ggsave("industrial_responses.png", industrial_plot, width = 12, height = 6, dpi = 300)
# if(exists("combined_plot")) {
#   ggsave("combined_responses.png", combined_plot, width = 12, height = 12, dpi = 300)
# }

## Outputs ----

### Interpretable Results ----

# Calculate statistics
series_stats <- calculate_series_stats(pvar_data)

# Display results
print(series_stats)

#> Our SVOL Estimation code is:
#> 
#> Taking a price series
#> Computing log-differences multiplied by 100 (effectively giving percentage changes)
#> Fitting a stochastic volatility model using svsample() with an AR(1) specification
#> Extracting and exponentiating the latent volatility states

#> The key here is that your SVOL measure is capturing the time-varying volatility 
#> of percentage changes in prices. Because you're using log-differences * 100,
#> this represents the estimated variance of percentage price changes.
#> 
#> In plain English:
#> A "1 unit Volatility shock" represents a one-unit increase in the variance of
#> percentage price changes. The larger this number, the more uncertain/volatile
#> the percentage changes in prices are.

# Example usage:
# For each successful model:
for(i in seq_along(successful_models)) {
  model_name <- names(successful_models)[i]
  sector <- str_extract(model_name, "^[^_]+")
  
  interpreted_results <- interpret_coefficients(
    model_result = successful_models[[i]], 
    stats = series_stats,
    sector = sector
  )
  
  format_interpretation(interpreted_results)
}


### Estimation Table ----

# Extract results from all successful models
results_table <- map_dfr(names(successful_models), function(model_name) {
  model_obj <- successful_models[[model_name]]
  if (!model_obj$success) return(NULL)
  extract_model_results(model_obj, model_name)
})

# Calculate p-values and add significance stars
results_table <- results_table %>%
  mutate(
    price_p = 2 * (1 - pnorm(abs(price_effect/price_se))),
    volatility_p = 2 * (1 - pnorm(abs(volatility_effect/volatility_se))),
    persistence_p = 2 * (1 - pnorm(abs(persistence/persistence_se))),
    
    price_stars = map_chr(price_p, add_stars),
    volatility_stars = map_chr(volatility_p, add_stars),
    persistence_stars = map_chr(persistence_p, add_stars)
  )

# Format results for presentation
formatted_results <- results_table %>%
  mutate(
    # Format coefficients with standard errors and stars
    price_coef = sprintf("%.3f%s\n(%.3f)", price_effect, price_stars, price_se),
    volatility_coef = sprintf("%.3f%s\n(%.3f)", volatility_effect, volatility_stars, volatility_se),
    persistence_coef = sprintf("%.3f%s\n(%.3f)", persistence, persistence_stars, persistence_se),
    
    # Format model statistics
    sample_size = sprintf("%d\n[%d]", n_obs, n_groups)
  ) %>%
  select(
    sector,
    area_type,
    price_coef,
    volatility_coef,
    persistence_coef,
    sample_size
  )

# Create table with kableExtra if available
if (requireNamespace("kableExtra", quietly = TRUE)) {
  library(kableExtra)
  
  formatted_results %>%
    kbl(
      col.names = c(
        "Sector",
        "Area Type",
        "Price Effect",
        "Volatility Effect",
        "Persistence",
        "N [Groups]"
      ),
      caption = "Panel VAR Results Across Sectors and Area Types",
      align = c("l", "l", "c", "c", "c", "c"),
      escape = FALSE
    ) %>%
    kable_styling(full_width = FALSE) %>%
    add_header_above(c(" " = 2, "Coefficients" = 3, "Model Statistics" = 1)) %>%
    footnote(
      general = "Standard errors in parentheses",
      symbol = c(
        "*** p < 0.01; ** p < 0.05; * p < 0.1",
        "Price and volatility effects show impact on consumption"
      )
    )
} else {
  # Simple print if kableExtra not available
  print(formatted_results)
}
