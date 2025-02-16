# Add this after your existing code:

# ==== IRF ANALYSIS AND VISUALIZATION ====

library(tidyverse)
library(gridExtra)

# Function to calculate IRFs and format for ggplot
calculate_irfs <- function(model, n_ahead = 24, conf_level = 0.95) {
  # Calculate IRFs with bootstrap confidence intervals
  irf_results <- bootstrap_irf(
    model = model,
    typeof_irf = "OIRF",  # Orthogonalized IRF
    n.ahead = n_ahead,
    nof_Nstar_draws = 500,  # Number of bootstrap replications
    confidence.band = conf_level
  )
  
  # Extract point estimates and confidence bands
  irf_data <- data.frame(
    horizon = 0:n_ahead,
    cons_to_price = irf_results$irf_s_1[, 1],  # Consumption response to price shock
    cons_to_vol = irf_results$irf_s_1[, 2],    # Consumption response to volatility shock
    lower_price = irf_results$lower_s_1[, 1],
    upper_price = irf_results$upper_s_1[, 1],
    lower_vol = irf_results$lower_s_1[, 2],
    upper_vol = irf_results$upper_s_1[, 2]
  )
  
  return(irf_data)
}

# Calculate IRFs for each model
urban_irfs <- calculate_irfs(urban_model)
rural_irfs <- calculate_irfs(rural_model)
mixed_irfs <- calculate_irfs(mixed_model)

# Function to create IRF plots using ggplot2
plot_irfs <- function(irf_data, title) {
  # Price shock plot
  p1 <- ggplot(irf_data, aes(x = horizon)) +
    geom_ribbon(aes(ymin = lower_price, ymax = upper_price), 
                fill = "gray80", alpha = 0.5) +
    geom_line(aes(y = cons_to_price), color = "blue", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste(title, "- Response to Price Shock"),
         x = "Months after shock",
         y = "% deviation from baseline") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  # Volatility shock plot
  p2 <- ggplot(irf_data, aes(x = horizon)) +
    geom_ribbon(aes(ymin = lower_vol, ymax = upper_vol), 
                fill = "gray80", alpha = 0.5) +
    geom_line(aes(y = cons_to_vol), color = "red", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste(title, "- Response to Volatility Shock"),
         x = "Months after shock",
         y = "% deviation from baseline") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  return(list(price = p1, volatility = p2))
}

# Create plots for each group
urban_plots <- plot_irfs(urban_irfs, "Urban Areas")
rural_plots <- plot_irfs(rural_irfs, "Rural Areas")
mixed_plots <- plot_irfs(mixed_irfs, "Mixed Areas")

# Arrange plots in a grid
price_response_grid <- grid.arrange(
  urban_plots$price, rural_plots$price, mixed_plots$price,
  ncol = 1,
  top = "Consumption Response to Price Shocks Across Area Types"
)

volatility_response_grid <- grid.arrange(
  urban_plots$volatility, rural_plots$volatility, mixed_plots$volatility,
  ncol = 1,
  top = "Consumption Response to Volatility Shocks Across Area Types"
)

# Save plots
ggsave("price_responses.png", price_response_grid, height = 12, width = 8)
ggsave("volatility_responses.png", volatility_response_grid, height = 12, width = 8)

# Add summary analysis of IRF results
analyze_irfs <- function(irf_data) {
  list(
    max_price_impact = max(abs(irf_data$cons_to_price)),
    max_vol_impact = max(abs(irf_data$cons_to_vol)),
    persistence_price = sum(abs(irf_data$cons_to_price) > 0.01),
    persistence_vol = sum(abs(irf_data$cons_to_vol) > 0.01)
  )
}

urban_analysis <- analyze_irfs(urban_irfs)
rural_analysis <- analyze_irfs(rural_irfs)
mixed_analysis <- analyze_irfs(mixed_irfs)

# Print analysis results
cat("\nIRF Analysis Results:\n")
cat("\nUrban Areas:")
cat("\nMaximum price impact:", round(urban_analysis$max_price_impact, 3))
cat("\nMaximum volatility impact:", round(urban_analysis$max_vol_impact, 3))
cat("\nPersistence of price shock (months):", urban_analysis$persistence_price)
cat("\nPersistence of volatility shock (months):", urban_analysis$persistence_vol)

cat("\n\nRural Areas:")
cat("\nMaximum price impact:", round(rural_analysis$max_price_impact, 3))
cat("\nMaximum volatility impact:", round(rural_analysis$max_vol_impact, 3))
cat("\nPersistence of price shock (months):", rural_analysis$persistence_price)
cat("\nPersistence of volatility shock (months):", rural_analysis$persistence_vol)