library(readr)
library(tidyverse)
library(stochvol)

#LOAD DATA
cons_price <- read_csv("final_data/merged/cons_price.csv")

#Sort data by time within each local authority
cons_price <- cons_price %>%
  arrange(`Local.authority`,Year)

##GAS PRICE DOMESTIC
#Define function to apply SVOL model per local authority 
fit_svol <- function(cons_price){
  Y<-cons_price$gas_price_domestic
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h <- ts(exp(h), start=min(cons_price$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 
svol_results_gpd <- cons_price %>%
  group_by(`Local.authority`) %>%
  group_split() %>%
  setNames(unique(cons_price$`Local.authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_gpd[["Westminster"]]$volatility, main = "Stochastic Volatility Electricity (Westminster)", ylab = "Volatility")

# Create a data frame to store the results
volatility_gpd <- data.frame(
  LAD = rep(names(svol_results_gpd), each = length(svol_results_gpd[[1]]$volatility)), 
  Year = unlist(lapply(svol_results_gpd, function(x) seq(from = min(cons_price$Year), length.out = length(x$volatility)))),  
  volatility = unlist(lapply(svol_results_gpd, function(x) x$volatility)),
  stringsAsFactors = FALSE,
  row.names = NULL  # Ensures no row names are set
)

# Write to CSV
write.csv(volatility_gpd, "volatility_gpd.csv", row.names = FALSE)

###GAS PRICE INDUSTRIAL
#Define function to apply SVOL model per local authority 
fit_svol <- function(cons_price){
  Y<-cons_price$gas_price_industrial
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h <- ts(exp(h), start=min(cons_price$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 
svol_results_gpi <- cons_price %>%
  group_by(`Local.authority`) %>%
  group_split() %>%
  setNames(unique(cons_price$`Local.authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_gpd[["Westminster"]]$volatility, main = "Stochastic Volatility Electricity (Westminster)", ylab = "Volatility")

# Create a data frame to store the results
volatility_gpi <- data.frame(
  LAD = rep(names(svol_results_gpi), each = length(svol_results_gpi[[1]]$volatility)), 
  Year = unlist(lapply(svol_results_gpi, function(x) seq(from = min(cons_price$Year), length.out = length(x$volatility)))),  
  volatility = unlist(lapply(svol_results_gpi, function(x) x$volatility)),
  stringsAsFactors = FALSE,
  row.names = NULL  # Ensures no row names are set
)

# Write to CSV
write.csv(volatility_gpi, "volatility_gpi.csv", row.names = FALSE)

###ELECTRICITY PRICE DOMESTIC
#Define function to apply SVOL model per local authority 
fit_svol <- function(cons_price){
  Y<-cons_price$elec_price_domestic
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h <- ts(exp(h), start=min(cons_price$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 
svol_results_epd <- cons_price %>%
  group_by(`Local.authority`) %>%
  group_split() %>%
  setNames(unique(cons_price$`Local.authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_epd[["Westminster"]]$volatility, main = "Stochastic Volatility Electricity (Westminster)", ylab = "Volatility")

# Create a data frame to store the results
volatility_epd <- data.frame(
  LAD = rep(names(svol_results_epd), each = length(svol_results_epd[[1]]$volatility)), 
  Year = unlist(lapply(svol_results_epd, function(x) seq(from = min(cons_price$Year), length.out = length(x$volatility)))),  
  volatility = unlist(lapply(svol_results_epd, function(x) x$volatility)),
  stringsAsFactors = FALSE,
  row.names = NULL  # Ensures no row names are set
)

# Write to CSV
write.csv(volatility_epd, "volatility_epd.csv", row.names = FALSE)

###ELECTRICITY PRICE INDUSTRIAL 
#Define function to apply SVOL model per local authority 
fit_svol <- function(cons_price){
  Y<-cons_price$elec_price_industrial
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h <- ts(exp(h), start=min(cons_price$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 
svol_results_epi <- cons_price %>%
  group_by(`Local.authority`) %>%
  group_split() %>%
  setNames(unique(cons_price$`Local.authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_epi[["Westminster"]]$volatility, main = "Stochastic Volatility Electricity (Westminster)", ylab = "Volatility")

# Create a data frame to store the results
volatility_epi <- data.frame(
  LAD = rep(names(svol_results_epi), each = length(svol_results_epi[[1]]$volatility)), 
  Year = unlist(lapply(svol_results_epi, function(x) seq(from = min(cons_price$Year), length.out = length(x$volatility)))),  
  volatility = unlist(lapply(svol_results_epi, function(x) x$volatility)),
  stringsAsFactors = FALSE,
  row.names = NULL  # Ensures no row names are set
)

# Write to CSV
write.csv(volatility_epi, "volatility_epi.csv", row.names = FALSE)
