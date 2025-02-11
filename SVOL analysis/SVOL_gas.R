library(readr)
library(tidyverse)
library(stochvol)

#ALL METERS CONSUMPTION
data_ts_amg <- ("/Users/charlotte/ECOM186-time-series/final_data/data_ts_amg")
data_ts_amg <- read_csv("final_data/data_ts_amg.csv")

#Sort data by time within each local authority
data_ts_amg <- data_ts_amg %>%
  arrange(`Local authority`,Year)

#Define function to apply SVOL model per local authority 
fit_svol <- function(data_ts_amg){
  Y<-data_ts_amg$Value
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h<-ts(exp(h), start=min(data_ts_amg$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 

svol_results_g <- data_ts_amg %>%
  group_by(`Local authority`) %>%
  group_split() %>%
  setNames(unique(data_ts_amg$`Local authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_g[["Westminster"]]$volatility, main = "Stochastic Volatility Gas (Westminster)", ylab = "Volatility")

# Create a data frame to store the results
volatility_amg <- data.frame(
  LAD = rep(names(svol_results_g), each = length(svol_results_g[[1]]$volatility)), 
  Year = unlist(lapply(svol_results_g, function(x) seq(from = min(data_ts_amg$Year), length.out = length(x$volatility)))),  
  volatility = unlist(lapply(svol_results_g, function(x) x$volatility)),
  stringsAsFactors = FALSE,
  row.names = NULL  # Ensures no row names are set
)

# Write to CSV
write.csv(volatility_amg, "volatility_amg.csv", row.names = FALSE)


#DOMESTIC CONSUMPTION 
data_ts_dg <- ("/Users/charlotte/ECOM186-time-series/final_data/data_ts_dg")
data_ts_dg <- read_csv("final_data/data_ts_dg.csv")

#Sort data by time within each local authority
data_ts_dg <- data_ts_dg %>%
  arrange(`Local authority`,Year)

#Define function to apply SVOL model per local authority 
fit_svol <- function(data_ts_dg){
  Y<-data_ts_dg$Value
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h<-ts(exp(h), start=min(data_ts_dg$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 

svol_results_dg <- data_ts_dg %>%
  group_by(`Local authority`) %>%
  group_split() %>%
  setNames(unique(data_ts_dg$`Local authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_dg[["Westminster"]]$volatility, main = "Stochastic Volatility Domestic Gas (Westminster)", ylab = "Volatility")

# Create a data frame to store the results
volatility_dg <- data.frame(
  LAD = rep(names(svol_results_dg), each = length(svol_results_dg[[1]]$volatility)), 
  Year = unlist(lapply(svol_results_dg, function(x) seq(from = min(data_ts_dg$Year), length.out = length(x$volatility)))),  
  volatility = unlist(lapply(svol_results_dg, function(x) x$volatility)),
  stringsAsFactors = FALSE,
  row.names = NULL  # Ensures no row names are set
)

# Write to CSV
write.csv(volatility_dg, "volatility_dg.csv", row.names = FALSE)

#NON-DOMESTIC CONSUMPTION
data_ts_ndg <- ("/Users/charlotte/ECOM186-time-series/final_data/data_ts_ndg")
data_ts_ndg <- read_csv("final_data/data_ts_ndg.csv")

#Sort data by time within each local authority
data_ts_ndg <- data_ts_ndg %>%
  arrange(`Local authority`,Year)

#Define function to apply SVOL model per local authority 
fit_svol <- function(data_ts_ndg){
  Y<-data_ts_ndg$Value
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h<-ts(exp(h), start=min(data_ts_ndg$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 

svol_results_ndg <- data_ts_ndg %>%
  group_by(`Local authority`) %>%
  group_split() %>%
  setNames(unique(data_ts_ndg$`Local authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_ndg[["Westminster"]]$volatility, main = "Stochastic Volatility Industry Gas (Westminster)", ylab = "Volatility")

# Create a data frame to store the results
volatility_ndg <- data.frame(
  LAD = rep(names(svol_results_ndg), each = length(svol_results_ndg[[1]]$volatility)), 
  Year = unlist(lapply(svol_results_ndg, function(x) seq(from = min(data_ts_ndg$Year), length.out = length(x$volatility)))),  
  volatility = unlist(lapply(svol_results_ndg, function(x) x$volatility)),
  stringsAsFactors = FALSE,
  row.names = NULL  # Ensures no row names are set
)

# Write to CSV
write.csv(volatility_ndg, "volatility_ndg.csv", row.names = FALSE)

