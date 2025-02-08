library(readr)
library(tidyverse)
library(stochvol)

setwd("/Users/charlotte/ECOM186-time-series")

#ALL METERS CONSUMPTION
data_ts_am <- ("/Users/charlotte/ECOM186-time-series/final_data/data_ts_am")
data_ts_am <- read_csv("final_data/data_ts_am.csv")

#Sort data by time within each local authority
data_ts_am <- data_ts_am %>%
  arrange(`Local authority`,Year)

#Define function to apply SVOL model per local authority 
fit_svol <- function(data_ts_am){
  Y<-data_ts_am$Value
  DY<-diff(log(Y))*100 #compute log-differences

REPS<- 25000
BURN <- 20000
draws <- svsample(DY, REPS,BURN,"ar1")

pred <- predict(draws,1)
pp <- pred$y[[1]]

data_latent <- draws$latent
h <- apply(data_latent[[1]],2,mean)
h<-ts(exp(h), start=min(data_ts_am$Year),frequency = 1)

return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 

svol_results <- data_ts_am %>%
  group_by(`Local authority`) %>%
  group_split() %>%
  setNames(unique(data_ts_am$`Local authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results[["Westminster"]]$volatility, main = "Stochastic Volatility Electricity (Westminster)", ylab = "Volatility")

#DOMESTIC CONSUMPTION 
data_ts_d <- ("/Users/charlotte/ECOM186-time-series/final_data/data_ts_d")
data_ts_d <- read_csv("final_data/data_ts_d.csv")

#Sort data by time within each local authority
data_ts_d <- data_ts_d %>%
  arrange(`Local authority`,Year)

#Define function to apply SVOL model per local authority 
fit_svol <- function(data_ts_d){
  Y<-data_ts_d$Value
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h<-ts(exp(h), start=min(data_ts_d$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 

svol_results_d <- data_ts_d %>%
  group_by(`Local authority`) %>%
  group_split() %>%
  setNames(unique(data_ts_d$`Local authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_d[["Westminster"]]$volatility, main = "Stochastic Volatility Domestic Electricity (Westminster)", ylab = "Volatility")

#NON-DOMESTIC CONSUMPTION
data_ts_nd <- ("/Users/charlotte/ECOM186-time-series/final_data/data_ts_nd")
data_ts_nd <- read_csv("final_data/data_ts_nd.csv")

#Sort data by time within each local authority
data_ts_nd <- data_ts_nd %>%
  arrange(`Local authority`,Year)

#Define function to apply SVOL model per local authority 
fit_svol <- function(data_ts_nd){
  Y<-data_ts_nd$Value
  DY<-diff(log(Y))*100 #compute log-differences
  
  REPS<- 25000
  BURN <- 20000
  draws <- svsample(DY, REPS,BURN,"ar1")
  
  pred <- predict(draws,1)
  pp <- pred$y[[1]]
  
  data_latent <- draws$latent
  h <- apply(data_latent[[1]],2,mean)
  h<-ts(exp(h), start=min(data_ts_nd$Year),frequency = 1)
  
  return(list(model=draws, volatility=h, prediction=pp))
}

#Apply function to each local authority 

svol_results_nd <- data_ts_nd %>%
  group_by(`Local authority`) %>%
  group_split() %>%
  setNames(unique(data_ts_nd$`Local authority`)) %>%
  lapply(fit_svol)

# Example: Plot volatility for one local authority
plot.ts(svol_results_nd[["Westminster"]]$volatility, main = "Stochastic Volatility Industry Electricity (Westminster)", ylab = "Volatility")




