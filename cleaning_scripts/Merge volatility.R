library(readxl)
library(dplyr)
library(tidyr)

cons_price <- read.csv("final_data/merged/cons_price.csv")
volatility_am <- read.csv("final_data/SVOL_results/volatility_am.csv")
volatility_amg <- read.csv("final_data/SVOL_results/volatility_amg.csv")
volatility_d <- read.csv("final_data/SVOL_results/volatility_d.csv")
volatility_dg <- read.csv("final_data/SVOL_results/volatility_dg.csv")
volatility_nd <- read.csv("final_data/SVOL_results/volatility_nd.csv")
volatility_ndg <- read.csv("final_data/SVOL_results/volatility_ndg.csv")
volatility_gpd <- read.csv("final_data/SVOL_results/volatility_gpd.csv")
volatility_gpi <- read.csv("final_data/SVOL_results/volatility_gpi.csv")
volatility_epd <- read.csv("final_data/SVOL_results/volatility_epd.csv")
volatility_epi <- read.csv("final_data/SVOL_results/volatility_epi.csv")

names(volatility_am)[names(volatility_am) == "volatility"] <- "v_am_e"
names(volatility_amg)[names(volatility_amg) == "volatility"] <- "v_am_g"
names(volatility_d)[names(volatility_d) == "volatility"] <- "v_d_e"
names(volatility_dg)[names(volatility_dg) == "volatility"] <- "v_d_g"
names(volatility_nd)[names(volatility_nd) == "volatility"] <- "v_nd_e"
names(volatility_ndg)[names(volatility_ndg) == "volatility"] <- "v_nd_g"
names(volatility_gpd)[names(volatility_gpd) == "volatility"] <- "v_p_d_g"
names(volatility_gpi)[names(volatility_gpi) == "volatility"] <- "v_p_i_g"
names(volatility_epd)[names(volatility_epd) == "volatility"] <- "v_p_d_e"
names(volatility_epi)[names(volatility_epi) == "volatility"] <- "v_p_i_e"

names(volatility_am)[names(volatility_am) == "LAD"] <- "Local.authority"
names(volatility_amg)[names(volatility_amg) == "LAD"] <- "Local.authority"
names(volatility_d)[names(volatility_d) == "LAD"] <- "Local.authority"
names(volatility_dg)[names(volatility_dg) == "LAD"] <- "Local.authority"
names(volatility_nd)[names(volatility_nd) == "LAD"] <- "Local.authority"
names(volatility_ndg)[names(volatility_ndg) == "LAD"] <- "Local.authority"
names(volatility_gpd)[names(volatility_gpd) == "LAD"] <- "Local.authority"
names(volatility_gpi)[names(volatility_gpi) == "LAD"] <- "Local.authority"
names(volatility_epd)[names(volatility_epd) == "LAD"] <- "Local.authority"
names(volatility_epi)[names(volatility_epi) == "LAD"] <- "Local.authority"

#Merge datasets
cons_price_vol <- cons_price %>%
  inner_join(volatility_am, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_amg, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_d, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_dg, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_nd, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_ndg, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_gpd, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_gpi, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_epd, by = c("Year", "Local.authority")) %>%
  inner_join(volatility_epi, by = c("Year", "Local.authority")) 

#write csv
write.csv(cons_price_vol, "cons_price_vol.csv",row.names = FALSE)
