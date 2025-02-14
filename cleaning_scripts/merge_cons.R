library(readxl)
library(dplyr)
library(tidyr)

####ELECTRICITY DATA

data_ts_am <- read.csv("final_data/clean_data/data_ts_am.csv")
data_ts_d <- read.csv("final_data/clean_data/data_ts_d.csv")
data_ts_nd <- read.csv("final_data/clean_data/data_ts_nd.csv")

names(data_ts_am)[names(data_ts_am) == "Value"] <- "total_cons_elec"
names(data_ts_d)[names(data_ts_d) == "Value"] <- "domestic_cons_elec"
names(data_ts_nd)[names(data_ts_nd) == "Value"] <- "industrial_cons_elec"

# Convert to character (or numeric if appropriate)
data_ts_am$Code <- as.character(data_ts_am$Code)
data_ts_d$Code <- as.character(data_ts_d$Code)
data_ts_nd$Code <- as.character(data_ts_nd$Code)

data_ts_am$Year <- as.numeric(data_ts_am$Year)
data_ts_d$Year <- as.numeric(data_ts_d$Year)
data_ts_nd$Year <- as.numeric(data_ts_nd$Year)

data_ts_am$Local.authority <- as.character(data_ts_am$Local.authority)
data_ts_d$Local.authority <- as.character(data_ts_d$Local.authority)
data_ts_nd$Local.authority <- as.character(data_ts_nd$Local.authority)

data_ts_am$Country.or.region <- as.character(data_ts_am$Country.or.region )
data_ts_d$Country.or.region  <- as.character(data_ts_d$Country.or.region )
data_ts_nd$Country.or.region  <- as.character(data_ts_nd$Country.or.region )

#Merge data 
all_elec_cons <- data_ts_am %>%
  inner_join(data_ts_d, by = c("Code", "Year", "Local.authority", "Country.or.region")) %>%
  inner_join(data_ts_nd, by = c("Code", "Year", "Local.authority", "Country.or.region"))

write.csv(all_elec_cons, "all_elec_cons.csv",row.names = FALSE)

###GAS DATA 
data_ts_amg <- read.csv("final_data/clean_data/data_ts_amg.csv")
data_ts_dg <- read.csv("final_data/clean_data/data_ts_dg.csv")
data_ts_ndg <- read.csv("final_data/clean_data/data_ts_ndg.csv")

names(data_ts_amg)[names(data_ts_amg) == "Value"] <- "total_cons_gas"
names(data_ts_dg)[names(data_ts_dg) == "Value"] <- "domestic_cons_gas"
names(data_ts_ndg)[names(data_ts_ndg) == "Value"] <- "industrial_cons_gas"

# Convert to character (or numeric if appropriate)
data_ts_amg$Code <- as.character(data_ts_amg$Code)
data_ts_dg$Code <- as.character(data_ts_dg$Code)
data_ts_ndg$Code <- as.character(data_ts_ndg$Code)

data_ts_amg$Year <- as.numeric(data_ts_amg$Year)
data_ts_dg$Year <- as.numeric(data_ts_dg$Year)
data_ts_ndg$Year <- as.numeric(data_ts_ndg$Year)

data_ts_amg$Local.authority <- as.character(data_ts_amg$Local.authority)
data_ts_dg$Local.authority <- as.character(data_ts_dg$Local.authority)
data_ts_ndg$Local.authority <- as.character(data_ts_ndg$Local.authority)

data_ts_amg$Country.or.region <- as.character(data_ts_amg$Country.or.region )
data_ts_dg$Country.or.region  <- as.character(data_ts_dg$Country.or.region )
data_ts_ndg$Country.or.region  <- as.character(data_ts_ndg$Country.or.region )

#Merge data 

all_gas_cons <- data_ts_amg %>%
  inner_join(data_ts_dg, by = c("Code", "Year", "Local.authority", "Country.or.region")) %>%
  inner_join(data_ts_ndg, by = c("Code", "Year", "Local.authority", "Country.or.region"))

write.csv(all_gas_cons, "all_gas_cons.csv",row.names = FALSE)

###MERGE ELEC AND GAS 

all_cons <- all_elec_cons %>%
  inner_join(all_gas_cons, by = c("Code", "Year", "Local.authority", "Country.or.region")) 

write.csv(all_cons, "all_cons.csv",row.names = FALSE)
