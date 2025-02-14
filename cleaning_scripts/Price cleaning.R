library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)  

#Domestic prices
price_d <- ("/Users/charlottekneebone/Development/ECOM186-time-series/data/price_d.xlsx")
sheet_name <- "2.1.2"

# Read the Excel sheet, skip the first 10 rows
price_d <- read_excel(price_d, sheet = sheet_name, skip = 10)

#Select cols
price_d <- price_d %>%
  select("Year","Real price indices: Gas [Note 3]","Real price indices: Electricity [Note 3]") %>%
  rename( "gas_price_domestic" = "Real price indices: Gas [Note 3]",
          "elec_price_domestic" = "Real price indices: Electricity [Note 3]") %>%
  drop_na()

write.csv(price_d,"price_d.csv",row.names=FALSE)

#Industrial prices
price_i <- ("/Users/charlottekneebone/Development/ECOM186-time-series/data/price_i.xlsx")
sheet_name <- "3.3.2 (Annual)"

# Read the Excel sheet, skip the first 10 rows
price_i <- read_excel(price_i, sheet = sheet_name, skip = 9)

head(price_i)
#Select cols
price_i <- price_i %>%
  select("Year","Gas (Fuel price index numbers relative to the GDP deflator)\r\n[Note 3, 5]","Electricity (Fuel price index numbers relative to the GDP deflator)\r\n[Note 3, 5]") %>%
  rename( "gas_price_industrial" = "Gas (Fuel price index numbers relative to the GDP deflator)\r\n[Note 3, 5]",
          "elec_price_industrial" = "Electricity (Fuel price index numbers relative to the GDP deflator)\r\n[Note 3, 5]") %>%
  drop_na()

price_i$Year <- year(ymd(price_i$Year))

write.csv(price_i,"price_i.csv",row.names=FALSE)
