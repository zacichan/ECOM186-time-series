library(readxl)
library(dplyr)
library(tidyr)

price <- ("/Users/charlottekneebone/Development/ECOM186-time-series/data/price.xlsx")
price_data <- "2.1.2"

# Read the Excel sheet, skip the first 10 rows
price_data <- read_excel(price, sheet = price_data, skip = 10)

#Select cols
select_data <- price_data %>%
  select("Year","Real price indices: Gas [Note 3]","Real price indices: Electricity [Note 3]") %>%
  rename( "gas_price" = "Real price indices: Gas [Note 3]",
          "elec_price" = "Real price indices: Electricity [Note 3]") %>%
  drop_na()



