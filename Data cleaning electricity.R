library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

elec_data <- ("/Users/charlotte/ECOM186-time-series/elec.cons.xlsx")
sheets <- excel_sheets(elec_data)[-1]

#Mean Domestic Consumption

columns_keep <- c("Code", "Country or region", "Local authority", "Mean consumption\r\n(kWh per meter):\r\nDomestic\r\n",
                 "Mean consumption\r\n(kWh per meter):\r\nAll Domestic\r\n", "Mean consumption\r\n(kWh per meter):\r\nAll Domestic")

data_combined <- map_dfr(sheets, function(sheet) {
  df <- read_excel(elec_data, sheet = sheet,skip=4) %>%
                 select(any_of(columns_keep)) %>%
                   rename("Mean Domestic Consumption" = any_of(c(
                     "Mean consumption\r\n(kWh per meter):\r\nDomestic\r\n",
                     "Mean consumption\r\n(kWh per meter):\r\nAll Domestic\r\n",
                     "Mean consumption\r\n(kWh per meter):\r\nAll Domestic"
                   )) 
                   ) %>%
                   
                   mutate(Year = as.numeric(sheet))
                 
                 return(df)
})

data_ts_d <- data_combined %>%
  pivot_wider(names_from = Year, values_from = "Mean Domestic Consumption")

#Mean non-domestic consumption 

columns_keep <- c("Code", "Country or region", "Local authority", "Mean consumption\r\n(kWh per meter):\r\nNon-Domestic",
                  "Mean consumption\r\n(kWh per meter):\r\nAll Non-Domestic\r\n", "Mean consumption\r\n(kWh per meter):\r\nAll Non-Domestic")

data_combined <- map_dfr(sheets, function(sheet) {
  df <- read_excel(elec_data, sheet = sheet,skip=4) %>%
    select(any_of(columns_keep)) %>%
    rename("Mean Non-Domestic Consumption" = any_of(c(
      "Mean consumption\r\n(kWh per meter):\r\nNon-Domestic",
      "Mean consumption\r\n(kWh per meter):\r\nAll Non-Domestic\r\n",
      "Mean consumption\r\n(kWh per meter):\r\nAll Non-Domestic"
    )) 
    ) %>%
    
    mutate(Year = as.numeric(sheet))
  
  return(df)
})

data_ts_nd <- data_combined %>%
  pivot_wider(names_from = Year, values_from = "Mean Non-Domestic Consumption")

#Mean consumption all meters 

columns_keep <- c("Code", "Country or region", "Local authority", "Mean consumption\r\n(kWh per meter):\r\nAll meters",
                  "Mean consumption\r\n(kWh per meter):\r\nAll meters\r\n", "Mean consumption\r\n(kWh per meter):\r\nAll meters")

data_combined <- map_dfr(sheets, function(sheet) {
  df <- read_excel(elec_data, sheet = sheet,skip=4) %>%
    select(any_of(columns_keep)) %>%
    rename("Mean Consumption All Meters" = any_of(c(
      "Mean consumption\r\n(kWh per meter):\r\nAll meters",
      "Mean consumption\r\n(kWh per meter):\r\nAll meters\r\n",
      "Mean consumption\r\n(kWh per meter):\r\nAll meters"
    )) 
    ) %>%
    
    mutate(Year = as.numeric(sheet))
  
  return(df)
})

data_ts_am <- data_combined %>%
  pivot_wider(names_from = Year, values_from = "Mean Consumption All Meters")



