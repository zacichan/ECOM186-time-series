library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

install.packages("stringdist")
library(stringdist)

elec_data <- ("/Users/charlotte/ECOM186-time-series/data/elec.cons.xlsx")
sheets <- excel_sheets(elec_data)[-1]

#Mean Domestic Consumption

columns_keep <- c("Code", "Country or region","Local authority", "Mean consumption\r\n(kWh per meter):\r\nDomestic\r\n",
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
}) %>%
  
# Remove "Unallocated" and "All Local Authorities" rows
filter(!`Local authority` %in% c("Unallocated", "All local authorities", "All local authorities ")) %>%

#Recode LAs
mutate(`Local authority` = recode(`Local authority`,
                                  "Bournemouth" = "Bournemouth, Christchurch and Poole",
                                  "King's Lynn and West Norfolk" = "King's Lynn and West Norfolk"

))

# Combine local authority rows with Welsh name included 
data_combined <- data_combined %>%
  mutate(`Local authority` = str_extract(`Local authority`, "^[^/]+") %>% str_trim())

# Identify the most recent "Code" for each Local Authority
latest_codes <- data_combined %>%
  group_by(`Local authority`) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%  # In case of ties, keep the last row
  select(`Local authority`, Code) %>%
  distinct()

#Identify the most recent "Country or region" for each Local Authority
latest_region <- data_combined %>%
  group_by(`Local authority`) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  slice_tail(n = 2) %>%  # In case of ties, keep the last row
  select(`Local authority`, `Country or region`) %>%
  distinct()

# Merge the latest Code back into the main dataset
data_combined <- data_combined %>%
  select(-Code) %>%
  left_join(latest_codes, by = "Local authority")

# Merge the latest `Country or region` back into the main dataset
data_combined <- data_combined %>%
  select(-`Country or region`) %>%
  left_join(latest_region, by = "Local authority")

data_ts_d <- data_combined %>%
  pivot_wider(names_from = Year, values_from = "Mean Domestic Consumption")

# Identify Local Authorities with missing values in any year
#incomplete_local_authorities_ts <- data_ts_d %>%
#  filter(if_any(where(is.numeric), is.na)) %>%
#  select(`Local authority`)

# View the list
# print(incomplete_local_authorities_ts)

#Remove incomplete data rows 
data_ts_d <- na.omit(data_ts_d)

#Convert to long format 
data_ts_d <- data_ts_d %>%
  mutate(
    Year = paste(colnames(data_ts_d)[4:ncol(data_ts_d)], collapse = ","),  # Concatenate year names
    Value = apply(data_ts_d[, 4:ncol(data_ts_d)], 1, function(x) paste(x, collapse = ","))  # Concatenate values
  ) %>%
  select(`Local authority`, Code, `Country or region`, Year, Value)  # Keep required columns

write.csv(data_ts_d, "data_ts_d.csv", row.names = FALSE)

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
  
}) %>%
  
  # Remove "Unallocated" and "All Local Authorities" rows
  filter(!`Local authority` %in% c("Unallocated", "All local authorities", "All local authorities ")) %>%
  
  #Recode LAs
  mutate(`Local authority` = recode(`Local authority`,
                                    "Bournemouth" = "Bournemouth, Christchurch and Poole",
                                    "King's Lynn and West Norfolk" = "King's Lynn and West Norfolk"
                                    
  ))

# Combine local authority rows with Welsh name included 
data_combined <- data_combined %>%
  mutate(`Local authority` = str_extract(`Local authority`, "^[^/]+") %>% str_trim())

# Identify the most recent "Code" for each Local Authority
latest_codes <- data_combined %>%
  group_by(`Local authority`) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%  # In case of ties, keep the last row
  select(`Local authority`, Code) %>%
  distinct()

#Identify the most recent "Country or region" for each Local Authority
latest_region <- data_combined %>%
  group_by(`Local authority`) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  slice_tail(n = 2) %>%  # In case of ties, keep the last row
  select(`Local authority`, `Country or region`) %>%
  distinct()

# Merge the latest Code back into the main dataset
data_combined <- data_combined %>%
  select(-Code) %>%
  left_join(latest_codes, by = "Local authority")

# Merge the latest `Country or region` back into the main dataset
data_combined <- data_combined %>%
  select(-`Country or region`) %>%
  left_join(latest_region, by = "Local authority")

data_ts_nd <- data_combined %>%
  pivot_wider(names_from = Year, values_from = "Mean Non-Domestic Consumption")

# Identify Local Authorities with missing values in any year
#incomplete_local_authorities_ts <- data_ts_nd %>%
#  filter(if_any(where(is.numeric), is.na)) %>%
#  select(`Local authority`)

# View the list
#print(incomplete_local_authorities_ts)

#Remove incomplete data rows 
data_ts_nd <- na.omit(data_ts_nd)

#Convert to long format 
data_ts_nd <- data_ts_nd %>%
  mutate(
    Year = paste(colnames(data_ts_nd)[4:ncol(data_ts_nd)], collapse = ","),  # Concatenate year names
    Value = apply(data_ts_nd[, 4:ncol(data_ts_nd)], 1, function(x) paste(x, collapse = ","))  # Concatenate values
  ) %>%
  select(`Local authority`, Code, `Country or region`, Year, Value)  # Keep required columns

write.csv(data_ts_nd, "data_ts_nd.csv", row.names = FALSE)


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
}) %>%

# Remove "Unallocated" and "All Local Authorities" rows
filter(!`Local authority` %in% c("Unallocated", "All local authorities", "All local authorities ")) %>%
  
  #Recode LAs
  mutate(`Local authority` = recode(`Local authority`,
                                    "Bournemouth" = "Bournemouth, Christchurch and Poole",
                                    "King's Lynn and West Norfolk" = "King's Lynn and West Norfolk"
                                    
  ))

head(data_combined)
# Combine local authority rows with Welsh name included 
data_combined <- data_combined %>%
  mutate(`Local authority` = str_extract(`Local authority`, "^[^/]+") %>% str_trim())

# Identify the most recent "Code" for each Local Authority
latest_codes <- data_combined %>%
  group_by(`Local authority`) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%  # In case of ties, keep the last row
  select(`Local authority`, Code) %>%
  distinct()

#Identify the most recent "Country or region" for each Local Authority
latest_region <- data_combined %>%
  group_by(`Local authority`) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  slice_tail(n = 2) %>%  # In case of ties, keep the last row
  select(`Local authority`, `Country or region`) %>%
  distinct()

# Merge the latest Code back into the main dataset
data_combined <- data_combined %>%
  select(-Code) %>%
  left_join(latest_codes, by = "Local authority")

# Merge the latest `Country or region` back into the main dataset
data_combined <- data_combined %>%
  select(-`Country or region`) %>%
  left_join(latest_region, by = "Local authority")

data_ts_am <- data_combined %>%
  pivot_wider(names_from = Year, values_from = "Mean Consumption All Meters")

# Identify Local Authorities with missing values in any year
#incomplete_local_authorities_ts <- data_ts_d %>%
#  filter(if_any(where(is.numeric), is.na)) %>%
#  select(`Local authority`)

# View the list
# print(incomplete_local_authorities_ts)

#Remove incomplete data rows 
data_ts_am <- na.omit(data_ts_am)

#Convert to long format 
data_ts_am <- data_ts_am %>%
  mutate(
    Year = paste(colnames(data_ts_am)[4:ncol(data_ts_am)], collapse = ","),  # Concatenate year names
    Value = apply(data_ts_am[, 4:ncol(data_ts_am)], 1, function(x) paste(x, collapse = ","))  # Concatenate values
  ) %>%
  select(`Local authority`, Code, `Country or region`, Year, Value)  # Keep required columns

write.csv(data_ts_am, "data_ts_am.csv", row.names = FALSE)



