library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

install.packages("stringdist")
library(stringdist)

elec_data <- ("/Users/charlottekneebone/Development/ECOM186-time-series/data/elec.cons.xlsx")
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

#################### Identify Local Authorities with missing values in any year#####################
incomplete_local_authorities_ts <- data_ts_d %>%
  filter(if_any(where(is.numeric), is.na)) %>%
  select(`Local authority`)

incomplete_data_ts <- data_ts_d %>%
  semi_join(incomplete_local_authorities_ts, by = "Local authority")

# Pivot data to long format
incomplete_data_long <- incomplete_data_ts %>%
  pivot_longer(cols = where(is.numeric), names_to = "Year", values_to = "Value")

# Calculate overall summary statistics
overall_summary_stats_d <- incomplete_data_long %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE)
  )
####################################################################################################

# View the list
print(incomplete_local_authorities_ts)

#Remove incomplete data rows 
data_ts_d <- na.omit(data_ts_d)

#Convert to long format 
data_ts_d <- data_ts_d %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value") %>%
  select(`Local authority`, Code, `Country or region`, Year, Value)

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

#################### Identify Local Authorities with missing values in any year#####################
incomplete_local_authorities_ts <- data_ts_nd %>%
  filter(if_any(where(is.numeric), is.na)) %>%
  select(`Local authority`)

incomplete_data_ts <- data_ts_nd %>%
  semi_join(incomplete_local_authorities_ts, by = "Local authority")

# Pivot data to long format
incomplete_data_long <- incomplete_data_ts %>%
  pivot_longer(cols = where(is.numeric), names_to = "Year", values_to = "Value")

# Calculate overall summary statistics
overall_summary_stats_nd <- incomplete_data_long %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE)
  )
####################################################################################################

#Remove incomplete data rows 
data_ts_nd <- na.omit(data_ts_nd)

#Convert to long format 
data_ts_nd <- data_ts_nd %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value") %>%
  select(`Local authority`, Code, `Country or region`, Year, Value)

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

####################Identify Local Authorities with missing values in any year#####################
incomplete_local_authorities_ts <- data_ts_am %>%
  filter(if_any(where(is.numeric), is.na)) %>%
  select(`Local authority`)

incomplete_data_ts <- data_ts_am %>%
  semi_join(incomplete_local_authorities_ts, by = "Local authority")

# Pivot data to long format
incomplete_data_long <- incomplete_data_ts %>%
  pivot_longer(cols = where(is.numeric), names_to = "Year", values_to = "Value")

# Calculate overall summary statistics
overall_summary_stats_am <- incomplete_data_long %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE)
  )
####################################################################################################

#Remove incomplete data rows 
data_ts_am <- na.omit(data_ts_am)

#Convert to long format 
data_ts_am <- data_ts_am %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value") %>%
  select(`Local authority`, Code, `Country or region`, Year, Value)

write.csv(data_ts_am, "data_ts_am.csv", row.names = FALSE)



