# Load required packages
library(tidyverse)
library(readxl)

# 1. Data Import ----
# Import consumption, price and volatility data
cons_price_vol <- read_csv("final_data/master/cons_price_vol.csv")

# Import Rural Urban Classification lookup table
rural_urban_class <- read_excel(
  "data/Rural_Urban_Classification_2011_lookup_tables_for_local_authority_areas.xlsx", 
  sheet = "LAD21",
  skip = 2
) %>%
  rename(
    Code = `Local Authority District Area 2021 Code`,
    Local.authority = `Local Authority District Area 2021 Name`
  )

# 2. Create Rural-Urban Classification Lookup for Scotland and Wales ----
rural_urban_lookup <- tibble(
  Code = c(
    # Scottish authorities
    "S12000005", "S12000006", "S12000008", "S12000010", "S12000011", 
    "S12000014", "S12000018", "S12000019", "S12000020", "S12000021",
    "S12000026", "S12000028", "S12000029", "S12000030", "S12000033",
    "S12000034", "S12000035", "S12000036", "S12000038", "S12000039",
    "S12000040", "S12000041", "S12000042", "S12000045", "S12000047",
    "S12000048", "S12000049", "S12000050",
    # Welsh authorities
    "W06000001", "W06000002", "W06000003", "W06000004", "W06000005",
    "W06000006", "W06000008", "W06000009", "W06000010", "W06000011",
    "W06000012", "W06000013", "W06000014", "W06000015", "W06000016",
    "W06000018", "W06000019", "W06000020", "W06000021", "W06000022",
    "W06000023", "W06000024"
  ),
  `Rural Urban Classification 2011 (6 fold)` = c(
    # Scottish authorities
    "Urban with Significant Rural",    # Clackmannanshire
    "Mainly Rural",                    # Dumfries and Galloway
    "Urban with Significant Rural",    # East Ayrshire
    "Urban with Significant Rural",    # East Lothian
    "Urban with Major Conurbation",    # East Renfrewshire
    "Urban with City and Town",        # Falkirk
    "Urban with Major Conurbation",    # Inverclyde
    "Urban with Significant Rural",    # Midlothian
    "Largely Rural",                   # Moray
    "Urban with Significant Rural",    # North Ayrshire
    "Mainly Rural",                    # Scottish Borders
    "Urban with Significant Rural",    # South Ayrshire
    "Urban with Major Conurbation",    # South Lanarkshire
    "Urban with Significant Rural",    # Stirling
    "Urban with City and Town",        # Aberdeen City
    "Largely Rural",                   # Aberdeenshire
    "Mainly Rural",                    # Argyll and Bute
    "Urban with City and Town",        # City of Edinburgh
    "Urban with Major Conurbation",    # Renfrewshire
    "Urban with Major Conurbation",    # West Dunbartonshire
    "Urban with Significant Rural",    # West Lothian
    "Largely Rural",                   # Angus
    "Urban with City and Town",        # Dundee City
    "Urban with Major Conurbation",    # East Dunbartonshire
    "Urban with Significant Rural",    # Fife
    "Largely Rural",                   # Perth and Kinross
    "Urban with City and Town",        # Glasgow City
    "Urban with Major Conurbation",    # North Lanarkshire
    # Welsh authorities
    "Largely Rural",                   # Isle of Anglesey
    "Mainly Rural",                    # Gwynedd
    "Urban with Significant Rural",    # Conwy
    "Urban with Significant Rural",    # Denbighshire
    "Urban with Significant Rural",    # Flintshire
    "Urban with City and Town",        # Wrexham
    "Mainly Rural",                    # Ceredigion
    "Largely Rural",                   # Pembrokeshire
    "Largely Rural",                   # Carmarthenshire
    "Urban with City and Town",        # Swansea
    "Urban with City and Town",        # Neath Port Talbot
    "Urban with Significant Rural",    # Bridgend
    "Urban with Significant Rural",    # Vale of Glamorgan
    "Urban with City and Town",        # Cardiff
    "Urban with Significant Rural",    # Rhondda Cynon Taf
    "Urban with Significant Rural",    # Caerphilly
    "Urban with City and Town",        # Blaenau Gwent
    "Urban with City and Town",        # Torfaen
    "Largely Rural",                   # Monmouthshire
    "Urban with City and Town",        # Newport
    "Mainly Rural",                    # Powys
    "Urban with City and Town"         # Merthyr Tydfil
  )
)

# 3. Merge and Update Classifications ----
# Initial merge with existing classifications
merged_df <- cons_price_vol %>%
  left_join(
    rural_urban_class %>% 
      select(Code, `Rural Urban Classification 2011 (6 fold)`, 
             `Rural Urban Classification 2011 (3 fold)`),
    by = "Code"
  )

# Add Scottish and Welsh classifications
merged_df_final <- merged_df %>%
  left_join(rural_urban_lookup, by = "Code") %>%
  mutate(`Rural Urban Classification 2011 (6 fold)` = 
           coalesce(`Rural Urban Classification 2011 (6 fold).x`, 
                    `Rural Urban Classification 2011 (6 fold).y`)) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # Add 3-fold classification
  mutate(`Rural Urban Classification 2011 (3 fold)` = case_when(
    `Rural Urban Classification 2011 (6 fold)` %in% 
      c("Urban with Major Conurbation", "Urban with Minor Conurbation", 
        "Urban with City and Town") ~ "Predominantly Urban",
    `Rural Urban Classification 2011 (6 fold)` == "Urban with Significant Rural" ~ 
      "Urban with Significant Rural",
    `Rural Urban Classification 2011 (6 fold)` %in% 
      c("Largely Rural", "Mainly Rural") ~ "Predominantly Rural",
    TRUE ~ NA_character_
  ))

# 4. Quality Checks ----
# Check for any remaining missing classifications
missing_classifications <- merged_df_final %>%
  filter(is.na(`Rural Urban Classification 2011 (6 fold)`)) %>%
  distinct(Code, Local.authority)

# Print summary statistics
cat("Data Quality Summary:\n")
cat("Total number of authorities:", n_distinct(merged_df_final$Code), "\n")
cat("Number of time periods:", n_distinct(merged_df_final$Year), "\n")
cat("Missing classifications:", nrow(missing_classifications), "\n")

# Save final dataset if needed
write_csv(merged_df_final, "final_data/master/cons_price_vol_with_rural_urban.csv")
