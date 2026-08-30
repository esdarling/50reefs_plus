#summary of eez by 50 reef plus 
#oct 2025 prioritizations 

library(mermaidr)
library(tidyverse)
library(janitor)
library(here)
library(readxl)

#Need - counts of EEZs and jurisdictions for May 2026 revision
file_path <- here("50 reefs plus/Copy of Zawada et al. Supplementary Tables.xlsx")
data <- read_excel(file_path, sheet="Table S6") %>% 
  clean_names() %>% 
  rename(jurisdictions = 2)

data
names(data)

#50 Reefs+ total (new + Beyer)
data %>% 
  filter(x50reefs_plus_km2 > 0) %>% 
  summarise(n = n_distinct(eez_country)) #71

data %>% 
  filter(x50reefs_plus_km2 > 0) %>% 
  summarise(n = n_distinct(jurisdictions)) #100

#Comparison to Beyer
data %>% 
  filter(beyer_only_km2 > 0) %>% 
  summarise(n = n_distinct(eez_country)) #41

data %>% 
  filter(beyer_only_km2 > 0) %>% 
  summarise(n = n_distinct(jurisdictions)) #46

100-46

#SSP5-8.5 Table S7 - counts of EEZs and jurisdictions for May 2026 revision
file_path <- here("50 reefs plus/Copy of Zawada et al. Supplementary Tables.xlsx")
data <- read_excel(file_path, sheet="Table S7") %>% 
  clean_names() %>% 
  rename(jurisdictions = 2)

data
names(data)

#50 Reefs+ total (new + Beyer)
data %>% 
  filter(x50reefs_plus_km2 > 0) %>% 
  summarise(n = n_distinct(eez_country)) #75

data %>% 
  filter(x50reefs_plus_km2 > 0) %>% 
  summarise(n = n_distinct(jurisdictions)) #103

#Comparison to Beyer
data %>% 
  filter(beyer_only_km2 > 0) %>% 
  summarise(n = n_distinct(eez_country)) #41

data %>% 
  filter(beyer_only_km2 > 0) %>% 
  summarise(n = n_distinct(jurisdictions)) #46

100-46

# 
# file_path <- here("Copy of  
# 
# # Read the first two rows as data
# headers <- read_excel(file_path, col_names = FALSE, n_max = 2)
# 
# # Combine them into one header vector
# combined_names <- paste(headers[1, ], headers[2, ]) |>
#   gsub("NA", "", x = _) |>
#   trimws()
# 
# # Now read the rest of the data, skipping the first two rows
# data <- read_excel(file_path, skip = 2, col_names = combined_names) %>% 
#   clean_names()
# 
# names(data)
# 
# #how many countries? 84
# data %>% 
#   select(eez_country) %>% 
#   distinct()
# 
# #140 territories
# data %>% 
#   select(eez_territory) %>% 
#   distinct()
# 
# #make summary by country to double check 
# names(data)
# data
# 
# #country and territory summary
# data_summary <- data %>%
#   group_by(eez_country) %>%
#   summarise(
#     n_territories = n_distinct(eez_territory),
#     territories = paste(unique(eez_territory), collapse = ", ")) %>% 
#   arrange(-n_territories)
# 
# data_summary
# 
# #sum all km2 by country
# country_sum <- data %>%
#   group_by(eez_country) %>%
#   summarise(across(ends_with("_km2"), sum, na.rm = TRUE))
# 
# country_sum
# names(country_sum)
# 
# write_csv(country_sum, 
#           here("50 reefs plus/country_sum_km2.csv"))
#   
