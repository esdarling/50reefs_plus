#Aug 2026 - revision comparison of different extents

library(mermaidr)
library(tidyverse)
library(janitor)
library(here)
library(readxl)

# Helper: read, clean, rename, and drop summary/total rows at the bottom
read_reef_summary <- function(path) {
  df <- read_xlsx(path) %>% 
    clean_names() %>% 
    rename("eez_countries_territories" = countries_territories_and_maritime_jurisdictions_with_coral_reef_ee_zs)
  
  # Stop before the first fully-NA "totals" row (e.g. eez_country == NA)
  first_na_row <- which(is.na(df$eez_country))[1]
  if (!is.na(first_na_row)) {
    df <- df[seq_len(first_na_row - 1), ]
  }
  
  df
}

# Read 4 extents
here()
aca_wcmc <- read_reef_summary(here("aug2026-results tables/ACA_FullExtent_plus_WCMC__table_s6_eez_and_beyer_summaries.xlsx"))
aca_coral <- read_reef_summary(here("aug2026-results tables/ACA_CoralAlgae__table_s6_eez_and_beyer_summaries.xlsx"))
aca_coral_wcmc <- read_reef_summary(here("aug2026-results tables/ACA_CoralAlgae_plus_WCMC__table_s6_eez_and_beyer_summaries.xlsx"))
wcmc <- read_reef_summary(here("aug2026-results tables/WCMC__table_s6_eez_and_beyer_summaries.xlsx"))

# Quick check
map(list(aca_wcmc, aca_coral, aca_coral_wcmc, wcmc), tail, 3)

# Create table of key summaries
# aca_wcmc %>% 
#   summarize(total_reef = sum(total_reef_extent_km2), 
#             total_50reefs_plus = sum(x50reefs_plus_km2),
#             n_eez_country = n_distinct(eez_country[eez_country != "None"]),
#             n_territories_only = n_distinct(eez_countries_territories[eez_countries_territories != "None" & 
#                                                                         eez_countries_territories != eez_country]))
# aca_wcmc %>% 
#   filter(eez_countries_territories != "None" & 
#            eez_countries_territories != eez_country) %>% 
#   distinct(eez_country, eez_countries_territories) %>% 
#   arrange(eez_country) %>% 
#   print(n= Inf)
# 
# aca_wcmc %>% 
#   filter(eez_country == "None")

# Put datasets into a named list for easy iteration
datasets <- list(
  aca_wcmc = aca_wcmc,
  aca_coral = aca_coral,
  wcmc = wcmc, 
  aca_coral_wcmc = aca_coral_wcmc
)

# One summary table with 4 rows, one per dataset
summary_table <- imap_dfr(datasets, function(df, name) {
  df %>% 
    summarize(
      dataset = name,
      total_reef = sum(total_reef_extent_km2), 
      total_50reefs_plus = sum(x50reefs_plus_km2),
      n_eez_country = n_distinct(eez_country[eez_country != "None"]),
      n_territories_only = n_distinct(eez_countries_territories[eez_countries_territories != "None" & 
                                                                   eez_countries_territories != eez_country])
    )
}) %>% 
  relocate(dataset)

summary_table
write_csv(summary_table, here("aug2026-results tables/reef_summary_table.csv"))

# Print territories list for each dataset
#install.packages("writexl")
library(writexl)

territory_sheets <- imap(datasets, function(df, name) {
  df %>% 
    filter(eez_countries_territories != "None" & 
             eez_countries_territories != eez_country) %>% 
    distinct(eez_country, eez_countries_territories) %>% 
    arrange(eez_country)
})

write_xlsx(territory_sheets, here("aug2026-results tables/territories_by_dataset.xlsx"))

#top10 countries
top10_sheets <- imap(datasets, function(df, name) {
  df %>% 
    filter(eez_country != "None") %>% 
    slice_max(x50reefs_plus_km2, n = 10) %>% 
    select(eez_country, eez_countries_territories, x50reefs_plus_km2)
})

write_xlsx(top10_sheets, here("aug2026-results tables/top10_by_dataset.xlsx"))
