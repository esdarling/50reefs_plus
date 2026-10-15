#Oct 2026 - revision comparison of different extents

library(mermaidr)
library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(writexl)

# Lookup: short R name = exact column header in the Supplementary Tables (S6–S8)
col_lookup <- c(
  eez_country                       = "EEZ Country",
  eez_countries_territories         = "Countries, Territories, and Maritime Jurisdictions with Coral Reef EEZs",
  total_reef_extent_km2             = "Total Reef Extent (km²)",
  x50reefs_plus_km2                 = "50Reefs+ (km²)",
  x50reefs_plus_pct                 = "50Reefs+ (% of total)",
  new_50reefs_plus_km2              = "New 50Reefs+ Selections (km²)",
  new_50reefs_plus_pct              = "New 50Reefs+ Selections (% of total)",
  x50reefs_plus_beyer_overlap_km2   = "50Reefs+ overlap with Beyer (km²)",
  x50reefs_plus_beyer_overlap_pct   = "50Reefs+ overlap with Beyer (% of total)",
  beyer_original_km2                = "Original Beyer Extent (km²)",
  beyer_original_pct                = "Original Beyer (% of total)",
  beyer_only_km2                    = "Beyer Only (km²)",
  beyer_only_pct                    = "Beyer Only (% of total)",
  not_selected_km2                  = "Not Selected by Either (km²)",
  not_selected_pct                  = "Not Selected by Either (% of total)"
)

# Helper: read, rename, and drop summary/total rows at the bottom
read_reef_summary <- function(path) {
  df <- read_xlsx(path)

  # Flag any expected columns that are missing from this file
  missing_cols <- setdiff(col_lookup, names(df))
  if (length(missing_cols) > 0) {
    warning(basename(path), " is missing columns: ", paste(missing_cols, collapse = "; "))
  }

  df <- df %>%
    rename(any_of(col_lookup)) %>%
    clean_names()   # tidies any unexpected extra columns; leaves the names above unchanged

  # Stop before the first fully-NA row (blank row, then the totals row)
  first_na_row <- which(is.na(df$eez_country))[1]
  if (!is.na(first_na_row)) {
    df <- df[seq_len(first_na_row - 1), ]
  }

  df
}

# Read 3 extents
here()
aca_wcmc  <- read_reef_summary(here("oct2026-results tables/Zawada et al. Supplementary Table_S6.xlsx"))
wcmc      <- read_reef_summary(here("oct2026-results tables/Zawada et al. Supplementary Table_S7.xlsx"))
aca_coral <- read_reef_summary(here("oct2026-results tables/Zawada et al. Supplementary Table_S8.xlsx"))

# Quick check
map(list(aca_wcmc, wcmc, aca_coral), tail, 3)

# Put datasets into a named list for easy iteration
datasets <- list(
  aca_wcmc  = aca_wcmc,
  wcmc      = wcmc,
  aca_coral = aca_coral
)

# One summary table with 3 rows, one per dataset
summary_table <- imap_dfr(datasets, function(df, name) {
  df %>%
    summarize(
      dataset = name,
      total_reef = sum(total_reef_extent_km2),
      total_50reefs_plus = sum(x50reefs_plus_km2),
      total_new_50reefs_plus = sum(new_50reefs_plus_km2),
      total_50reefs_plus_beyer_overlap = sum(x50reefs_plus_beyer_overlap_km2),
      total_beyer_original = sum(beyer_original_km2),
      total_beyer_only = sum(beyer_only_km2),
      total_not_selected = sum(not_selected_km2),
      n_eez_country = n_distinct(eez_country[eez_country != "None"]),
      n_territories_only = n_distinct(eez_countries_territories[eez_countries_territories != "None" &
                                                                   eez_countries_territories != eez_country])
    )
}) %>%
  relocate(dataset)

summary_table
write_csv(summary_table, here("oct2026-results tables/reef_summary_table.csv"))

# Territories list for each dataset
territory_sheets <- imap(datasets, function(df, name) {
  df %>%
    filter(eez_countries_territories != "None" &
             eez_countries_territories != eez_country) %>%
    distinct(eez_country, eez_countries_territories) %>%
    arrange(eez_country)
})

write_xlsx(territory_sheets, here("oct2026-results tables/territories_by_dataset.xlsx"))

# Top 10 countries by 50Reefs+ area
top10_sheets <- imap(datasets, function(df, name) {
  df %>%
    filter(eez_country != "None") %>%
    slice_max(x50reefs_plus_km2, n = 10) %>%
    select(eez_country, eez_countries_territories, x50reefs_plus_km2, x50reefs_plus_pct)
})

write_xlsx(top10_sheets, here("oct2026-results tables/top10_by_dataset.xlsx"))
