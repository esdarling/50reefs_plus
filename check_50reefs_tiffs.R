library(terra)
library(tidyverse)
library(here)

here()

terraOptions(progress = 1)   # shows a progress bar so you can tell it's working

tif_paths <- list.files(here(), pattern = "\\.tiff?$", full.names = TRUE, ignore.case = TRUE)
tif_paths

# Count 1s (and total non-NA cells) for one file, in a single pass
count_ones <- function(p) {
  r  <- rast(p)
  fq <- freq(r)                         # one read: counts of every value
  tibble(
    file     = basename(p),
    n_ones   = sum(fq$count[fq$value == 1]),
    n_cells  = sum(fq$count),           # all non-NA cells
    pct_ones = round(100 * n_ones / n_cells, 1)
  )
}

# Run one at a time
res1 <- count_ones(tif_paths[1]); res1
res2 <- count_ones(tif_paths[2]); res2
res3 <- count_ones(tif_paths[3]); res3

# Combine once all three are done
ones_summary <- bind_rows(res1, res2, res3)
ones_summary
write_csv(ones_summary, here("tif_ones_summary.csv"))