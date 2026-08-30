#check-50-reefs-tiff

library(terra)
library(here)

r <- rast(here("prioritisations_250m.tif"))
r

crs(r)
res(r)

global(r, fun = c("min", "max", "mean"), na.rm = TRUE)
plot(r, col = hcl.colors(50, "YlOrRd"))

nlyr(r)

