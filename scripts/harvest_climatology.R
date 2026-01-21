# script for harvesting global monthly temp and salinity 

library(stars)
library(dplyr)

file = "/mnt/ecocast/projectdata/fishkillhabs/cmems_mod_glo_phy_my_0.083deg-climatology_P1M-m.nc"

# example

x <- read_stars(file, proxy=FALSE)

z <- st_set_crs(x, 4326) |>
  slice("time", 1) |>
  slice("depth", 1)

write_stars(z, "/mnt/ecocast/projectdata/fishkillhabs/so_01.tif", layer = 2)

a = read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/so_1.tif")
b = read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/thetao_1.tif")


# all months
for (month in 1:12) {
  z <- st_set_crs(x, 4326) |>
    slice("time", month) |>
    slice("depth", 1)
  
  write_stars(z, paste("/mnt/ecocast/projectdata/fishkillhabs/climatology/thetao_", month, ".tif", sep=""), layer = 1)
  write_stars(z, paste("/mnt/ecocast/projectdata/fishkillhabs/climatology/so_", month, ".tif", sep=""), layer = 2)
}




