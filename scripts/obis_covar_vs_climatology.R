# comparison between obis provided covariates vs copernicus climatology (salinity, sst and depth)


source("setup.R")

occ = read_sf("/mnt/ecocast/projectdata/fishkillhabs/obis/Karenia_mikimotoi.gpkg") |>
  dplyr::mutate(month = factor(month, levels = month.abb)) |>
  rename(sst_obis = sst,
         sss_obis = sss,
         depth_obis = depth,
         bathymetry_obis = bathymetry) |>
  filter(!is.na(month))

sst = stars::read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/mon_sst.tif", proxy=FALSE) |>
  stars::st_set_dimensions("band",
                           values = month.abb,
                           names = "month")
names(sst) = c("sst")
sss = stars::read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/mon_sss.tif", proxy=FALSE) |>
  stars::st_set_dimensions("band",
                           values = month.abb,
                           names = "month")
names(sss) = c("sss")

env <- c(sst, sss, tolerance = 1e-06, along=NA_integer_)

depth = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif")
names(depth) = c("depth")
dd = sapply(month.abb, function(mon) {depth}, simplify = FALSE)
depth = do.call(c, append(dd, list(along = list(month = month.abb))))

present_conditions = c(env, depth, tolerance = 1e-06, along=NA_integer_)


variables = extract_covars(present_conditions, occ, form = "wide")


ggplot(variables, aes(x = sst_obis, y=sst)) +
  geom_point()

ggplot(variables, aes(x = sss_obis, y=sss)) +
  geom_point()

ggplot(variables, aes(x = depth_obis, y=depth)) +
  geom_point()

ggplot(variables, aes(x = bathymetry_obis, y=depth)) +
  geom_point()
