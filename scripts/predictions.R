

source("setup.R")

species = "Noctiluca scintillans"

cfg = read_configuration(scientificname = species,
                         version = "v1", 
                         path = data_path("models"))

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

model_fits = read_model_fit(filename = "Noctiluca-scintillans-v1-model_fits")
model_fits

nowcast = predict_stars(model_fits, present_conditions)
nowcast

plot_prediction(nowcast['default_btree'])

pa_nowcast = threshold_prediction(nowcast)
plot_prediction(pa_nowcast['default_btree'])

path = make_path("predictions")

write_prediction(nowcast,
                 scientificname = cfg$scientificname,
                 version = cfg$version,
                 year = "CURRENT",
                 scenario = "CURRENT")
write_prediction(forecast_2075,
                 scientificname = cfg$scientificname,
                 version = cfg$version,
                 year = "2075",
                 scenario = "RCP85")