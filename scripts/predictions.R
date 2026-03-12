# Load fitted models and predict on monthly climatologies

source("setup.R")

species = "Alexandrium catenella"
model_v = "v3"

#mask = read_mask()

cfg = read_configuration(scientificname = species,
                         version = model_v, 
                         path = data_path("models"))

present_conditions = read_covariates() |>
  mutate(depth = log10(depth))

present_conditions = st_warp(present_conditions, crs = st_crs(present_conditions), cellsize = 0.33)

#sf_use_s2(FALSE)
#covar_crop = st_crop(present_conditions, mask)

file = gsub(" ", "-", sprintf("%s-%s-model_fits", species, model_v))

model_fits = read_model_fit(filename = file) |>
  filter(!wflow_id %in% c("default_maxent"))
model_fits

nowcast = predict_stars(model_fits, present_conditions)
nowcast

nowcast_crop = predict_stars(model_fits, covar_crop)
nowcast


plot_prediction(nowcast['default_glm'])

plot_prediction(nowcast['default_btree'])

plot_prediction(nowcast['default_rf'])

pa_nowcast = threshold_prediction(nowcast)
plot_prediction(pa_nowcast['default_rf'])

path = make_path("predictions")

write_prediction(nowcast,
                 scientificname = cfg$scientificname,
                 version = cfg$version,
                 year = "CURRENT",
                 scenario = "CURRENT")

###

