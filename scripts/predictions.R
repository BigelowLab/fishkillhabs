# Load fitted models and predict on monthly climatologies

source("setup.R")

species = "Karenia brevis"
model_v = "v2"

mask = read_mask()

cfg = read_configuration(scientificname = species,
                         version = model_v, 
                         path = data_path("models"))

present_conditions = read_covariates() |>
  mutate(depth = log10(depth))

covar_crop = st_crop(present_conditions, mask)

file = gsub(" ", "-", sprintf("%s-%s-model_fits", species, model_v))

model_fits = read_model_fit(filename = file) |>
  filter(!wflow_id %in% c("default_maxent"))
model_fits

nowcast = predict_stars(model_fits, present_conditions)
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

