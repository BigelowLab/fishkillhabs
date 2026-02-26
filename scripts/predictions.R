# Load fitted models and predict on monthly climatologies

source("setup.R")

species = "Karenia brevis"
model_v = "v2"

cfg = read_configuration(scientificname = species,
                         version = model_v, 
                         path = data_path("models"))

present_conditions = read_covariates()

present_conditions = mutate(present_conditions, depth = log10(depth))

model_fits = read_model_fit(filename = "Noctiluca-scintillans-v2-model_fits") |>
  filter(!wflow_id %in% c("default_maxent"))
model_fits

nowcast = predict_stars(model_fits, present_conditions)
nowcast

plot_prediction(nowcast['default_glm'])

plot_prediction(nowcast['default_btree'])

pa_nowcast = threshold_prediction(nowcast)
plot_prediction(pa_nowcast['default_rf'])

path = make_path("predictions")

write_prediction(nowcast,
                 scientificname = cfg$scientificname,
                 version = cfg$version,
                 year = "CURRENT",
                 scenario = "CURRENT")

###

