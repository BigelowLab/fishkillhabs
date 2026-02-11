

source("setup.R")

species = "Noctiluca scintillans"

cfg = read_configuration(scientificname = species,
                         version = "v1", 
                         path = data_path("models"))

present_conditions = read_covariates()

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