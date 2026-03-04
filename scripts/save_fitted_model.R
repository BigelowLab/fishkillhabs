

source("setup.R")

species = "Karenia brevis"
model_v = "v2"

model_fits = read_model_fit(filename = file) |>
  filter(!wflow_id %in% c("default_maxent"))

model_fits

model_fits$.workflow[2][[1]]$fit

rf_fit = model_fits$.workflow[2][[1]]$fit$fit

save(rf_fit, file="mymodel.rds")

save(rf_fit, file="mymodel.hdf5")


