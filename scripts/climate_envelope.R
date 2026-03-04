# Climate envelope analysis

source("setup.R")

species = c("Margalefidinium polykrikoides", "Karenia brevis", "Karenia mikimotoi", "Alexandrium catenella",
            "Noctiluca scintillans", "Heterosigma akashiwo")

env = read_covariates(depth = TRUE)

# minimum and maximum of all covariates
min_max = lapply(species, function(s) {
  obs <- read_obis(s) |>
    select(-depth)
  
  obs_env=extract_covars(env, obs, form = "wide") |>
    st_drop_geometry()
  
  ce = find_climate_env(obs_env, species=s, month=FALSE)
  return(ce)
}) |>
  bind_rows()

write_csv(min_max, file.path(ROOT_DATA_PATH, "climate_envelope", "minmax_climate_envelope.csv"))

# mean +- 1 standard deviation
mean_1sd = lapply(species, function(s) {
  obs <- read_obis(s) |>
    select(-depth)
  
  obs_env=extract_covars(env, obs, form = "wide") |>
    st_drop_geometry()
  
  ce = find_climate_env(obs_env, species=s, month=FALSE, method="mean_sd", nsd=1)
  return(ce)
}) |>
  bind_rows()

write_csv(mean_1sd, file.path(ROOT_DATA_PATH, "climate_envelope", "mean1sd_climate_envelope.csv"))

# mean +- 2 standard deviations
mean_2sd = lapply(species, function(s) {
  obs <- read_obis(s) |>
    select(-depth)
  
  obs_env=extract_covars(env, obs, form = "wide") |>
    st_drop_geometry()
  
  ce = find_climate_env(obs_env, species=s, month=FALSE, method="mean_sd", nsd=2)
  return(ce)
}) |>
  bind_rows()

write_csv(mean_2sd, file.path(ROOT_DATA_PATH, "climate_envelope", "mean2sd_climate_envelope.csv"))

