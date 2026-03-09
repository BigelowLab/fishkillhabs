# Climate envelope analysis

source("setup.R")

species = c("Margalefidinium polykrikoides", 
            "Karenia brevis", 
            "Karenia mikimotoi", 
            "Alexandrium catenella",
            "Noctiluca scintillans", 
            "Heterosigma akashiwo",
            "Chattonella marina",
            "Pseudochattonella verruculosa",
            "Prymnesium polylepis",
            "Chrysochromulina leadbeateri")

env = read_covariates(depth = TRUE)

# minimum and maximum of all covariates
path = file.path(data_path("climate_envelope"), "minmax_climate_envelope.csv")
ce = read_csv(path)
ix = !species %in% ce$species
min_max = lapply(species[ix], function(s) {
  obs <- read_obis(s) |>
    select(-depth)
  
  obs_env=extract_covars(env, obs, form = "wide") |>
    st_drop_geometry()
  
  new_ce = find_climate_env(obs_env, species=s, month=FALSE)
  return(new_ce)
}) |>
  bind_rows()

write_csv(min_max, path, append=TRUE)

# mean +- 1 standard deviation
path = file.path(data_path("climate_envelope"), "mean1sd_climate_envelope.csv")
ce = read_csv(path)
ix = !species %in% ce$species

mean_1sd = lapply(species[ix], function(s) {
  obs <- read_obis(s) |>
    select(-depth)
  
  obs_env=extract_covars(env, obs, form = "wide") |>
    st_drop_geometry()
  
  new_ce = find_climate_env(obs_env, species=s, month=FALSE, 
                            method="mean_sd", 
                            nsd=1)
  return(new_ce)
}) |>
  bind_rows()

write_csv(mean_1sd, path, append=TRUE)

# mean +- 2 standard deviations
path = file.path(data_path("climate_envelope"), "mean2sd_climate_envelope.csv")
ce = read_csv(path)
ix = !species %in% ce$species

mean_2sd = lapply(species[ix], function(s) {
  obs <- read_obis(s) |>
    select(-depth)
  
  obs_env=extract_covars(env, obs, form = "wide") |>
    st_drop_geometry()
  
  new_ce = find_climate_env(obs_env, species=s, month=FALSE, 
                            method="mean_sd", 
                            nsd=2)
  return(new_ce)
}) |>
  bind_rows()

write_csv(mean_2sd, path, append=TRUE)

