## Produce daily global presence maps for 10 (pilot) fish-killing HAB species

# packages

source("setup.R")

species = "Alexandrium catenella"
model_v = "v3"

cfg = read_configuration(scientificname = species,
                         version = model_v, 
                         path = data_path("models"))

# load global covariates

daily_covar = read_covariates_daily()

# load pre-trained model

file = gsub(" ", "-", sprintf("%s-%s-model_fits", species, model_v))

model_fit = read_model_fit(filename = file) |>
  filter(wflow_id %in% c("default_rf"))
model_fit


# predict - one species

model = workflows::extract_fit_engine(model_fit$.workflow[[1]])

mtype = model_fit_spec(model_fit)

p = predict(daily_covar, model, type = get_response_type(mtype))[1]

#colors = c("magma",
#           "inferno",
#           "plasma",
#           "viridis",
#           "cividis",
#           "rocket",
#           "mako",
#           "turbo")[1]
#
#risk_map = ggplot2::ggplot() +
#  stars::geom_stars(data = p) + 
#  ggplot2::scale_fill_viridis_c(option = colors[1], 
#                                limits = c(0,1), 
#                                na.value = "grey50")
#risk_map

species <- c("Margalefidinium polykrikoides",
             "Heterosigma akashiwo",
             "Noctiluca scintillans", 
             "Karenia mikimotoi",
             "Karenia brevis",
             "Alexandrium catenella",
             "Chattonella marina",
             "Pseudochattonella verruculosa",
             "Prymnesium polylepis",
             "Chrysochromulina leadbeateri")

risk_maps = lapply(species,
                   function(s) {
                     cfg = read_configuration(scientificname = s,
                                              version = "v3", 
                                              path = data_path("models"))
                     
                     file = gsub(" ", "-", sprintf("%s-%s-model_fits", s, model_v))
                     
                     model_fit = read_model_fit(filename = file) |>
                       filter(wflow_id %in% c("default_rf"))
                     
                     model = workflows::extract_fit_engine(model_fit$.workflow[[1]])
                     
                     mtype = model_fit_spec(model_fit)
                     
                     p = predict(daily_covar, model, type = get_response_type(mtype))[1]
                     
                     outpath = file.path("predictions", sprintf("%s-riskmap-%s.tif", 
                                                                gsub(" ", "_", species[1], fixed = TRUE),
                                                                Sys.Date()))
                     
                     write_stars(p, outpath)
                   })

# format for google cloud

outfile = file.path("predictions", sprintf("%s-riskmap-%s.tif", 
                                            gsub(" ", "_", species[1], fixed = TRUE),
                                            Sys.Date()))

write_stars(p, outfile)

# write to cloud storage




