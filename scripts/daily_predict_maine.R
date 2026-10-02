## Produce daily global presence maps for 10 (pilot) fish-killing HAB species

setwd("/mnt/ecocast/projects/fishkillhabs")

source("setup.R")

model_v = "v3"

daily_covar = read_covariates_daily(downsample=FALSE)

sf_use_s2(FALSE)

me_daily_covar = st_crop(daily_covar, st_bbox(cofbb::get_bb("gom", form = "bb"), crs = st_crs(daily_covar)))

species <- c("Karenia mikimotoi",
             "Alexandrium catenella")

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
                     
                     p = predict(me_daily_covar, model, type = get_response_type(mtype))[1]
                     names(p) = "predicted_probability"
                     
                     outpath_stars = file.path("/mnt/ecocast/projects/fishkillhabs/predictions", 
                                               gsub(" ", "_", tolower(s), fixed = TRUE), 
                                               sprintf("%s-riskmap_gom.tif", 
                                                       gsub(" ", "_", s, fixed = TRUE)))
                     
                     write_stars(p, outpath_stars,driver="COG")
                   })

colors = c("magma",
           "inferno",
           "plasma",
           "viridis",
           "cividis",
           "rocket",
           "mako",
           "turbo")[1]

p = read_stars("/mnt/ecocast/projects/fishkillhabs/predictions/karenia_mikimotoi/Karenia_mikimotoi-riskmap_gom.tif")
names(p) = "probability"
COAST = oame::read_coast()


risk_map = ggplot2::ggplot() +
  stars::geom_stars(data = p) + 
  geom_sf(data=COAST) +
  ggplot2::scale_fill_viridis_c(option = colors[1], 
                                limits = c(0,1), 
                                na.value = "grey50") +
  #labs(title = "Karenia mikimotoi presence risk 9 September 2026") +
  theme(axis.title = element_blank())
risk_map

plot_file = sprintf("/mnt/ecocast/projects/fishkillhabs/predictions/karenia_mikimotoi/karenia_mikimotoi_riskmap_gom_%s.png", Sys.Date())

ggsave(plot_file, risk_map, width=8, height=8)

