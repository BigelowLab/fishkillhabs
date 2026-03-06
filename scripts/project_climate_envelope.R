# Projects temp and salinity climate envelope onto monthly climatology for one species

source("setup.R")

ce = read_csv(file.path(ROOT_DATA_PATH, "climate_envelope", "mean1sd_climate_envelope.csv")) |>
  filter(species == "Heterosigma akashiwo")

env = read_covariates()

m = slice(env, month, "Dec")

pred = mutate(m, 
              pred = ifelse(between(thetao, ce$min_sst, ce$max_sst) & 
                              between(so, ce$min_sss, ce$max_sss) & 
                              between(mlotst, ce$min_mld, ce$max_mld) & 
                              between(bottomT, ce$min_sbt, ce$max_sbt) & 
                              depth <= 500, 1, 0))

ggplot() + 
  geom_stars(data=pred, aes(fill=pred)) +
  theme_bw() +
  #theme(legend.position="none", axis.title = element_blank()) +
  #facet_wrap(vars(new_dim)) +
  coord_sf()


# project all months in facet plot

preds_m = lapply(month.abb,
                 function(mon){
                   m = slice(env, month, mon)
                   
                   pred = mutate(m, 
                                 pred = ifelse(between(thetao, ce$min_sst, ce$max_sst) & 
                                                 between(so, ce$min_sss, ce$max_sss) & 
                                                 between(mlotst, ce$min_mld, ce$max_mld) & 
                                                 between(bottomT, ce$min_sbt, ce$max_sbt) & 
                                                 depth <= 500, 1, 0))
                   pred["pred"]
                 })

names(preds_m) = month.abb

r = do.call("c", preds_m)                                                                                                        
r = st_redimension(r)                                                                                                            
names(r) = "pred"


ggplot() + 
  geom_stars(data=r, aes(fill=pred)) +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

library(cofbb)

# northwest atlantic
bb = get_bb("nwa", "sf")

# florida
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -100, ymin = 24, xmax = -75, ymax = 35)), crs = 4326))

#northeast atlantic
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -13, ymin = 50, xmax = 30, ymax = 75)), crs = 4326))

#pacific northwest
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -130, ymin = 40, xmax = -120, ymax = 60)), crs = 4326))


sf_use_s2(FALSE)
z = st_crop(r, bb)


ggplot() + 
  geom_stars(data=z, aes(fill=pred)) +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

ggsave(file.path("/mnt/ecocast/projectdata/fishkillhabs/predictions", "km_allmonths_minmax_csf.png"), p, width = 9.5, height = 8, units="in")


# add haedat events

h = read_haedat()

h_km = filter(h, causativeSpeciesName0 == "Karenia mikimotoi") |> 
  mutate(new_dim = as.factor(format(as.Date(initialDate), format = "%b"))) |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(new_dim)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)

h_kb = filter(h, causativeSpeciesName0 == "Karenia brevis") |> 
  mutate(new_dim = as.factor(format(as.Date(initialDate), format = "%b"))) |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(new_dim)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)


kb_florida = ggplot() + 
  geom_stars(data=z, aes(fill=pred)) +
  geom_sf(data = h_kb, color="red") +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

ggsave(file.path("/mnt/ecocast/projectdata/fishkillhabs/predictions", "kb_mean2sd_florida_haedat.svg"), kb_florida, width = 9.5, height = 8, units="in")


h_ha = filter(h, causativeSpeciesName0 == "Heterosigma akashiwo", massMortal == 1, !is.na(latitude), !is.na(longitude))

