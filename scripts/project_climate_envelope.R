# Projects temp and salinity climate envelope onto monthly climatology for one species

source("setup.R")

ce = read_csv(file.path(data_path("climate_envelope"), "mean2sd_climate_envelope.csv")) |>
  filter(species == "Karenia brevis")

env = read_covariates()

m = slice(env, month, "Dec")

pred = predict_climate_env(ce, m)

ggplot() + 
  geom_stars(data=pred, aes(fill=pred)) +
  theme_bw() +
  coord_sf()

# project all months in facet plot

preds_m = predict_climate_env(ce, env, month=TRUE)


p = ggplot() + 
  geom_stars(data=preds_m, aes(fill=pred), downsample=5) +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

filename = sprintf("%s_%s_%s.png",
                   species = "kb",
                   mtype = "mean1sd",
                   region = "floridagulf")

ggsave(file.path(data_path("predictions"), filename), p, width = 9.5, height = 8, units="in")

library(cofbb)

# northwest atlantic
bb = get_bb("nwa", "sf")

# florida + gulf
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -100, ymin = 24, xmax = -75, ymax = 35)), crs = 4326))

#northeast atlantic
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -13, ymin = 50, xmax = 30, ymax = 75)), crs = 4326))

#pacific northwest
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -130, ymin = 40, xmax = -120, ymax = 60)), crs = 4326))

# japan + korea
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = 122, ymin = 30, xmax = 150, ymax = 48)), crs = 4326))


sf_use_s2(FALSE)
z = st_crop(preds_m, bb)


p = ggplot() + 
  geom_stars(data=z, aes(fill=pred)) +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

p

filename = sprintf("%s_%s_%s.png",
                   species = "kb",
                   mtype = "mean1sd",
                   region = "floridagulf")

ggsave(file.path(data_path("predictions"), filename), p, width = 9.5, height = 8, units="in")


# add haedat events

h_km = read_haedat(species = "Karenia mikimotoi")

h_kb = read_haedat(species = "Karenia brevis")
  
cm = read_haedat(species = "Chattonella marina")

pv = read_haedat(species = "Pseudochattonella verruculosa")

ha = read_haedat(species = "Heterosigma akashiwo", country = c("KOREA, REPUBLIC OF"))


p = ggplot() + 
  geom_stars(data=z, aes(fill=pred)) +
  geom_sf(data = h_kb, color="red") +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

p

filename = sprintf("%s_%s_%s_haedat.png",
                   species = "kb",
                   mtype = "mean1sd",
                   region = "floridagulf")

ggsave(file.path(data_path("predictions"), filename), p, width = 9.5, height = 8, units="in")


