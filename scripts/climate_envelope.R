

source("setup.R")

# Climate envelope analysis

mp <- read_obis("Margalefidinium polykrikoides")

find_climate_env(mp)
plot_climate_env(mp)


ha <- read_obis("Heterosigma akashiwo")

find_climate_env(ha)
plot_climate_env(ha)


ns <- read_obis("Noctiluca scintillans")

find_climate_env(ns)
plot_climate_env(ns)


km <- read_obis("Karenia mikimotoi")

find_climate_env(km)
plot_climate_env(km)


kb <- read_obis("Karenia brevis")

find_climate_env(kb)
plot_climate_env(kb)


# example occurence plot

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data=kb, color="red", size=1) +
  theme_bw()




