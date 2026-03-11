## Subset predictions

source("setup.R")

library(cofbb)

p = read_prediction(scientificname = "Karenia brevis",
                version = "v2",
                year = "CURRENT",
                scenario = "CURRENT",
                path = data_path("predictions"))


# global predictions, one month

x = slice(p, month, "Jan")

gg = ggplot2::ggplot() +
  stars::geom_stars(data = x[2]) + 
  ggplot2::scale_fill_viridis_c(option = "magma", 
                                limits = c(0,1), 
                                na.value = "grey50") +
  coord_sf()

gg

# regional subset, all months

bb = get_bb("nwa", "sf")

bb = sf::st_sf(name = "bounding box",
          geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -88, ymin = 24, xmax = -78, ymax = 32)), crs = 4326))

sf_use_s2(FALSE)
z = st_crop(p, bb)


gg = ggplot2::ggplot() +
  stars::geom_stars(data = z[2]) + 
  geom_sf(data = h_kb, color="red") +
  ggplot2::scale_fill_viridis_c(option = "magma", 
                                limits = c(0,1), 
                                na.value = "grey50") +
  facet_wrap(vars(month)) +
  coord_sf()

gg

filename = sprintf("%s_%s_%s_haedat.png",
                   species = "kb",
                   mtype = "glm",
                   region = "floridagulf")

ggsave(file.path(data_path("predictions"), filename), gg, width = 9.5, height = 8, units="in")


# presence/absence predictions

pa = threshold_prediction(p)

z = st_crop(p, bb)

gg = ggplot2::ggplot() +
  stars::geom_stars(data = z[2]) + 
  geom_sf(data = h_kb, color="red") +
  facet_wrap(vars(month)) +
  coord_sf()

gg

plot_prediction(pa['default_btree'])

