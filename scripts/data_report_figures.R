

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
})


h = read_csv("~/Documents/data/haedat/haedat_search.csv")

glimpse(h)

count(h, massMortal)

mm <- filter(h, massMortal == 1)

count(mm, causativeSpeciesName0) |>
  arrange(desc(n))

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data=mm, aes(x=longitude, y=latitude), color="red", size=1) +
  labs(x = element_blank(), y=element_blank(), title="HAEDAT Mass Mortality Record Distribution") +
  theme_bw()


mm_top_5 <- filter(mm, causativeSpeciesName0 %in% c("Karenia brevis", "Karenia mikimotoi", "Heterosigma akashiwo", 
                                                    "Cochlodinium polykrikoides", "Gymnodinium breve"))

ggplot(data = world) +
  geom_sf() +
  geom_point(data=mm_top_5, aes(x=longitude, y=latitude), color="red", size=1) +
  facet_wrap(vars(causativeSpeciesName0)) +
  labs(x = element_blank(), y=element_blank(), title="HAEDAT Top 5 Mass Mortality Taxa Distribution") +
  theme_bw()
