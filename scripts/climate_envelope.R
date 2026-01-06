

source("setup.R")

species = "Margalefidinium polykrikoides"
x <- read_obis(species)

xx <- mutate(x, month = format(eventDate, format = "%m")) |>
  filter(!is.na(month), shoredistance < 21000) 

# some shoredistances negative actually inshore

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data=x, color="red", size=1) +
  labs(x = element_blank(), y=element_blank(), title = paste("All OBIS", species, "occurrences", sep=" ")) +
  theme_bw()

ggplot() +
  geom_sf(color="red", size=1) +
  geom_sf(data = world) +
  labs(x = element_blank(), y=element_blank()) +
  theme_bw()


monthly_occurence <- group_by(xx, month) |>
  summarise(min_sst = min(sst, na.rm=TRUE),
            max_sst = max(sst, na.rm=TRUE),
            min_sss = min(sss, na.rm=TRUE),
            max_sss = max(sss, na.rm=TRUE))

ggplot(monthly_occurence) +
  geom_sf(color="red", size=1) +
  geom_sf(data = world) +
  labs(x = element_blank(), y=element_blank()) +
  theme_bw() +
  facet_wrap(vars(month))
  

pivot_longer(xx, cols = c(sst, sss)) |>
  ggplot(aes(x=month, y=value)) +
  geom_boxplot() +
  facet_grid(cols = vars(name))


