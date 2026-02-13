

neg_bath = filter(obs, bathymetry < 0)

ggplot() +
  geom_sf(data = obs, alpha = 0.2, shape = "circle small", size = 1) +
  geom_sf(data = coast, col = "orange") +
  geom_text(data = all_counts,
            mapping = aes(x = 90, 
                          y = 120, 
                          label = sprintf("n: %i", .data$n)),
            size = 3) + 
  labs(x = "Longitude", y = "Latitude", title = "All observations") +
  facet_wrap(~month)


ggplot() +
  geom_stars(data=mask) +
  geom_sf(data=coast, col="orange") +
  geom_sf(data = obs, alpha = 0.2, shape = "circle small", size = 1)
