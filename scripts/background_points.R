# Script for collecting background points using OBIS obs

source("setup.R")

species = "Karenia mikimotoi"

obs <- read_obis(species)

obs

coast = rnaturalearth::ne_coastline(scale = 50, returnclass = "sf") |> 
  st_geometry()

mask = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif") |>
  set_names("depth") |>
  mutate(depth = ifelse(between(depth, 0, 500), 1, NA))


all_counts = count(st_drop_geometry(obs), month)

all_counts

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

thinned_obs = sapply(month.abb,
                     function(mon){ 
                       temp_x = obs |> filter(month == mon)
                       if(nrow(temp_x) == 0) return(NULL)
                       thin_by_cell(temp_x, mask)
                     }, simplify = FALSE) |>
  dplyr::bind_rows() 

thinned_counts = count(st_drop_geometry(thinned_obs), month)

thinned_counts

ggplot() +
  geom_sf(data = thinned_obs, 
          alpha = 0.2, 
          shape = "circle small", 
          size = 1) +
  geom_sf(data = coast, col = "orange") +
  geom_text(data = thinned_counts,
            mapping = aes(x = 90, 
                          y = 120, 
                          label = sprintf("n: %i", .data$n)),
            size = 3) + 
  labs(x = "Longitude", y = "Latitude", title = "Thinned observations") +
  facet_wrap(~month)


bias_map = rasterize_point_density(obs, mask)

ggplot() +
  geom_stars(data = bias_map, aes(fill = count)) +
  scale_fill_viridis_b(na.value = "transparent") +
  geom_sf(data = coast, col = "orange") + 
  labs(x = "Longitude", y = "Latitude", title = "Bias map using all observations")

nback_avg = mean(all_counts$n) |>
  round()
nback_avg


obsbkg = sapply(month.abb,
                function(mon){ 
                  temp_x = thinned_obs |> filter(month == mon)
                  sample_background(temp_x, # <- just this month
                                    bias_map,
                                    method = "bias",  # <-- it needs to know it's a bias map
                                    return_pres = TRUE, # <-- give me the obs back, too
                                    n = nback_avg) |>   # <-- how many points
                    mutate(month = mon, .before = 1)
                }, simplify = FALSE) |>
  bind_rows() |>
  mutate(month = factor(month, levels = month.abb))
obsbkg 

count(st_drop_geometry(obsbkg), month, class)


write_model_input(obsbkg, scientificname = species)
