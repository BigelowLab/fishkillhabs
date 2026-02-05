# Script for collecting background points using OBIS obs

source("setup.R")

species = "Noctiluca scintillans"

obs <- read_obis(species) |>
  mutate(month = factor(format(eventDate, format="%b"), levels = month.abb))

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


## bias_map = rasterize_point_density(obs, mask)
x=obs
y=mask
name = "count"
y = stars:::st_upfront(y)
y = y[1]
d = dim(y)
if(length(d) > 2){
  y = dplyr::slice(y, names(d)[3], 1)  
}

v = sf::st_as_sf(y)
# trim the points to just a "name" attribute
x = dplyr::mutate(x, {{ name }} := 1) |>
  dplyr::select(dplyr::all_of(name))

# aggregate by counting the instances of x in each element of y 
# (or the polygonized-by-cell "v" version) and then cast back to 
# the template raster
bias_map = aggregate(x, v, FUN = length) |>
  stars::st_rasterize(template = y, align = TRUE)

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

path = file.path(ROOT_DATA_PATH)
spname = gsub(" ", "_", species, fixed = TRUE)
fname = if (is.null(version)){
  sprintf("%s-model_input.gpkg", spname)
} else {
  sprintf("%s-%s-model_input.gpkg", spname, version)
}
filename = file.path(path, fname)
x = sf::write_sf(obsbkg, filename)
invisible(x)

model_input = sf::read_sf(filename)
"/mnt/ecocast/projectdata/fishkillhabs/Noctiluca_scintillans-model_input.gpkg"
