# Script for collecting background points using OBIS obs

source("setup.R")

species = "Karenia brevis"

obs <- read_obis(species)

obs

coast = read_coastline()

mask = read_mask()


all_counts = count(st_drop_geometry(obs), month)

all_counts

thinned_obs = sapply(month.abb,
                     function(mon){ 
                       temp_x = obs |> filter(month == mon)
                       if(nrow(temp_x) == 0) return(NULL)
                       thin_by_cell(temp_x, mask)
                     }, simplify = FALSE) |>
  dplyr::bind_rows() 

thinned_counts = count(st_drop_geometry(thinned_obs), month)

thinned_counts

thinned_obs = obs
thinned_counts = all_counts

bias_map = rasterize_point_density(obs, mask)

nback_avg = mean(all_counts$n) |>
  round()
nback_avg

obsbkg = sapply(month.abb,
                function(mon){ 
                  temp_x = thinned_obs |> filter(month == mon)
                  sample_background(temp_x, # <- just this month
                                    #bias_map,
                                    mask,
                                    method = "random",  # <-- it needs to know it's a bias map
                                    return_pres = TRUE, # <-- give me the obs back, too
                                    n = nback_avg) |>   # <-- how many points
                    mutate(month = mon, .before = 1)
                }, simplify = FALSE) |>
  bind_rows() |>
  mutate(month = factor(month, levels = month.abb))

obsbkg 

obsbkg_counts = count(st_drop_geometry(obsbkg), month, class)

obsbkg_counts


write_model_input(obsbkg, scientificname = species)
