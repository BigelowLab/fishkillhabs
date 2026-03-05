# Projects temp and salinity climate envelope onto monthly climatology for one species

source("setup.R")

ce = read_csv(file.path(ROOT_DATA_PATH, "climate_envelope", "mean2sd_climate_envelope.csv")) |>
  filter(species == "Karenia mikimotoi")

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
bb = get_bb("nwa", "sf")
bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -88, ymin = 24, xmax = -78, ymax = 32)), crs = 4326))

bb = sf::st_sf(name = "bounding box",
               geom = sf::st_sfc(bb_as_POLYGON(c(xmin = -13, ymin = 50, xmax = 30, ymax = 75)), crs = 4326))

sf_use_s2(FALSE)
z = st_crop(r, bb)


ggplot() + 
  geom_stars(data=z, aes(fill=pred)) +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

# project all months in facet plot

preds_l = lapply(seq(1, 12),
                 function(mon) {
                   a = read_stars(paste("/mnt/ecocast/projectdata/fishkillhabs/climatology/thetao_", mon, ".tif", sep=""))
                   b = read_stars(paste("/mnt/ecocast/projectdata/fishkillhabs/climatology/so_", mon, ".tif", sep=""))
                   
                   z <- c(a,b, bathy, tolerance = 1e-06)
                   names(z) = c("sst", "sss", "depth")
                   
                   clim = filter(ce, month == mon)
                   
                   preds = mutate(z, pred = ifelse(between(sst, clim$min_sst, clim$max_sst) & between(sss, clim$min_sss, clim$max_sss) & depth <= 500, 1, 0))
                   
                   preds["pred"]
                 })

names(preds_l) = month.abb
r = do.call("c", preds_l)                                                                                                        
r = st_redimension(r)                                                                                                            
names(r) = "pred"

p = ggplot() + 
  geom_stars(data=r, aes(fill=pred)) +
  theme_bw() +
  theme(legend.position="none", axis.title = element_blank()) +
  facet_wrap(vars(new_dim)) +
  coord_sf()

ggsave(file.path("/mnt/ecocast/projectdata/fishkillhabs/predictions", "km_allmonths_minmax_csf.png"), p, width = 9.5, height = 8, units="in")