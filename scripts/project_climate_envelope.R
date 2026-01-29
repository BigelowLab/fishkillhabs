# Projects temp and salinity climate envelope onto monthly climatology for one species

source("setup.R")

species = "Margalefidinium polykrikoides"

occ <- read_obis(species)

ce = find_climate_env(st_drop_geometry(occ), method="mean_sd", nsd=1) |>

bathy = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif")


for (m in 1:12) {
  
  a = read_stars(paste("/mnt/ecocast/projectdata/fishkillhabs/climatology/thetao_", m, ".tif", sep=""))
  b = read_stars(paste("/mnt/ecocast/projectdata/fishkillhabs/climatology/so_", m, ".tif", sep=""))
  
  z <- c(a,b, bathy, tolerance = 1e-06)
  names(z) = c("sst", "sss", "depth")
  
  clim = filter(ce, month == m)
  
  preds = mutate(z, pred = ifelse(between(sst, clim$min_sst, clim$max_sst) & between(sss, clim$min_sss, clim$max_sss) & depth <= 500, 1, 0))
  
  p = ggplot() + 
    geom_stars(data=preds, aes(fill=pred)) +
    theme_bw() +
    theme(legend.position="none", axis.title = element_blank()) +
    ggtitle(paste(species, "mean +/- 1 SD: month", m, sep=" ")) #+
    #coord_sf()
  
  outfile = paste(species, m, ".png", sep="")
  outfile = gsub(" ", "_", outfile)
  ggsave(file.path("/mnt/ecocast/projectdata/fishkillhabs/predictions", outfile), p, width=8, height=5, units="in")
}



## a = read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/thetao_1.tif")
## b = read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/so_1.tif")
## bathy = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif")
## 
## #z <- c(a,b)
## #names(z) = c("sst", "sss")
## 
## z = c(a,b, bathy, tolerance = 1e-06)
## names(z) = c("sst", "sss", "depth")
## 
## preds = mutate(z, pred = ifelse(between(sst, 8.07, 28.5) & between(sss, 7.48, 37.2) & depth <= 500, 1, 0))
## 
## plot(preds["pred"])
## 
## ggplot() + 
##  geom_stars(data=preds, aes(fill=pred)) +
##  theme_bw()


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