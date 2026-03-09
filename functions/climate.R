
find_climate_env = function(x, 
                            species, 
                            method = c("min_max", "mean_sd")[1], 
                            month=FALSE,
                            nsd=1) {
  
  if (month) {
    find_climate_env_month(x, species)
  } else {
    switch(method,
           "min_max" = {
             summarise(x,
                       min_sst = min(thetao, na.rm=TRUE),
                       max_sst = max(thetao, na.rm=TRUE),
                       min_sss = min(so, na.rm=TRUE),
                       max_sss = max(so, na.rm=TRUE),
                       min_mld = min(mlotst, na.rm=TRUE),
                       max_mld = max(mlotst, na.rm=TRUE),
                       min_sbt = min(bottomT, na.rm=TRUE),
                       max_sbt = max(bottomT, na.rm=TRUE)) |>
               mutate(species = species, .before=min_sst)
           },
           "mean_sd" = {
             vars = c("thetao", "so", "mlotst", "bottomT")
             s = sapply(vars, function(v) {
               list(vmean = mean(x[[v]], na.rm=TRUE),
                    vsd = sd(x[[v]], na.rm=TRUE))
             }, simplify=FALSE, USE.NAMES=TRUE)
             
             summarise(x,
                       min_sst = s$thetao$vmean-nsd*s$thetao$vsd,
                       max_sst = s$thetao$vmean+nsd*s$thetao$vsd,
                       min_sss = s$so$vmean-nsd*s$so$vsd,
                       max_sss = s$so$vmean+nsd*s$so$vsd,
                       min_mld = s$mlotst$vmean-nsd*s$mlotst$vsd,
                       max_mld = s$mlotst$vmean+nsd*s$mlotst$vsd,
                       min_sbt = s$bottomT$vmean-nsd*s$bottomT$vsd,
                       max_sbt = s$bottomT$vmean+nsd*s$bottomT$vsd) |>
               mutate(species = species, .before=min_sst)
           })
  }
}


find_climate_env_month <- function(x, species, method = c("min_max", "mean_sd")[1], nsd=1) {
  
  switch(method,
         "min_max" = {
           group_by(x, month) |>
             summarise(min_sst = min(sst, na.rm=TRUE),
                       max_sst = max(sst, na.rm=TRUE),
                       min_sss = min(sss, na.rm=TRUE),
                       max_sss = max(sss, na.rm=TRUE))
         },
         "mean_sd" = {
           group_by(x, month) |>
             summarise(mean_sst = mean(sst, na.rm=TRUE),
                       sd_sst = sd(sst, na.rm=TRUE),
                       min_sst = mean_sst-nsd*sd_sst,
                       max_sst = mean_sst+nsd*sd_sst,
                       mean_sss = mean(sss, na.rm=TRUE),
                       sd_sss = sd(sss, na.rm=TRUE),
                       min_sss = mean_sss-nsd*sd_sss,
                       max_sss = mean_sss+nsd*sd_sss)
         })
}

predict_climate_env = function(ce, m, month=FALSE) {
  
  #' predicts areas within climate envelope upper and lower bounds for a single month
  #' @param ce tibble containing one row describing upper and lower bounds for each variable in climate envelope
  #' @param env stars object with all covariates and the dimensions lat and lon
  
  if (month) {
    predict_climate_env_month(ce, env=m)
  } else {
    mutate(m, 
           pred = ifelse(between(thetao, ce$min_sst, ce$max_sst) & 
                           between(so, ce$min_sss, ce$max_sss) & 
                           between(mlotst, ce$min_mld, ce$max_mld) & 
                           between(bottomT, ce$min_sbt, ce$max_sbt) & 
                           depth <= 500, 1, 0))
  }
}

predict_climate_env_month = function(ce, env) {
  
  #' predicts areas within climate envelope upper and lower bounds for all months in a year
  #' @param ce tibble containing one row describing upper and lower bounds for each variable in climate envelope
  #' @param env stars object with all covariates and the dimensions lat, lon and month
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
  
  return(r)
}


plot_climate_env <- function(x) {
  pivot_longer(x, cols = c(sst, sss)) |>
    ggplot(aes(x=month, y=value)) +
    geom_boxplot() +
    facet_grid(cols = vars(name)) +
    scale_x_discrete(limits = month.abb)
}

