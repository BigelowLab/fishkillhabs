

find_climate_env <- function(x, method = c(min_max, mean_sd)[1]) {
  
  switch(method,
         min_max = {
           group_by(x, month) |>
             summarise(min_sst = min(sst, na.rm=TRUE),
                       max_sst = max(sst, na.rm=TRUE),
                       min_sss = min(sss, na.rm=TRUE),
                       max_sss = max(sss, na.rm=TRUE))
         },
         mean_sd = {
           group_by(x, month) |>
             summarise(mean_sst = mean(sst, na.rm=TRUE),
                       sd_sst = sd(sst, na.rm=TRUE),
                       min_sst = mean_sst-sd_sst,
                       max_sst = mean_sst+sd_sst,
                       mean_sss = mean(sss, na.rm=TRUE),
                       sd_sss = sd(sss, na.rm=TRUE),
                       min_sss = mean_sss-sd_sss,
                       max_sss = mean_sss+sd_sss)
         })
}


plot_climate_env <- function(x) {
  pivot_longer(x, cols = c(sst, sss)) |>
    ggplot(aes(x=month, y=value)) +
    geom_boxplot() +
    facet_grid(cols = vars(name)) +
    scale_x_discrete(limits = month.abb)
}