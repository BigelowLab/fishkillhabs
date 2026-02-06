## match species occurrences and background points with covariates

source("setup.R")

species = "Noctiluca scintillans"

model_input = read_model_input(scientificname = species)

sst = stars::read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/mon_sst.tif", proxy=FALSE) |>
  stars::st_set_dimensions("band",
                           values = month.abb,
                           names = "month")
names(sst) = c("sst")
sss = stars::read_stars("/mnt/ecocast/projectdata/fishkillhabs/climatology/mon_sss.tif", proxy=FALSE) |>
  stars::st_set_dimensions("band",
                           values = month.abb,
                           names = "month")
names(sss) = c("sss")

env <- c(sst, sss, tolerance = 1e-06, along=NA_integer_)

pairs(env)

keep = filter_collinear(env, method = "cor_caret", cutoff = 0.65)
keep = c("depth", "month", keep)

depth = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif")
names(depth) = c("depth")
dd = sapply(month.abb, function(mon) {depth}, simplify = FALSE)
depth = do.call(c, append(dd, list(along = list(month = month.abb))))

present = c(env, depth, tolerance = 1e-06, along=NA_integer_)

present

variables = extract_covars(present, model_input, form = "wide")

variables = variables |>
  mutate(class = model_input$class) |>    # the $ extracts a column 
  select(-.id)        

plot_pres_vs_bg(variables |> select(-month), "class")

cfg = list(
  version = "v1",
  scientificname = species,
  background = "average of observations per month",
  keep_vars =  keep)

ok = make_path(data_path("models")) # make a directory for models
write_configuration(cfg)

write_model_input(variables, scientificname = species, version = "v1")

