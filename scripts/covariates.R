## match species occurrences and background points with covariates

source("setup.R")

species = "Alexandrium catenella"
model_v = "v2"

model_input = read_model_input(scientificname = species)

#env = read_covariates(depth = FALSE)

#pairs(env)
#
#keep = filter_collinear(env, method = "cor_caret", cutoff = 0.65)
#keep = c("depth", keep)

keep = c("depth","thetao","so","mlotst","bottomT")

present = read_covariates(depth = TRUE)

present

variables = extract_covars(present, model_input, form = "wide")

variables = variables |>
  mutate(class = model_input$class) |> 
  select(-.id) |>
  drop_na()

plot_pres_vs_bg(variables |> select(-month), "class")

cfg = list(
  version = model_v,
  scientificname = species,
  background = "average of observations per month",
  keep_vars =  keep)

ok = make_path(data_path("models"))
write_configuration(cfg)

write_model_input(variables, scientificname = species, version = model_v)

