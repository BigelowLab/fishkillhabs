
source("setup.R")

species = "Karenia brevis"
model_v = "v3"
model_type = "rf"
model_input = read_model_input(scientificname = species, 
                               version = model_v,
                               log_me = c("depth"))

summary(model_input)

depth = seq(0.6, 2.7, by=0.2)
thetao = seq(0, 35, by=2)
so = seq(0,41, by=3)
mlotst=seq(0, 477, by=30)
bottomT = seq(-2,34, by=2)

floor(min(model_input$depth))

lut = expand.grid(depth = depth, 
                  thetao = thetao,
                  so=so, 
                  mlotst=mlotst, 
                  bottomT=bottomT)


file = gsub(" ", "-", sprintf("%s-%s-model_fits", species, model_v))

model_fit = read_model_fit(filename = file) |>
  filter(wflow_id == c("default_rf"))

model = model_fit$.workflow

p=predict(model, lut, type="prob")

lut_pred = cbind(lut, p[1][[1]]) |> tibble()

filename = sprintf("%s-%s-%s-lut.csv.gz",
                   gsub(" ", "_", scientificname, fixed = TRUE),
                   model_v[1],
                   model_type)
path = file.path(data_path("predictions"), filename)

write_csv(lut_pred, path)
