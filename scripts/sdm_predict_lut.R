
source("setup.R")

s = c("Alexandrium catenella", 
      "Chattonella marina",
      "Chrysochromulina leadbeateri",
      "Heterosigma akashiwo",
      "Karenia brevis",
      "Karenia mikimotoi",
      "Margalefidinium polykrikoides",
      "Noctiluca scintillans",
      "Prymnesium polylepis",
      "Pseudochattonella verruculosa")

for (species in s) {
  cat(species, "\n")
  model_v = "v3"
  model_type = "rf"
  model_input = read_model_input(scientificname = species, 
                                 version = model_v,
                                 log_me = c("depth"))
  
  summary(model_input)
  
  depth = seq(min(model_input$depth), max(model_input$depth), by=0.2)
  thetao = seq(min(model_input$thetao), max(model_input$thetao), by=2)
  so = seq(min(model_input$so), max(model_input$so), by=3)
  mlotst=seq(min(model_input$mlotst), max(model_input$mlotst), by=30)
  bottomT = seq(min(model_input$bottomT),max(model_input$bottomT), by=2)
  
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
  
  lut_pred = cbind(lut, p[1][[1]]) |> 
    tibble() |> 
    select(-.pred_background) |>
    rename(pred = .pred_presence)
  
  filename = sprintf("%s-%s-%s-lut.csv.gz",
                     gsub(" ", "_", species, fixed = TRUE),
                     model_v[1],
                     model_type)
  path = file.path(data_path("predictions"), "sdm_lut", filename)
  
  write_csv(lut_pred, path)
}


## species = "Karenia brevis"
## model_v = "v3"
## model_type = "rf"
## model_input = read_model_input(scientificname = species, 
##                                version = model_v,
##                                log_me = c("depth"))
## 
## summary(model_input)
## 
## depth = seq(min(model_input$depth), max(model_input$depth), by=0.2)
## thetao = seq(min(model_input$thetao), max(model_input$thetao), by=2)
## so = seq(min(model_input$so), max(model_input$so), by=3)
## mlotst=seq(min(model_input$mlotst), max(model_input$mlotst), by=30)
## bottomT = seq(min(model_input$bottomT),max(model_input$bottomT), by=2)
## 
## lut = expand.grid(depth = depth, 
##                   thetao = thetao,
##                   so=so, 
##                   mlotst=mlotst, 
##                   bottomT=bottomT)
## 
## 
## file = gsub(" ", "-", sprintf("%s-%s-model_fits", species, model_v))
## 
## model_fit = read_model_fit(filename = file) |>
##   filter(wflow_id == c("default_rf"))
## 
## model = model_fit$.workflow
## 
## p=predict(model, lut, type="prob")
## 
## lut_pred = cbind(lut, p[1][[1]]) |> 
##   tibble() |> 
##   select(-.pred_background) |>
##   rename(pred = .pred_presence)
## 
## filename = sprintf("%s-%s-%s-lut.csv.gz",
##                    gsub(" ", "_", species, fixed = TRUE),
##                    model_v[1],
##                    model_type)
## path = file.path(data_path("predictions"), "sdm_lut", filename)
## 
## write_csv(lut_pred, path)

