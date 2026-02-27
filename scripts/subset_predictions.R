## Subset predictions

source("setup.R")

p = read_prediction(scientificname = "Karenia brevis",
                version = "v2",
                year = "CURRENT",
                scenario = "CURRENT",
                path = data_path("predictions"))


x = slice(p, month, "Jan")

gg = ggplot2::ggplot() +
  stars::geom_stars(data = x[1]) + 
  ggplot2::scale_fill_viridis_c(option = "magma", 
                                limits = c(0,1), 
                                na.value = "grey50") +
  coord_sf()

gg

bb = st_bbox(c(xmin = -71, ymin = 43, xmax = -67, ymax = 47.5), crs = st_crs(x))

st_crop(x, bb)

pa = threshold_prediction(p)


plot_prediction(pa['default_btree'])



x = slice(present_conditions, month, "Jun") |>
  mutate(depth = log10(depth))

model_fits = read_model_fit(filename = "Noctiluca-scintillans-v2-model_fits") |>
  filter(wflow_id == "default_btree")

nowcast = predict_stars(model_fits, x)
