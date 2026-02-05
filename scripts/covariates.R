## match species occurrences and background points with covariates

source("setup.R")

#species = "Noctiluca scintillans"
#obs <- read_obis(species) |>
#  mutate(month = factor(format(eventDate, format="%b"), levels = month.abb)) |>
#  rename(obis_sst = sst,
#         obis_sss = sss,
#         obis_depth = depth)

model_input = sf::read_sf("/mnt/ecocast/projectdata/fishkillhabs/Noctiluca_scintillans-model_input.gpkg") |>
  dplyr::mutate(month = factor(month, levels = month.abb),
                class = factor(class, levels = c("presence", "background")))

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

z = c(env, depth, tolerance = 1e-06, along=NA_integer_)

z


d = dim(z)
(length(d) >= 3) && ("month" %in% names(model_input))

n = nrow(model_input)
N = floor(log10(n)+ 1)
fmt = paste0("p%0.", N, "i")
geomcol = attributes(model_input)$sf_column

zz = model_input |>
  dplyr::mutate(.id = sprintf(fmt, seq_len(n)), .before = 1) |>
  dplyr::group_by(month) |>
  dplyr::group_map(
    function(rows, key){
      tempx = dplyr::slice(z, "month", month.abb[key$month])
      r = stars::st_extract(tempx, sf::st_coordinates(rows)) |>
        dplyr::as_tibble()
      dplyr::bind_cols(rows, r) |>
        dplyr::relocate(dplyr::all_of(geomcol), 
                        .after = dplyr::last_col())
    }, .keep = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::arrange(.id)

model_input = zz |>
  mutate(class = model_input$class) |>    # the $ extracts a column 
  select(-.id)        

cfg = list(
  version = "v1",
  scientificname = "Mola mola",
  background = "average of observations per month",
  keep_vars =  keep)
