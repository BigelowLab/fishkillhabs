
read_mask = function(max_depth = 500) {
  #' Reads mask to use for building model input
  #' @param max_depth integer defining maximum depth to use for mask
  #' @return stars object for mask where cells that meet depth criteria are 1 and others are NA
  mask = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif") |>
    set_names("depth") |>
    mutate(depth = ifelse(between(depth, 0, max_depth), 1, NA))
}