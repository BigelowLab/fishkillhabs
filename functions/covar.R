read_covariates <- function(depth=TRUE) {
  
  #' Reads monthly covariates and returns them as a single stars object
  #' @param depth logical, if true includes depth
  #' @returns stars object containing all covariates as attributes along with lat, lon and month dimensions
  #' 

  path = andreas::copernicus_path("world", "GLOBAL_MULTIYEAR_PHY_001_030")
  
  db = andreas::read_database(path) |>
    dplyr::filter(name %in% c("temp", "mlotst", "sal"), 
                  depth %in% c("sur", "bot", "mld"), 
                  period == "month-clim")
  
  r = andreas::read_andreas(db, path) |>
    stars::st_set_dimensions("time",
                             values = month.abb,
                             names = "month")

  if (depth) {
    depth = read_depth(months = TRUE)
    
    r = c(r, depth, tolerance = 1e-06, along=NA_integer_)
  }
  
  return(r)
}


read_depth <- function(months = FALSE) {
  depth = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif")
  names(depth) = c("depth")
  
  if (months) {
    dd = sapply(month.abb, function(mon) {depth}, simplify = FALSE)
    depth = do.call(c, append(dd, list(along = list(month = month.abb)))) |>
      set_point(NA)
  }
  return(depth)
}

set_point = function(x, point = NA){
  #' Set the "point" value for a stars object
  #' 
  #' @export
  #' @param x stars object
  #' @param point logical possible values are TRUE, FALSE and NA
  #' @return the input with x and y dim point values modified
  d <- stars::st_dimensions(x)
  d[[1]]$point <- point
  d[[2]]$point <- point
  stars::st_dimensions(x) <- d
  x
}
