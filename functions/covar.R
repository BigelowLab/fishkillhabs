
read_covariates <- function(depth=TRUE) {
  
  #' Reads monthly covariates and returns them as a single stars object
  #' @param depth logical, if true includes depth
  #' @returns stars object containing all covariates as attributes along with lat, lon and month dimensions
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
  
  r = c(sst, sss, tolerance = 1e-06, along=NA_integer_)
  
  if (depth) {
    depth = read_stars("/mnt/ecocast/projectdata/fishkillhabs/bathy.tif")
    names(depth) = c("depth")
    dd = sapply(month.abb, function(mon) {depth}, simplify = FALSE)
    depth = do.call(c, append(dd, list(along = list(month = month.abb))))
    
    r = c(r, depth, tolerance = 1e-06, along=NA_integer_)
  }
  
  return(r)
}