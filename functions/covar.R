
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