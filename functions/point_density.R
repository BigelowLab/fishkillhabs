rasterize_point_density <- function(x, y, 
                                    name = "count",
                                    dilate = 0,
                                    dilate_value = 1,
                                    mask = y){
  
  #' Given a set of points and a target raster geometry, compute the density (count)
  #' of points in each cell.
  #' 
  #' @param x sf POINT object
  #' @param y stars object that defines the geometry of the output
  #' @param name chr, the name of the output variable
  #' @param dilate num, the size of the square structuring element used for dilating the
  #'   output counts (padding with zeroes).  Set to 0 to skip. 
  #' @param dilate_value num, if dilation occurs, this is the value assigned to the padded cells
  #' @param mask `stars` object that defines masked areas where
  #'   dilation does not occur with the value NA.  It must have the same spatial 
  #'   geometry as the input \code{y}.
  #' @return stars array with point densities
  
  if (inherits(y, "stars")){
    # if y has scrambled coords reorganize as x,y,z
    # then be sure we have just one variable and one band (ie simple 2d geometry)
    y = stars:::st_upfront(y)
    y = y[1]
    d = dim(y)
    if(length(d) > 2){
      y = dplyr::slice(y, names(d)[3], 1)  
    }
    
    # cast as point data
    v = sf::st_as_sf(y)
    # trim the points to just a "name" attribute
    x = dplyr::mutate(x, {{ name }} := 1) |>
      dplyr::select(dplyr::all_of(name))
    # aggregate by counting the instances of x in each element of y 
    # (or the polygonized-by-cell "v" version) and then cast back to 
    # the template raster
    r = aggregate(x, v, FUN = length) |>
      stars::st_rasterize(template = y, align = TRUE)
  } 
  
  
  if (dilate[1] > 0){
    
    
    raw = r[[1]][]
    
    raw = (!is.na(raw)) * 1.0
    
    m = raw |> 
      imager::as.cimg() |>
      imager::dilate_square(dilate[1]) |>
      as.matrix()
    
    if (!is.null(mask)){
      
      if (inherits(mask[[1]], "factor")) mask[[1]] <- as.numeric(mask[[1]])
      ix = is.na(mask[[1]][]) 
      
      m[ix] <- NA
      # in case the edge cases produce unwanted zeroes (like over land)
      m[m <= 0] <- NA
    }
    
    # here we transfer the padded zeroes to the count data
    
    s = r[[1]]
    ix <- (!is.na(s)) | (is.na(m))
    s[!ix] <- dilate_value
    r[[1]] <- s
    
  }
  
  r
}
