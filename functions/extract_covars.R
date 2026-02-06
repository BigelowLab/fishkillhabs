extract_covars = function(x, y, form = "long") {
  
  #' Extract point data from a covariate stars object
  #' 
  #' @param x stars object (with or without month dimension)
  #' @param y sf point data, if this has a "month" attribute then we extract by month
  #'   as long as x has a month dimension
  #' @param form chr one of either "long" (the default) or "wide" to control 
  #'   output column layout. 
  #' @param ... other arguments passed to `stars::st_extract()`
  #' @return table of variable data for each input point
  
  d = dim(x)
  if ((length(d) >= 3) && ("month" %in% names(y))) {
    return(extract_covars_by_month(x, y, form = form))
  }
  
  n = nrow(y)
  N = floor(log10(n)+ 1)
  fmt = paste0("p%0.", N, "i")
  pnames = sprintf(fmt, seq_len(n))
  xy = sf::st_coordinates(y)
  if (length(dim(x)) == 2){
    m = stars::st_extract(x, xy)
    rownames(m) <- pnames
    p = dplyr::as_tibble(m, rownames = ".id") |>
      tidyr::pivot_longer(-dplyr::all_of(".id"),
                          values_to = "value",
                          names_to = "name")
    
  } else {
    nms = stars::st_get_dimension_values(x, 3)
    p = lapply(names(x),
               function(nm){
                 this_att = x[nm]
                 m = if (inherits(this_att[[1]], 'factor')){
                   d = dim(this_att[[1]])
                   month_lut = seq_along(month.abb)
                   names(month_lut) = month.abb
                   m = month_lut[as.character(this_att[[1]])]
                   this_att[[1]] <- m
                   stars::st_extract(this_att, xy)
                 } else {
                   stars::st_extract(this_att, xy)
                 }
                 colnames(m) <- nms
                 rownames(m) <- pnames
                 m |>
                   dplyr::as_tibble(rownames = ".id") |>
                   dplyr::mutate(name = nm, .after = 1)
               }) |>
      dplyr::bind_rows() |>
      tidyr::pivot_longer(-dplyr::all_of(c(".id", "name")),
                          names_to = "month",
                          values_to = "value")
  }
  if (tolower(form[1]) == "wide"){
    p = tidyr::pivot_wider(p,
                           names_from = "name",
                           values_from = "value")
  }
  
  p
}


extract_covars_by_month = function(x, y, form = "long") {
  
  #' Extract point data from a covariates stars object
  #' 
  #' @param x stars object (with or without month dimension)
  #' @param y sf point data, if this has a "month" attribute then we extract by month
  #'   as long as x has a month dimension
  #' @param form chr one of either "long" (the default) or "wide" to control 
  #'   output column layout. 
  #' @param ... other arguments passed to `stars::st_extract()`
  #' @return table of variable data for each input point
  
  d = dim(x)
  if (length(d) != 3) stop("x must have 3rd dimension")
  if (!"month" %in% names(y)) stop("y must have 'month' as an attribute")
  
  n = nrow(y)
  N = floor(log10(n)+ 1)
  fmt = paste0("p%0.", N, "i")
  geomcol = attributes(y)$sf_column
  
  z = y |>
    dplyr::mutate(.id = sprintf(fmt, seq_len(n)), .before = 1) |>
    dplyr::group_by(month) |>
    dplyr::group_map(
      function(rows, key){
        tempx = dplyr::slice(x, "month", month.abb[key$month])
        r = stars::st_extract(tempx, sf::st_coordinates(rows)) |>
          dplyr::as_tibble()
        dplyr::bind_cols(rows, r) |>
          dplyr::relocate(dplyr::all_of(geomcol), 
                          .after = dplyr::last_col())
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.id)
  
  if (tolower(form[1]) == "long"){
    z = z |>
      tidyr::pivot_longer(!dplyr::all_of(c("month", "class", ".id", "Month", geomcol)),
                          names_to = "name",
                          values_to = "value") |>
      dplyr::relocate(dplyr::all_of(geomcol), 
                      .after = dplyr::last_col())
  }
  z
}