
read_haedat = function(month = TRUE, 
                       tidy=TRUE,
                       species = NULL,
                       country = NULL) {
  
  #' Reads locally stored table of haedat data and provides some formatting options
  #' @param logical if TRUE adds month column
  #' @param tidy logical, if true filters other missing values like lat, lon and month
  #' @param species character defining single species to return, if NULL returns all species
  #' @param country character vector of event countries to return, if NULL returns all countries
  #' @returns tibble of haedat data
  h = suppressMessages(read_csv(file.path(ROOT_DATA_PATH, "haedat_search.csv"))) 
  
  if (month) {
    h = find_month(h)
  }
  
  if (tidy) {
    h = dplyr::filter(h, !is.na(latitude), !is.na(longitude), !is.na(month))
  }
  
  if (!is.null(species)) {
    h = dplyr::filter(h, causativeSpeciesName0 == species)
  }
  
  if (!is.null(country)) {
    h = dplyr::filter(h, countryName %in% country)
  }
  
  r = st_as_sf(h, coords = c("longitude", "latitude"), crs=4326)
  
  return(r)
}



find_month = function(x) {
  
  #' Assing a month value to events by trying multiple date columns in order
  #' @param x tibble of haedat data
  #' @returns x with month complete column
  months1 = mutate(x, month = factor(format(as.Date(eventDate), format = "%b"), levels = month.abb))
  
  months2 = filter(months1, is.na(month)) |>
    mutate(month = factor(format(as.Date(initialDate), format = "%b"), levels = month.abb)) 
  
  months3 = filter(months2, is.na(month)) |>
    mutate(month = factor(format(as.Date(finalDate), format = "%b"), levels = month.abb))
  
  months4 = filter(months3, is.na(month)) |>
    mutate(month = factor(format(as.Date(quarantineStartDate), format = "%b"), levels = month.abb))
  
  r = bind_rows(months1, months2) |>
    bind_rows(months3) |>
    bind_rows(months4) |>
    filter(!is.na(month))
  
  return(r)
}