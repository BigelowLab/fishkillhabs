

standard_obis_fields = function(){
  #' Retrieve a vector of standard OBIS fields
  #' 
  #' @return character vector of names
  fields = c("id",
             "basisOfRecord",
             "eventDate",
             "eventTime",
             "individualCount",
             "decimalLongitude",
             "decimalLatitude",
             "depth",
             "bathymetry",
             "shoredistance",
             "sst",
             "sss")
  return(fields)
}

tidy_obis = function(x = fetch_obis(),
                     fields = standard_obis_fields(),
                     crs = 4326){
  
  #' Tidy raw OBIS results and return as an sf object
  #' 
  #' @param x data frame or tibble of raw obis results
  #' @param fields chr, the preferred fields and order of fields
  #' @param crs the CRS of the data
  #' @return sf object
  
  x = dplyr::select(x, dplyr::any_of(fields))
  cnames = colnames(x)
  if ("eventDate" %in% cnames) {
    dates = substring(x$eventDate, 1,10) |> as.Date()
    x = dplyr::mutate(x, 
                      eventDate = dates,
                      year = suppressWarnings(as.numeric(format(eventDate, "%Y"))),
                      month = suppressWarnings(format(eventDate, "%b")) |>
                        factor(levels = month.abb),
                      .after = eventDate)
  }
  if ("individualCount" %in% cnames) {
    x = dplyr::mutate(x,individualCount = suppressWarnings(as.numeric(individualCount)))
  }
  
  sf::st_as_sf(x, 
               coords = c("decimalLongitude", "decimalLatitude"),
               crs = crs) 
}



fetch_obis = function(scientificname = "Margalefidinium polykrikoides",
                      bb = NULL,
                      fields = standard_obis_fields(),
                      tidy = TRUE,
                      output_path = data_path("obis") |> make_path(),
                      crs = 4326){
  
  #' Fetch one or more species of obis data
  #' 
  #' @param scientificname one or more "genus" or "genus species" scientific name(s)
  #' @param bb either NULL (to skip) or something from which a bounding box may be
  #'   extracted.  Used to filter the request to a smaller-than-global scope.
  #' @param fields chr one or more fields to retrieve
  #' @param tidy logical, if TRUE tidy up the downloaded data
  #' @param output_path chr, the destination path where the data are stored using 
  #'  Files are automatically named "genus_species.gpkg" or "genus.gpkg" as
  #'  appropriate for each request.
  #' @param crs the CRS for the data
  #' @return spatial table of observations
  #'  
  geometry = if(is.null(bb)){
    NULL
  } else {
    if (inherits(bb, "bbox")) bb = sf::st_as_sfc(bb)
    if (!inherits(bb, "sfc")) bb = sf::st_as_sfc(bb)
    sf::st_as_text(bb)
  }
  
  xx = lapply(scientificname,
              function(species){
                x = robis::occurrence(scientificname = species,
                                      geometry = geometry)
                if (tidy) x = tidy_obis(x, fields = fields, crs = crs)
                sf::write_sf(x, 
                             file.path(output_path, 
                                       sprintf("%s.gpkg", gsub(" ", "_", species, fixed = TRUE))))
              }) |>
    dplyr::bind_rows()
  return(xx)
}



read_obis = function(scientificname = "Karenia mikimotoi",
                     path = data_path("obis"),
                     maxdepth = 500,
                     maxdistance = 20000){
  
  #' Read one or more obis data files
  #' 
  #' @param scientificname one or more scientific names to read
  #' @param path chr the data path
  #' @return sf table
  
  xx = lapply(scientificname,
              function(species){
                filename = file.path(path, 
                                     sprintf("%s.gpkg", gsub(" ", "_", species, fixed = TRUE)))
                sf::read_sf(filename)
              }) |>
    dplyr::bind_rows() |>
    dplyr::filter(between(bathymetry, 0, !!maxdepth), 
                  between(shoredistance, 0, !!maxdistance),
                  !is.na(eventDate)) |>
    dplyr::mutate(month = as.numeric(format(eventDate, format="%m")))
  return(xx)
}



browse_obis = function(id = "00040fa1-7acd-4731-bf1e-6dc16e30c7d4",
                       base_url = "https://api.obis.org/v3/occurrence"){
  
  #' Browse the record for one unique id
  #' 
  #' @param id chr, a unique ID or an OBIS record (one row) If multiples are 
  #'   provided all but the first are ignored.
  #' @param root_url chr, the root url
  #' @return NULL invisibly
  #' @examples
  #' \dontrun{
  #'   obs = read_obis(scientificname = "Mola mola")
  #'   one = obis |> slice(202)
  #'   browse_obis(one)
  #' }
  
  if (inherits(id, "data.frame")) id = dplyr::pull(id, id)
  httr::BROWSE(file.path(base_url, id[1]))
}

make_path = function(path){
  
  #' Make a path if it doesn't exist
  #' 
  #' @param path chr the path to create
  #' @return the path
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}

data_path = function(...,  root = ROOT_DATA_PATH){
  
  #' Get the data path
  #' 
  #' @param what chr path and file segments to append to the root path using the
  #'   system file separator
  #' @param root chr, the root personal data directory
  #' @return some directory in the personal data path
  
  file.path(root, ...)
}

obis_path = function(root = ROOT_DATA_PATH){
  
  #' Retrieve the path to the obis data
  #' 
  #' @param root chr, the root personal data directory
  #' @return the path to the brickman config file
  
  path = file.path(root, "obis")
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}

