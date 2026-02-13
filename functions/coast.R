
read_coastline = function(scale = 50) {
  rnaturalearth::ne_coastline(scale = 50, returnclass = "sf") |> 
    st_geometry()
}
