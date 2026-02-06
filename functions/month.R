
month_as_number = function(x) {
  lut = seq_along(month.abb) |>
    as.numeric() |>
    rlang::set_names(month.abb)  
  
  lut[x]
}