ranking <- function(out_dir, 
                    var, 
                    country_code, 
                    climate_year_start, 
                    climate_year_end,
                    doy,
                    years = NULL,
                    values = NULL) {
  # --- Fast path: use provided year/value vectors from plot ---
  if (!is.null(years) && !is.null(values)) {
    stopifnot(length(years) == length(values))
    rk <- data.frame(Year = years, Value = as.numeric(values))
    return(rk[order(rk$Value), ])
  }
  
  # --- Default path: read values from NetCDF files ---
  out_dir <- file.path(out_dir, "mc_temp")
  years_vec  <- integer(0)
  values_vec <- numeric(0)
  
  for (i in climate_year_start:climate_year_end) {
    filename <- paste(var, "_", i, "_", country_code, "_fldmean.nc", sep = "")
    file <- file.path(out_dir, filename)
    
    if (file.exists(file)) {
      data <- cmsafops::read_ncvar(var, file)
      data <- as.numeric(unlist(data))
      years_vec  <- c(years_vec, i)
      values_vec <- c(values_vec, data[doy])
    }
  }
  
  rk <- data.frame(Year = years_vec, Value = values_vec)
  rk[order(rk$Value), ]
}
