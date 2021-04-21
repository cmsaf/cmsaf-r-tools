calc_field <- function(infile, file_data, op, weights = NULL) {
  nc_in <- nc_open(infile)
  result <- array(NA, dim = c(length(file_data$dimension_data$t)))

  for (i in seq_along(file_data$dimension_data$t)) {
    dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, i),
                         count = c(-1, -1, 1))
    if (sum(!is.na(dum_dat)) > 0) {
      switch(op,
             result[i] <- max(dum_dat, na.rm = TRUE),
             result[i] <- min(dum_dat, na.rm = TRUE),
             result[i] <- mean(dum_dat, na.rm = TRUE),
             result[i] <- stats::weighted.mean(dum_dat, weights, na.rm = TRUE),
             result[i] <- max(dum_dat, na.rm = TRUE) - min(dum_dat, na.rm = TRUE),
             result[i] <- stats::sd(dum_dat, na.rm = TRUE),
             result[i] <- sum(dum_dat, na.rm = TRUE)
      )
    } else {
      result[i] <- NA
    }
  }

  nc_close(nc_in)

  if (length(file_data$dimension_data$t) == 1) {
    if (op == 4) {
      dummy <- array(NA, dim = c(dim(result)[1], dim(result)[2], 1))
    } else {
      dummy <- array(NA, dim = c(1, 1, 1))
    }
    dummy[1, 1, 1] <- result
    result <- dummy
  }

  return(result)
}
