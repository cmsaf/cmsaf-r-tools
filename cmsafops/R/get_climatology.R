get_climatology <- function(infile, file_data, nc = NULL) {
  limit <- 2601 * 2601 * 31	  # This value can be ajusted to avoid RAM overflow

  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)
  if ((length(file_data$dimension_data$x) * length(file_data$dimension_data$y) * length(file_data$dimension_data$t)) < limit) {
    dum_dat <- ncvar_get(nc_in, file_data$variable$name, collapse_degen = FALSE)
    clim <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
  } else {

    dum1 <- round((limit / length(file_data$dimension_data$x)) / length(file_data$dimension_data$y))
    dum2 <- seq(1, length(file_data$dimension_data$t), dum1)
    dum3 <- array(dum1, dim = c(length(dum2)))
    cor <- dum1 * length(dum2) - length(file_data$dimension_data$t)
    dum3[length(dum2)] <- dum3[length(dum2)] - cor

    sum_data <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), length(dum2)))
    num <- 0
    for (i in seq_along(dum2)) {
      dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, dum2[i]), count = c(-1, -1, dum3[i]), collapse_degen = FALSE)
      sum_data[, , i] <- rowSums(dum_dat, dims = 2, na.rm = TRUE)
      nan <- rowSums(!is.na(dum_dat), dims = 2)
      num <- num + nan
    }
    clim <- rowSums(sum_data, dims = 2, na.rm = TRUE) / num
  }
  if (is.null(nc)) nc_close(nc_in)
  return(clim)
}
