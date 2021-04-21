calc_timx_result <- function(op, infile, dimension_data, var_name, na.rm, p) {
  limit <- 2601 * 2601 * 31  # This value can be adjusted to avoid RAM overflow

  dimensionality <- as.double(length(dimension_data$x)) *
    as.double(length(dimension_data$y)) * as.double(length(dimension_data$t))

  if (dimensionality < limit) {
    # Result can directly be calculated.
    nc_in <- nc_open(infile)
    dum_dat <- ncvar_get(nc_in, var_name, collapse_degen = FALSE)
    nc_close(nc_in)
    result <- switch(
      op,
      max = {do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))},
      min = {do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))},
      mean = {rowMeans(dum_dat, dims = 2, na.rm = na.rm)},
      sum = {rowSums(dum_dat, dims = 2, na.rm = na.rm)},
      sd = {apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)},
      pctl = {apply(dum_dat, c(1, 2), stats::quantile, probs = p, names = FALSE, na.rm = TRUE)}
    )
  } else {
    nr_of_possible_x_dims <- round(
      (limit / length(dimension_data$x)) / length(dimension_data$t)
    )
    dimsteps_start <- seq(1, length(dimension_data$y), nr_of_possible_x_dims)
    dimsteps_count <- rep(nr_of_possible_x_dims, length(dimsteps_start))
    cor <- nr_of_possible_x_dims * length(dimsteps_start) - length(dimension_data$y)
    dimsteps_count[length(dimsteps_start)] <- dimsteps_count[length(dimsteps_start)] - cor

    result <- array(NA, dim = c(length(dimension_data$x),
                                length(dimension_data$y),
                                1))

    for (i in seq_along(dimsteps_start)) {
      nc_in <- nc_open(infile)
      dum_dat <- ncvar_get(nc_in,
                           var_name,
                           start = c(1, dimsteps_start[i], 1),
                           count = c(-1, dimsteps_count[i], -1),
                           collapse_degen = FALSE)
      nc_close(nc_in)
      result[, seq(dimsteps_start[i], dimsteps_start[i] + dimsteps_count[i] - 1, 1), 1] <-
        switch(
          op,
          max = {do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))},
          min = {do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))},
          mean = {rowMeans(dum_dat, dims = 2, na.rm = na.rm)},
          sum = {rowSums(dum_dat, dims = 2, na.rm = na.rm)},
          sd = {apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)},
          pctl = {apply(dum_dat, c(1, 2), stats::quantile, probs = p, names = FALSE, na.rm = TRUE)}
        )
    }
  }
  return(result)
}
