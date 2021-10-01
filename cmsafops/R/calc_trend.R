calc_trend <- function(infile, file_data, option, nc = NULL) {
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)

  length.dimension.x <- length(file_data$dimension_data$x)
  length.dimension.y <- length(file_data$dimension_data$y)
  length.dimension.t <- length(file_data$dimension_data$t)
  
  target <- array(NA, dim = c(length.dimension.x,
                              length.dimension.y, 1))
  target2 <- array(NA, dim = c(length.dimension.x,
                               length.dimension.y, 1))
  target_p <- array(NA, dim = c(length.dimension.x,
                                length.dimension.y, 1))
  
  if (option == 2) {

    x <- seq_along(file_data$dimension_data$t)

    for (i in seq_along(file_data$dimension_data$x)) {
      prog <- round((100 / length.dimension.x) * i)
      for (j in seq_along(file_data$dimension_data$y)) {
        dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(i, j, 1),
                             count = c(1, 1, -1))
        ifelse(length.dimension.t - (sum(is.na(dum_dat))) >= 2,
          {
            dummy <- which(is.finite(dum_dat))
            fit <- simplelm(x[dummy], dum_dat[dummy])
            val <- fit[1] * length.dimension.t
            val2 <- fit[1]
            sig <- 0
            ifelse(!is.na(fit[2]) & !is.na(fit[3]),
              {
                if (fit[2] * fit[3] < 0) (sig <- 0)
                if (fit[2] < 0 & fit[3] < 0) (sig <- -1)
                if (fit[2] > 0 & fit[3] > 0) (sig <- 1)
              },
              {
                sig <- NA
              })
          },
          {
            val <- NA
            val2 <- NA
            sig <- NA
          })
        
        target[i, j, 1] <- val
        target2[i, j, 1] <- val2
        target_p[i, j, 1] <- sig
      }
    }
  } else {
    dum_dat <- ncvar_get(nc_in, file_data$variable$name)
    target  <- apply(dum_dat, c(1, 2), simplelm2)
    target_p[which((target[2, , ] * target[3, , ]) < 0)] <- 0
    target_p[which(target[2, , ] < 0 & target[3, , ] < 0)] <- -1
    target_p[which(target[2, , ] > 0 & target[3, , ] > 0)] <- 1

    target2  <- target[1, , ]
    target   <- target2 * length.dimension.t
  }

  target[is.na(target)] <- file_data$variable$attributes$missing_value
  target2[is.na(target2)] <- file_data$variable$attributes$missing_value
  target_p[is.na(target_p)] <- file_data$variable$attributes$missing_value

  if (is.null(nc)) nc_close(nc_in)

  return(list(target = target, target_p = target_p, target2 = target2))
}
