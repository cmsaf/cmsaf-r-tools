simplelm2 <- function(y) {
  x <- seq_along(y)
  dummy <- !is.na(y)
  x <- x[dummy]
  y <- y[dummy]
  ## number of data
  n <- length(x)
  ## centring
  y0 <- sum(y) / length(y); yc <- y - y0
  x0 <- sum(x) / length(x); xc <- x - x0
  ## fitting an intercept-free model: yc ~ xc + 0
  xty <- c(crossprod(xc, yc))
  xtx <- c(crossprod(xc))
  slope <- xty / xtx
  rc <- yc - xc * slope
  ## Pearson estimate of residual standard error
  sigma2 <- c(crossprod(rc)) / (n - 2)
  ## standard error for slope
  slope_se <- sqrt(sigma2 / xtx)
  ## confidence interval
  lo_conf <- slope - (1.959964 * slope_se)
  up_conf <- slope + (1.959964 * slope_se)
  ## return estimation summary for slope and confidence interval
  c(slope, lo_conf, up_conf)
}
