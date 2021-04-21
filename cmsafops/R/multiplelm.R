multiplelm <- function(x, y) {
  suppressWarnings({
    time_count <- c(1:length(x))
    fm <- stats::lm(x~time_count+y)   # linear regression with R func
  
    slope1 <- fm$coef[[2]]
    slope2 <- fm$coef[[3]]
  
    conf <- stats::confint(fm)    # calc confidence interval with R func confint()
    lo_conf1 <- conf[[2]]  # lower value of the 95% confidence interval
    up_conf1 <- conf[[5]]   # upper value of the 95% confidence interval
    lo_conf2 <- conf[[3]]  # lower value of the 95% confidence interval
    up_conf2 <- conf[[6]]   # upper value of the 95% confidence interval
  })
  
  c(slope1, slope2, lo_conf1, up_conf1, lo_conf2, up_conf2)
}
