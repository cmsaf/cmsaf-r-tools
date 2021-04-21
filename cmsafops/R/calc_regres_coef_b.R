# calculate coefficient b of linear regression model
calc_regres_coef_b <- function(y)
{
  time_count <- seq_along(y)
  ones <- seq(1,1,length=length(y))

  content.matrix <- c(ones, time_count)
  X <- matrix(content.matrix, byrow = FALSE, ncol = 2)

  suppressWarnings({
  fm <- stats::lsfit(x = X, y = y, intercept = FALSE)
  data.b <- fm$coef[[2]]
  })
  
  return(data.b)
}