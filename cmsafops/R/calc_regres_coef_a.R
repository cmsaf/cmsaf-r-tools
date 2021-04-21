# calculate coefficient a of linear regression model
calc_regres_coef_a <- function(y)
{
  time_count <- seq_along(y)
  ones <- seq(1,1,length=length(y))

  content.matrix <- c(ones, time_count)
  X <- matrix(content.matrix, byrow = FALSE, ncol = 2)
  
  suppressWarnings({
  fm <- stats::lsfit(x = X, y = y)
  data.a <- fm$coef[[1]]
  })
  
  return(data.a)
}