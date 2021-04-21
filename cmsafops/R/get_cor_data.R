# generates two correlated data sets (for the examples timcor and timcovar)

simcor <- function (n, xmean, xsd, ymean, ysd, set_number) {
  set.seed(set_number)
  correlation <- stats::runif(n, min=-1, max=1)
  x <- stats::rnorm(n)
  y <- stats::rnorm(n)
  z <- correlation * scale(x)[,1] + sqrt(1 - correlation^2) * 
    scale(stats::resid(stats::lm(y ~ x)))[,1]
  xresult <- xmean + xsd * scale(x)[,1]
  yresult <- ymean + ysd * z
  return(data.frame(x=xresult,y=yresult))
}

get_cor_data <- function(dataset_number, lon_number, lat_number, time_number){
  data1 <- array(numeric(),c(lon_number, lat_number, time_number)) 
  data2 <- array(numeric(),c(lon_number, lat_number, time_number)) 
  for (i in 1:(lon_number * lat_number)){
    df_new <- simcor(time_number, 50, 20, 30, 30, i)
    for (j in 1:time_number) {
      data1[i + (j-1)*(lon_number*lat_number)] <- df_new$x[j]
      data2[i + (j-1)*(lon_number*lat_number)] <- df_new$y[j]
    }
  }
  if(dataset_number == 1)
    return(data1)
  else if(dataset_number == 2)
    return(data2)
  else
    cat("Only two data sets are supported. Use 1 or 2 for parameter dataset_number.")
}
