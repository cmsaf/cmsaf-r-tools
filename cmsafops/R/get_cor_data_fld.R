get_cor_data_fld <- function(dataset_number, lon_number, lat_number, time_number){
  data1 <- array(numeric(),c(lon_number, lat_number, time_number)) 
  data2 <- array(numeric(),c(lon_number, lat_number, time_number)) 
  for (i in 1:time_number){
    df_new <- simcor((lon_number * lat_number), 50, 20, 30, 30, i)
    for (j in 1:(lon_number * lat_number)) {
      data1[i + (j-1)*(time_number)] <- df_new$x[j]
      data2[i + (j-1)*(time_number)] <- df_new$y[j]
    }
  }
  if(dataset_number == 1)
    return(data1)
  else if(dataset_number == 2)
    return(data2)
  else
    cat("Only two data sets are supported. Use 1 or 2 for parameter dataset_number.")
}