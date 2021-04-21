ranking <- function(out_dir, 
                    var, 
                    country_code, 
                    climate_year_start, 
                    climate_year_end,
                    doy)
{
  
  out_dir = paste0(out_dir, "/mc_temp")
  years    <- NULL
  values   <- NULL
  
  for (i in climate_year_start:climate_year_end){
    filename <- paste(var, "_",i,"_", country_code,"_fldmean.nc",sep="")
    file <- file.path(out_dir,filename)
    
    if(file.exists(file)){
      data <- cmsafops::read_ncvar(var,file)
      data <- unlist(data)
      data <- as.numeric(data)
      
      years  <- append(years,i)
      values <- append(values,data[doy])
    }
  }
  
  ranking <- data.frame(years,values)
  names(ranking) <- c("Year","Value")
  
  return(ranking[order(ranking$Value),])
}