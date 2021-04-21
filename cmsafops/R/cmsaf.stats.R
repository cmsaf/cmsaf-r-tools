#'Calculates the rmse, mae, bias, correlation in grid space of two NetCDF files.
#'Designed for the CM SAF R Toolbox.
#'
#'@param var1 Name of NetCDF variable of the first file (character).
#'@param var2 Name of NetCDF variable of the second file (character).
#'@param infile1 Filename of first input NetCDF file. This may include 
#'  the directory (character).
#'@param infile2 Filename of second input NetCDF file. This may include 
#'  the directory (character).
#'@param outfile Filename of output csv file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of input file. Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?; Default: FALSE
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A csv file including the rmse, mae, bias and correlation in grid space 
#'  is written. 
#'@export
#'
#'@family metrics

cmsaf.stats <- function(var1, var2, infile1, infile2, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE){
  gc()
  calc_time_start <- Sys.time()
  
  if(overwrite){
    if(file.exists(outfile)){
      unlink(outfile)
    }
  }
  
  check_variable(var1)
  check_variable(var2)
  
  check_infile(infile1)
  check_infile(infile2)
  
  check_nc_version(nc34)
  
  ##### extract data from two files #####
  temp_outfile_one <- file.path(tempdir(), "outfile_one_tmp.nc")
  temp_outfile_two <- file.path(tempdir(), "outfile_two_tmp.nc")
  
  # unlink tmp files
  if(file.exists(temp_outfile_one)){
    unlink(temp_outfile_one)
  }
  if(file.exists(temp_outfile_two)){
    unlink(temp_outfile_two)
  }
  cmsafops::cmsaf.adjust.two.files(var1 = var1, infile1 = infile1, 
                                   var2 = var2, infile2 = infile2, 
                                   outfile1 = temp_outfile_one, outfile2 = temp_outfile_two, 
                                   nc34 = nc34, overwrite = overwrite, verbose = verbose)
  
  temp_outfile_cor <- file.path(tempdir(), "outfile_tmp_cor.nc")
  if(file.exists(temp_outfile_cor)){
    unlink(temp_outfile_cor)
  }
  
  # fldcor
  argumentList <- list(var1 = var1,
                       var2 = var2,
                       infile1 = temp_outfile_one,
                       infile2 = temp_outfile_two,
                       outfile = temp_outfile_cor,
                       nc34 = nc34,
                       overwrite = overwrite,
                       verbose = verbose)
  fun <- get("fldcor", asNamespace("cmsafops"))
  try(do.call(fun, argumentList))
  
  # read file fldcor
  nc_in <- nc_open(temp_outfile_cor)
  correlation <- ncvar_get(nc_in, var1, collapse_degen = FALSE)
  nc_close(nc_in)
  
  
  temp_outfile <- file.path(tempdir(), "outfile_tmp.nc")
  if(file.exists(temp_outfile)){
    unlink(temp_outfile)
  }
  
  # difference
  argumentList <- list(var1 = var1,
                       var2 = var2,
                       infile1 = temp_outfile_one,
                       infile2 = temp_outfile_two,
                       outfile = temp_outfile,
                       nc34 = nc34,
                       overwrite = overwrite,
                       verbose = verbose)
  fun <- get("cmsaf.sub", asNamespace("cmsafops"))
  try(do.call(fun, argumentList))
  
  # read file
  file_data <- read_file(temp_outfile, var1)
  
  nc_in <- nc_open(temp_outfile)
  dum_dat <- ncvar_get(nc_in, var1, collapse_degen = FALSE)
  date <- ncdf4::ncvar_get(nc_in, "time")
  t_unit <- ncdf4::ncatt_get(nc_in, "time", "units")$value
  date.time <- as.character(cmsafops::get_time(t_unit, date))
  nc_close(nc_in)
  
  rmse <- c()   # RMSE (root mean squared error)
  mae <- c()    # MAE (mean absolute error)
  bias <- c()
  for(i in seq_along(file_data$dimension_data$t)){
    rmse <- append(rmse, sqrt(mean((c(dum_dat[,,i]))^2, na.rm = TRUE)))
    mae <- append(mae, mean(abs(c(dum_dat[,,i])), na.rm = TRUE))
    bias <- append(bias, sqrt(  mean((c(dum_dat[,,i]))^2, na.rm = TRUE) - stats::var(c(dum_dat[,,i]), na.rm = TRUE) ))
  }
  
  result_stats <- cbind(date.time, rmse, mae, bias, correlation)
  result_stats_df <- as.data.frame(result_stats)
  utils::write.csv(x = result_stats_df, file = outfile, row.names = FALSE)
}