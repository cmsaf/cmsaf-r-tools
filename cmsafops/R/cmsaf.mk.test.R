#'Apply Mann-Kendall trend test.
#'
#'The function determines the trend from data of a single CM SAF NetCDF input
#'file basing on a Mann-Kendall test.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including three data layers is written. One layer contains a 
#'  measure for the significance of the calculated mann-kendall statistic (S). A very 
#'  high positive value of S is an indicator of an increasing trend and a very low 
#'  negative value indicates a decreasing trend. Another layer (Z) contains the calculated 
#'  normalized test statsitic Z. A positive value of Z is an indicator of an increasing 
#'  trend and a negative value indicates a decreasing trend.
#'@export
#'
#'@family temporal operators
#'
#' @examples
#'## Create an example NetCDF file with a similar structure as used by CM
#'## SAF. The file is created with the ncdf4 package.  Alternatively
#'## example data can be freely downloaded here: <https://wui.cmsaf.eu/>
#'
#'library(ncdf4)
#'
#'## create some (non-realistic) example data
#'
#'lon <- seq(10, 15, 0.5)
#'lat <- seq(50, 55, 0.5)
#'time <- seq(as.Date("2000-01-01"), as.Date("2010-12-31"), "month")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(11, 11, 132))
#'
#'## create example NetCDF
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time, unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -1, prec = "short")
#'vars <- list(var1)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file.nc"), vars)
#'ncvar_put(ncnew, var1, data)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'
#'## Determine the trend of the example CM SAF NetCDF file and write the
#'## output to a new file.
#'cmsaf.mk.test(var = "SIS", infile = file.path(tempdir(),
#'  "CMSAF_example_file.nc"), outfile = file.path(tempdir(),
#'  "CMSAF_example_file_mktrend.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_mktrend.nc")))
cmsaf.mk.test <- local({
  target.vector.S <- c()
  target.vector.Z <- c()
  function(var, infile, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE)
  {
    target.vector.S <<- c()
    target.vector.Z <<- c()
    gc()
    check_variable(var)
    
    check_infile(infile)
    check_outfile(outfile)
    
    outfile <- correct_filename(outfile)
    check_overwrite(outfile, overwrite)
    
    check_nc_version(nc34)
    
    calc_time_start <- Sys.time()
    
    S.value <- list(name = "S",
                standard_name = "mann-kendall statistic",
                long_name = "significance based on mann-kendall statistic",
                units = "1",
                info = "0 < positive significant, 0 = not significant, 0 > negative significant")
    
    Z.value <- list(name = "Z",
                    standard_name = "mann-kendall normalized statistic",
                    long_name = "significance based on mann-kendall normalized statistic",
                    units = "1",
                    info = "0 < positive significant, 0 = not significant, 0 > negative significant")
    
    ##### extract data from file #####
    file_data <- read_file(infile, var)
    file_data$variable$prec <- "float"
    
    time_bnds <- get_time_bounds_1(
      file_data$dimension_data$t
    )
    
    nc_in <- nc_open(infile)
    
    length.dimension.x <- length(file_data$dimension_data$x)
    length.dimension.y <- length(file_data$dimension_data$y)
    length.dimension.t <- length(file_data$dimension_data$t)
    
    dum_dat_1 <- array(NA, dim = c(length.dimension.x,
                                   length.dimension.y,
                                   length.dimension.t))
    
    for (i in seq_along(file_data$dimension_data$t)) {
      dum_dat_t <- ncvar_get(
        nc_in,
        file_data$variable$name,
        start = c(1, 1, i), count = c(-1, -1, 1),
        collapse_degen = FALSE
      )
      dum_dat_1[,,i] <- dum_dat_t
    }
    nc_close(nc_in)
    
    dum_dat_t_all <- c()
    
    # calc mk.test
    lapply(1:length.dimension.x, function(j){
      lapply(1:length.dimension.y, function(k){
        for(i in seq(length.dimension.t)){
          dum_dat_t_all <- append(dum_dat_t_all, dum_dat_1[j,k,i])
        }
        if(all(is.na(dum_dat_t_all))) # check na
        {
          target.vector.S <<- append(target.vector.S, 0)
          target.vector.Z <<- append(target.vector.Z, 0)
        }
        else{
          dum_dat_t_all[is.na(dum_dat_t_all)] <- 0
          result.mk.test <- trend::mk.test(as.vector(dum_dat_t_all), continuity = TRUE)
          target.vector.S <<- append(target.vector.S, result.mk.test$estimates[[1]])
          target.vector.Z <<- append(target.vector.Z, result.mk.test$statistic[[1]])
        }
        dum_dat_t_all <- c()
      })
    })
    
    # transform S vector
    result_array_S <- array(numeric(),c(length.dimension.x,length.dimension.y,1)) 
    for(i in 1:length(target.vector.S)){
      result_array_S[i] <- target.vector.S[i]
    }
    target.S <- aperm(result_array_S)
    
    # transform Z vector 
    result_array_Z <- array(numeric(),c(length.dimension.x,length.dimension.y,1)) 
    for(i in 1:length(target.vector.Z)){
      result_array_Z[i] <- target.vector.Z[i]
    }
    target.Z <- aperm(result_array_Z)
    
    result <- list(target.S = target.S, target.Z = target.Z)
    vars_data <- list(result = result, time_bounds = time_bnds)
  
    nc_format <- get_nc_version(nc34)
    cmsaf_info <- paste0("cmsafops::cmsaf.mk.test for variable ",
                         file_data$variable$name)
    time_data <- time_bnds[1, ]
  
    ##### prepare output #####
    global_att_list <- names(file_data$global_att)
    global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
    global_attributes <- file_data$global_att[global_att_list]
  
    dims <- define_dims(file_data$grid$is_regular,
                        file_data$dimension_data$x,
                        file_data$dimension_data$y,
                        time_data,
                        NB2,
                        file_data$time_info$units)
  
    vars <- define_vars_mk.test(file_data$variable, dims, nc_format$compression, S.value, Z.value)
  
    write_output_file_mk.test(
      outfile,
      nc_format$force_v4,
      vars,
      vars_data,
      file_data$variable$name,
      file_data$grid$vars, file_data$grid$vars_data,
      S.value, Z.value, file_data$variable$attributes$standard_name,
      cmsaf_info,
      file_data$time_info$calendar,
      file_data$variable$attributes,
      global_attributes
    )
  
    calc_time_end <- Sys.time()
    if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
  }
})