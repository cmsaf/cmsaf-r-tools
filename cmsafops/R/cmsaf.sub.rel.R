#'Subtract the fields of two input NetCDF files (relative).
#'Designed for the CM SAF R Toolbox.
#'
#'The function subtracts the fields of infile2 from the fields of infile1.
#'Infiles have to have the same spatial and temporal dimension.
#'
#'@param var1  Name of variable in infile1 (character).
#'@param var2  Name of variable in infile2 (character).
#'@param infile1 Filename of first input NetCDF file. This may include the
#'  directory (character).
#'@param infile2 Filename of second input NetCDF file. This may include the
#'  directory (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including the subtracted fields of infile1 and infile2
#'  is written.
#'  
#'@export
cmsaf.sub.rel <- function(var1, infile1, var2, infile2, outfile, nc34=4,
                      overwrite = FALSE, verbose = FALSE) {
  check_variable(var1)
  check_variable(var2)
  
  check_infile(infile1)
  check_infile(infile2)
  check_outfile(outfile)
  
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  
  check_nc_version(nc34)
  
  # get information about dimensions and attributes
  file_data1 <- read_file(infile1, var1)
  file_data2 <- read_file(infile2, var2)
  
  file_data1$variable$prec <- "float"
  file_data2$variable$prec <- "float"
  
  # check dimensions of infile1 and infile2
  if (file_data1$grid$is_regular != file_data2$grid$is_regular) {
    stop("Please use infiles with the same projection or use the remap operator")
  }
  
  if (!(length(file_data1$dimension_data$x) == length(file_data2$dimension_data$x) &&
        length(file_data1$dimension_data$y) == length(file_data2$dimension_data$y))) {
    stop("Dimensions of infiles do not match!")
  }
  
  result <- array(file_data1$variable$attributes$missing_value,
                  dim = c(length(file_data1$dimension_data$x),
                          length(file_data1$dimension_data$y),
                          1))
  
  vars_data <- list(result = result)
  
  if (length(file_data1$dimension_data$t) > 1 && length(file_data2$dimension_data$t) > 1 &&
      length(file_data1$dimension_data$t) != length(file_data2$dimension_data$t)) {
    stop("Uncompatible time lengths!")
  }
  
  time_len1 <- length(file_data1$dimension_data$t)
  time_len2 <- length(file_data2$dimension_data$t)
  
  if (time_len1 == time_len2 && time_len1 > 1) {
    case <- 1
    time <- file_data1$dimension_data$t
  }
  time_len <- length(time)
  
  # create netcdf
  nc_format <- get_nc_version(nc34)
  
  cmsaf_info <- paste0("cmsaf::cmsaf.sub.rel for variable ", var1)
  
  ##### prepare output #####
  global_att_list <- names(file_data1$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data1$global_att[global_att_list]
  
  dims <- define_dims(file_data1$grid$is_regular,
                      file_data1$dimension_data$x,
                      file_data1$dimension_data$y,
                      0,
                      NB2,
                      file_data1$time_info$units,
                      with_time_bnds = FALSE
  )
  
  vars <- define_vars(file_data1$variable, dims, nc_format$compression, with_time_bnds = FALSE)
  
  write_output_file(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data1$variable$name,
    file_data1$grid$vars, file_data1$grid$vars_data,
    cmsaf_info,
    file_data1$time_info$calendar,
    file_data1$variable$attributes,
    global_attributes,
    with_time_bnds = FALSE
  )
  
  # get data of infile1 and infile2 and calculate corresponding fields
  nc_in1 <- nc_open(infile1)
  nc_in2 <- nc_open(infile2)
  nc_out <- nc_open(outfile, write = TRUE)
  
  switch(case,
         {
           for (i in seq_len(time_len)) {
             dum_dat1 <- ncvar_get(nc_in1, file_data1$variable$name,
                                   start = c(1, 1, i), count = c(-1, -1, 1))
             dum_dat2 <- ncvar_get(nc_in2, file_data2$variable$name,
                                   start = c(1, 1, i), count = c(-1, -1, 1))
             dum_data <- ((dum_dat1 - dum_dat2)/dum_dat2)*100
             # dum_data_min <- do.call(min, c(na.rm = TRUE, lapply(seq_len(dim(dum_data)[2]), function(i) dum_data[,i])))
             # dum_data_max <- do.call(max, c(na.rm = TRUE, lapply(seq_len(dim(dum_data)[2]), function(i) dum_data[,i])))
             
             # if(dum_data_max != dum_data_min) {
             #   dum_data <- (dum_data - dum_data_min)/(dum_data_max - dum_data_min)
             # }

             dum_data[is.na(dum_data)] <- file_data1$variable$attributes$missing_value
             ncvar_put(nc_out, vars[[1]], dum_data, start = c(1, 1, i),
                       count = c(-1, -1, 1))
             ncvar_put(nc_out, dims$t, time[i], start = i, count = 1)
           }
         }
  )
  
  nc_close(nc_in1)
  nc_close(nc_in2)
  nc_close(nc_out)
}