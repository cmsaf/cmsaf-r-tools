#'Transform the coordinate system to -180 to 180 longitude of an infile
#'
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param var Name of NetCDF variable (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'
#'@return A NetCDF file including the coordinate system (-180 to 180 longitude) is
#'  written.
#'@export
#'
#'@family data manipulation functions

cmsaf.transform.coordinate.system <- function(infile, var, outfile){
  # read .nc-file 
  file_data <- read_file(infile, var)
  nc_in <- nc_open(infile)

  # read data from infile
  dum_dat <- ncvar_get(
    nc_in,
    file_data$variable$name,
    collapse_degen = FALSE
  )
  
  nc_close(nc_in)

  # convert coordinate system
  temp_file <- file.path(tempdir(),"temp_file.nc")
  data.matrix <- t(as.matrix(dum_dat[,,1])) 
  r <- raster::raster(nrow=length(file_data$dimension_data$y), ncol=length(file_data$dimension_data$x))
  raster::values(r) <- as.vector(t(data.matrix))
  raster::extent(r) <- raster::extent(0, 360, -90, 90)
  raster::rotate(r, filename=temp_file, format = "CDF", overwrite = TRUE, varname= var)
   
  # read temp_file.nc
  nc_in_tmp <- nc_open(temp_file)

  # variables
  #varnames <- names(nc_in_tmp$var)
  dim_names   <- names(nc_in_tmp$dim)
  dimensions <- get_dimensions(nc_in_tmp, dim_names)

  # use new coordinate system (180 to -180 for longitude)
  dimension.data <- list(
    x = nc_in_tmp$dim[[dimensions$names$x]]$vals,
    y = nc_in_tmp$dim[[dimensions$names$y]]$vals
  )
  nc_close(nc_in_tmp)
  
  vars_data <- list()
  file_data$variable$prec <- "float"
  nc34 <- 4 # use NetCDF 4
  nc_format <- get_nc_version(nc34)
  cmsaf_info <- paste0("Created with the CM SAF R Toolbox.")
  
  time_data <- file_data$dimension_data$t
  
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]
  
  dims <- define_dims(file_data$grid$is_regular,
                      dimension.data$x,
                      dimension.data$y,
                      time_data,
                      NB2,
                      file_data$time_info$units,
                      with_time_bnds = FALSE)
  
  vars <- define_vars(file_data$variable, dims, nc_format$compression, with_time_bnds = FALSE)
 
  write_output_file(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data$variable$name,
    file_data$grid$vars, file_data$grid$vars_data,
    cmsaf_info,
    file_data$time_info$calendar,
    file_data$variable$attributes,
    global_attributes,
    with_time_bnds = FALSE,
    write_result = FALSE
  )
  
  nc_out <- nc_open(outfile, write = TRUE)
  
  for(i in 1:length(file_data$dimension_data$t)){

    data.matrix <- t(as.matrix(dum_dat[,,i]))
    r <- raster::raster(nrow=length(dimension.data$y), ncol=length(dimension.data$x))
    raster::values(r) <- as.vector(t(data.matrix))
    raster::extent(r) <- raster::extent(0, 360, -90, 90)
    
    rr <- raster::rotate(r, varname = var)
    dum_dat_temp <- as.array(t(raster::as.matrix(rr)))
    ncvar_put(nc_out, vars[[1]], dum_dat_temp, start = c(1, 1, i), count = c(-1, -1, 1))
  }
  nc_close(nc_out)
  
  # remove temp_file.nc
  if(file.exists(temp_file)){
    unlink(temp_file)
  }
}