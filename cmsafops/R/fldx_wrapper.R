fldx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose) {
  calc_time_start <- Sys.time()

  check_variable(var)
  check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var)
  if (op > 2) {
    file_data$variable$prec <- "float"
  }
  if (op < 4 || op > 4) {
    file_data$dimension_data$x <- round((max(file_data$dimension_data$x, na.rm = TRUE) + min(file_data$dimension_data$x, na.rm = TRUE)) / 2, digits = 2)
    file_data$dimension_data$y <- round((max(file_data$dimension_data$y, na.rm = TRUE) + min(file_data$dimension_data$y, na.rm = TRUE)) / 2, digits = 2)
    result <- calc_field(infile, file_data, op)
  }else{
    grid <- raster::raster(nrows = length(file_data$dimension_data$x),
                           ncols = length(file_data$dimension_data$y),
                           xmn = min(file_data$dimension_data$x),
                           xmx = max(file_data$dimension_data$x),
                           ymn = min(file_data$dimension_data$y),
                           ymx = max(file_data$dimension_data$y))

    area <- raster::area(grid, weights = TRUE)
    weights <- raster::as.matrix(area)

    result <- calc_field(infile, file_data, op, weights)

    file_data$dimension_data$x <- round((max(file_data$dimension_data$x, na.rm = TRUE) + min(file_data$dimension_data$x, na.rm = TRUE)) / 2, digits = 2)
    file_data$dimension_data$y <- round((max(file_data$dimension_data$y, na.rm = TRUE) + min(file_data$dimension_data$y, na.rm = TRUE)) / 2, digits = 2)
  }

  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile)
    vars_data <- list(result = result, time_bounds = time_bnds)
  }else{
    vars_data <- list(result = result)
  }

  nc_format <- get_nc_version(nc34)
  cmsaf_info <- switch(
    op,
    paste0("cmsaf::fldmax for variable ",
           file_data$variable$name),
    paste0("cmsaf::fldmin for variable ",
           file_data$variable$name),
    paste0("cmsaf::fldmean for variable ",
           file_data$variable$name),
    paste0("cmsaf::wfldmean for variable ",
           file_data$variable$name),
    paste0("cmsaf::fldrange for variable ",
           file_data$variable$name),
    paste0("cmsaf::fldsd for variable ",
           file_data$variable$name),
    paste0("cmsaf::fldsum for variable ",
           file_data$variable$name)
  )

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      file_data$dimension_data$t,
                      NB2,
                      file_data$time_info$units,
                      with_time_bnds = file_data$time_info$has_time_bnds)

  if (!file_data$grid$is_regular) {
  new_data <- list()
  new_data[[which(names(file_data$grid$vars) %in% LON_NAMES)]] <- round((max(file_data$grid$vars_data[[LON_NAMES$DEFAULT]], na.rm = TRUE) + min(file_data$grid$vars_data[[LON_NAMES$DEFAULT]], na.rm = TRUE)) / 2, digits = 2)
  new_data[[which(names(file_data$grid$vars) %in% LAT_NAMES)]] <- round((max(file_data$grid$vars_data[[LAT_NAMES$DEFAULT]], na.rm = TRUE) + min(file_data$grid$vars_data[[LAT_NAMES$DEFAULT]], na.rm = TRUE)) / 2, digits = 2)
  file_data$grid <- redefine_grid_vars(file_data$grid, dims,
                                       nc_format$compression, new_data)
  }

  vars <- define_vars(file_data$variable, dims, nc_format$compression,
                      with_time_bnds = file_data$time_info$has_time_bnds)

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
    with_time_bnds = file_data$time_info$has_time_bnds
  )

  calc_time_end <- Sys.time()
  if (verbose) {
    message(get_processing_time_string(calc_time_start, calc_time_end))
  }
}
