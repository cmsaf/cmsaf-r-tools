# Wrapper for arithmetics with constants
#
# Wrapper function for basic arithmetic functions with constants (cmsaf.addc,
# cmsaf.divc, cmsaf.mulc, cmsaf.subc). Argument op is one of 1-4, depending on
# the arithmetic function used.
arithc_wrapper <- function(op, var, const, infile, outfile, nc34, overwrite,
                           verbose) {
  check_variable(var)

  check_constant(const)
  check_infile(infile)
  check_outfile(outfile)

  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)

  if (op == 4 && const == 0) {
    stop("Division by zero not allowed!")
  }

  calc_time_start <- Sys.time()

  # get information about dimensions and attributes
  file_data <- read_file(infile, var)
  if (op > 2) {
    file_data$variable$prec <- "float"
  }

  result <- array(file_data$variable$attributes$missing_value,
                  dim = c(length(file_data$dimension_data$x),
                          length(file_data$dimension_data$y),
                          1))
  time_bnds <- array(NA, dim = c(2, 1))
  vars_data <- list(result = result, time_bounds = time_bnds)

  # create netcdf
  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(op,
                       paste0("cmsaf::cmsaf.addc ", const, " added to ", var),
                       paste0("cmsaf::cmsaf.subc ", const, " subtracted from ",
                              var),
                       paste0("cmsaf::cmsaf.mulc ", var, " multiplied by ",
                              const),
                       paste0("cmsaf::cmsaf.divc ", var, " divided by ", const)
  )

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      0,
                      NB2,
                      file_data$time_info$units,
                      with_time_bnds = file_data$time_info$has_time_bnds
  )

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

  # calculate results

  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile)
  }
  nc_in <- nc_open(infile)
  nc_out <- nc_open(outfile, write = TRUE)

  for (i in seq_len(length(file_data$dimension_data$t))) {
    dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, i),
                         count = c(-1, -1, 1))
    dum_dat <- switch(op,
                      dum_dat + const,
                      dum_dat - const,
                      dum_dat * const,
                      dum_dat / const
    )
    dum_dat[is.na(dum_dat)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], dum_dat, start = c(1, 1, i),
              count = c(-1, -1, 1))
    ncvar_put(nc_out, dims$t, file_data$dimension_data$t[i], start = i,
              count = 1)
    if (file_data$time_info$has_time_bnds) {
      ncvar_put(nc_out, vars[[2]], time_bnds[, i], start = c(1, i),
                count = c(-1, 1))
    }
  }
  nc_close(nc_in)
  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
