# Wrapper for number of timesteps depending on threshold
#
# Wrapper function for counting the number of timesteps depending on a threshold
# (num_above, num_below, num_equal). Argument op is one of 1-3, depending on
# the function used.
num_wrapper <- function(op, var, thld, infile, outfile, nc34, overwrite,
                           verbose) {
  check_variable(var)
  check_constant(thld)
  check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  calc_time_start <- Sys.time()
  
  ##### extract data from file #####
  file_data <- read_file(infile, var)
  
  time_bnds <- get_time_bounds_1(
    file_data$dimension_data$t
  )

  # calculate results
  nc_in <- nc_open(infile)
  
  # initialize array with zeros
  result <- array(0, dim = c(length(file_data$dimension_data$x), 
					length(file_data$dimension_data$y)))

  for (i in seq_len(length(file_data$dimension_data$t))) {
    dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, i),
                         count = c(-1, -1, 1))
	
    dum_dat <- switch(op,
                      dum_dat > thld,
                      dum_dat < thld,
                      dum_dat == thld
    )
    
    dum_dat[is.na(dum_dat)] <- 0
	  result <- result + dum_dat
	
  }

  nc_close(nc_in)
  vars_data <- list(result = result, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)
  file_data$variable$prec <- "short"

  cmsaf_info <- switch(op,
                       paste0("cmsafops::num_above for variable ", var),
                       paste0("cmsafops::num_below for variable ", var),
                       paste0("cmsafops::num_equal for variable ", var)
  )

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

  vars <- define_vars(file_data$variable, dims, nc_format$compression)

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
    global_attributes
  )

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
