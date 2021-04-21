timx_wrapper <- function(op, var, infile, outfile, nc34, overwrite,
                         na.rm = TRUE, p = NULL, verbose) {
  calc_time_start <- Sys.time()

  check_variable(var)
  check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var)
  if (op %in% c("mean", "sum", "sd", "pctl")) {
    file_data$variable$prec <- "float"
  }

  time_bnds <- get_time_bounds_1(
    file_data$dimension_data$t
  )

  if (op == "pctl") {
    if (length(p) > 1) {
      p <- p[1]
    }
    if (p < 0 || p > 1) {
      if (verbose) message("Your given p-value is outside [0,1]. The default will be used (0.95).")
      p <- 0.95
    }
  }

  result <- calc_timx_result(op, infile, file_data$dimension_data,
                            file_data$variable$name, na.rm, p)
  result[is.na(result)] <- file_data$variable$attributes$missing_value

  vars_data <- list(result = result, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(op,
         max = {"cmsaf::timmax"},
         min = {"cmsaf::timmin"},
         mean = {"cmsaf::timmean"},
         sum = {"cmsaf::timsum"},
         sd = {"cmsaf::timsd"},
         pctl = {paste0("cmsaf::timpctl with p = ", p)}
  )
  cmsaf_info <- paste0(cmsaf_info, " for variable ", file_data$variable$name)

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
