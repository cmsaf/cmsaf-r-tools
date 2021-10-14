runx_wrapper <- function(op, var, nts, infile, outfile, nc34, overwrite, verbose, nc = NULL) {
  calc_time_start <- Sys.time()
  gc()
  check_variable(var)
  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var, nc = nc)
  if(nts <= length(file_data$dimension_data$t))
  {
    if (op > 2) {
      file_data$variable$prec <- "float"
    }
  
    date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  
    # Use placeholder for result so that it can be calculated later without the
    # need to have all input data in memory concurrently.
    length.dimension.x <- length(file_data$dimension_data$x)
    length.dimension.y <- length(file_data$dimension_data$y)
    length.dimension.t <- (length(file_data$dimension_data$t)-(nts-1))
    # data_placeholder <- array(
    #   file_data$variable$attributes$missing_value,
    #   dim = c(length.dimension.x,
    #           length.dimension.y,
    #           length.dimension.t)
    # )
  
    time_bnds <- get_time_bounds_run(file_data$dimension_data$t, nts)
  
    vars_data <- list(time_bounds = time_bnds)
    nc_format <- get_nc_version(nc34)
  
    cmsaf_info <- switch(
      op,
      paste0("cmsaf::runmin for variable ", file_data$variable$name),
      paste0("cmsaf::runmax for variable ", file_data$variable$name),
      paste0("cmsaf::runrange for variable ", file_data$variable$name),
      paste0("cmsaf::runsum for variable ", file_data$variable$name),
      paste0("cmsaf::runmean for variable ", file_data$variable$name),
      paste0("cmsaf::runsd for variable ", file_data$variable$name),
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
      global_attributes,
      write_result = FALSE # avoid vector memory exhaustion, Can be adjust
    )
  
    ##### calculate and write result #####
    nc_out <- nc_open(outfile, write = TRUE)
    if (!is.null(nc)) nc_in <- nc
    else nc_in <- nc_open(infile)
  
    # limit <- 2601 * 2601 * 31  # limit to avoid vector memory exhaustion, Can be adjust
    # dimensionality <- as.double(length.dimension.x) *
    #   as.double(length.dimension.y) * as.double(length.dimension.t)
    # 
    # if(limit < dimensionality){
    #   for (i in 1:length.dimension.t) {
    #     data <- data_placeholder[,,i]
    #     ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, i), count = c(-1, -1, 1))
    #   }
    # }else{
    #   ncvar_put(nc_out, vars[[1]], data_placeholder)
    # }
    # rm(data_placeholder)
  
    startt <- 1
    countt <- nts
    
    # length of running time steps
    length_time <- length(file_data$dimension_data$t)-(nts-1)
    
    for (i in 1:length_time) {
  
      # read data from infile
      dum_dat <- ncvar_get(
        nc_in,
        file_data$variable$name,
        start = c(1, 1, startt),
        count = c(-1, -1, countt),
        collapse_degen = FALSE
      )
      startt <- startt + 1
  
      switch(op,
             {
                if (verbose) message(paste0("apply running minimum ", i," of ", length_time))
                data <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
             },
             {
               if (verbose) message(paste0("apply running maximum ", i," of ", length_time))
               data <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
             },
             {
               if (verbose) message(paste0("apply running range ", i," of ", length_time))
               data.max <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
               data.min <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
               data <- data.max - data.min
             },
             {
               if (verbose) message(paste0("apply running sum ", i," of ", length_time))
               data <- rowSums(dum_dat, dims = 2, na.rm = TRUE) * ifelse(rowSums(is.na(dum_dat), dims = 2) == dim(dum_dat)[3], NA, 1)
             },
             {
               if (verbose) message(paste0("apply running mean ", i," of ", length_time))
               data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
             },
             {
               if (verbose) message(paste0("apply running standard deviation ", i," of ", length_time))
               data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
             }
      )
  
      data[is.na(data)] <- file_data$variable$attributes$missing_value
      ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, i), count = c(-1, -1, 1))
    }
    
    if (is.null(nc)) nc_close(nc_in)
    nc_close(nc_out)
  }
  else{
    stop("The input of the time steps must not be greater than the number of time steps from the input file")
  }
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}