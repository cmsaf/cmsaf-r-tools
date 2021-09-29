seasx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose, nc = NULL) {
  calc_time_start <- Sys.time()

  check_variable(var)
  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var, nc = nc)
  file_data$variable$prec <- "float"

  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  months_all <- date_time$months
  months_unique <- sort(unique(months_all))
  years_all <- date_time$years
  years_unique <- sort(unique(years_all))

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            1)
  )
  time_bnds <- get_time_bounds_1(
    file_data$dimension_data$t
  )
  vars_data <- list(result = data_placeholder, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(
    op,
    paste0("cmsaf::seasmean for variable ", file_data$variable$name),
    paste0("cmsaf::seassum for variable ", file_data$variable$name),
    paste0("cmsaf::seassd for variable ", file_data$variable$name),
    paste0("cmsaf::seasvar for variable ", file_data$variable$name)
  )

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data = 0,
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

  ##### calculate and write result #####
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)
  nc_out <- nc_open(outfile, write = TRUE)
  dummy_vec <- seq_along(months_all)

  count <- 1
  for (i in seq_along(years_unique)) {
    for (j in 1:4) {

      dum <- NA
      switch(j,
             {
               win <- which(years_all == years_unique[i] & months_all %in% c(1:2) | years_all == years_unique[i] - 1 & months_all == 12)
               if (length(win) >= 3) {
                 dum <- win
               }
             },
             {
               spr <- which(years_all == years_unique[i] & months_all %in% c(3:5))
               if (length(spr) >= 3) {
                 dum <- spr
               }
             },
             {
               sum <- which(years_all == years_unique[i] & months_all %in% c(6:8))
               if (length(sum) >= 3) {
                 dum <- sum
               }
             },
             {
               aut <- which(years_all == years_unique[i] & months_all %in% c(9:11))
               if (length(aut) >= 3) {
                 dum <- aut
               }
             }
             )

      if (all(is.na(dum))) {
        if (verbose) message("Not enough data to calculate a seasonal maximum!")
        next()
      }
        dum_dat <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), length(dum)))

        for (k in seq_along(dum)) {
          if (!is.na(dum[k])) {
            dum_dat[, , k] <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, dum[k]), count = c(-1, -1, 1), collapse_degen = FALSE)
          }
        }

    if (!(length(dum) == 3 | length(dum) > 85)) {
      if (verbose) message("Not enough data to calculate a seasonal maximum!")
      next()
    }

    switch(op,
           {
             if (verbose) message(paste0("apply seasonal mean ", count, " of ", 4 * length(years_unique)))
             data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply seasonal sum ", count, " of ", 4 * length(years_unique)))
             data <- rowSums(dum_dat, dims = 2, na.rm = TRUE) * ifelse(rowSums(is.na(dum_dat), dims = 2) == dim(dum_dat)[3], NA, 1)
           },
           {
             if (verbose) message(paste0("apply seasonal standard deviation ", count, " of ", 4 * length(years_unique)))
             data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply seasonal variance ", count, " of ", 4 * length(years_unique)))
             data <- apply(dum_dat, c(1, 2), stats::var, na.rm = TRUE)
           }
    )

    data[is.na(data)] <- file_data$variable$attributes$missing_value
    tdum <- min(file_data$dimension_data$t[dum], na.rm = TRUE)
    time_bnds[1, 1] <- min(file_data$dimension_data$t[dum], na.rm = TRUE)
    time_bnds[2, 1] <- max(file_data$dimension_data$t[dum], na.rm = TRUE)
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, count), count = c(-1, -1, 1))
    ncvar_put(nc_out, dims$t, tdum, start = count, count = 1)
    ncvar_put(nc_out, vars[[2]], time_bnds, start = c(1, count), count = c(-1, 1))
    count <- count + 1
    }
  }

  nc_close(nc_out)
  if (is.null(nc)) nc_close(nc_in)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
