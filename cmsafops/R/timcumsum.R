#' Accumulate data of NetCDF file.
#'
#' Computes the accumulation of the given variable over time. 
#' The resulting outfile has the same dimensions as the infile.
#'
#' @param var Name of variable in infile (character).
#' @param infile Character containing file name or path of input file.
#' @param outfile Character containing file name or path of output file. 
#'   If NULL, the input file is directly edited instead.
#' @param nc34 NetCDF version of output file. If \code{nc34 = 3} the output 
#'   file will be in NetCDFv3 format (numeric). Default output is NetCDFv4.
#' @param overwrite Logical; should existing output file be overwritten? 
#'   If outfile is NULL, this parameter is ignored.
#' @param na_replace Replacing NA values with either 'mean' or 'previous' 
#'   for monthly mean or previous value, respectively (character).
#' @param verbose logical; if TRUE, progress messages are shown
#'@param nc Alternatively to \code{infile} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#' @export
timcumsum <- function(var,
                 infile,
                 outfile,
                 nc34 = 4,
                 overwrite = FALSE,
                 na_replace = "mean",
                 verbose = FALSE,
                 nc = NULL) {
  calc_time_start <- Sys.time()

  check_variable(var)
  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)
  stopifnot(na_replace %in% c("mean", "previous"))
  ##### extract data from file #####
  file_data <- read_file(infile, var, nc = nc)

  stopifnot(length(file_data$dimension_data$t) > 1)

  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile, nc = nc)
  }
  if (file_data$time_info$has_time_bnds) {
    vars_data <- list(result = NA, time_bounds = time_bnds)
  } else {
    vars_data <- list(result = NA)
  }

  nc_format <- get_nc_version(nc34)

  cmsaf_info <-
    paste0("cmsafops::timcumsum for variable ", file_data$variable$name)

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <-
    global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(
    file_data$grid$is_regular,
    file_data$dimension_data$x,
    file_data$dimension_data$y,
    file_data$dimension_data$t,
    NB2,
    file_data$time_info$units,
    with_time_bnds = file_data$time_info$has_time_bnds
  )

  # Accumulating "short" values can lead to overflow. That's why we use "float".
  vars <-
    define_vars(
      file_data$variable,
      dims,
      nc_format$compression,
      with_time_bnds = file_data$time_info$has_time_bnds,
      precision = "float"
    )

  ##### write output file #####
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
    with_time_bnds = file_data$time_info$has_time_bnds,
    write_result = FALSE
  )

  ##### calculate #####
  limit <-
    2601 * 2601 * 31  # This value can be ajusted to avoid RAM overflow

  dimensionality <-
    as.double(length(file_data$dimension_data$x)) *
    as.double(length(file_data$dimension_data$y)) * as.double(length(file_data$dimension_data$t))

  nc_out <- nc_open(outfile, write = TRUE)

  if (dimensionality < limit) {
    # Result can directly be calculated.
    if (!is.null(nc)) nc_in <- nc
    else nc_in <- nc_open(infile)
    dum_dat <-
      ncvar_get(nc_in, file_data$variable$name, collapse_degen = FALSE)
    dum_times <-
      get_date_time(ncvar_get(nc_in, "time"), file_data$time_info$units)
    nc_close(nc_in)

    ##### replace NAs by monthly mean #####
    dum_na <- which(is.na(dum_dat), arr.ind = TRUE)
    if (nrow(dum_na) > 0) {
      if (na_replace == "mean") {
        of <- tempfile(fileext = ".nc")
        cmsafops::monmean(
          file_data$variable$name,
          infile = infile,
          outfile = of,
          overwrite = TRUE,
          nc = nc
        )

        nc_of <- nc_open(of)
        meandata <- ncvar_get(nc_of, file_data$variable$name, collapse_degen = FALSE)
        meantimes <-
          get_date_time(ncvar_get(nc_of, "time"),
                        file_data$time_info$units)
        mean_na <- which(is.na(meandata), arr.ind = TRUE)
        nc_close(nc_of)

      na_ind <-
        which(dum_na[, 1] %in% mean_na[, 1] &
                dum_na[, 2] %in% mean_na[, 2])
      if (length(na_ind) > 0)
        dum_na <- dum_na[-na_ind,]
        if (length(dum_na) == 3){
          dum_na2 <- array(NA, dim = c(1,3))
          dum_na2[1,] <- dum_na
          dum_na <- dum_na2
          rm(dum_na2)
        }
        dum_na_timestep <- unique(dum_na[, 3])
        dum_na_times <- dum_times[dum_na_timestep, ]
      for (k in seq_along(dum_na_timestep)) {
        mo <- which(meantimes$month == dum_na_times$month[k])
        yr <- which(meantimes$year == dum_na_times$year[k])
        dum_dat[, , dum_na_timestep[k]] <-
          meandata[,, mo[which(mo %in% yr)]]
      }
      } else {
        ref_na <- which(is.na(dum_dat[,,1]), arr.ind = TRUE)
        if (length(ref_na) > 0 && all(is.na(ref_na)))
          stop("Reference data invalid. The first record of your data is corrupt. Maybe try na_replace = 'mean' to repair.")
        na_ind <-
          which(dum_na[, 1] %in% ref_na[, 1] &
                  dum_na[, 2] %in% ref_na[, 2])
        if (length(na_ind) > 0)
          dum_na <- dum_na[-na_ind,]
          if (length(dum_na) == 3){
            dum_na2 <- array(NA, dim = c(1,3))
            dum_na2[1,] <- dum_na
            dum_na <- dum_na2
            rm(dum_na2)
          }
          dum_na_timestep <- unique(dum_na[, 3])
        for (k in seq_along(dum_na_timestep)) {
          dum_dat[, , dum_na_timestep[k]] <-
            dum_dat[, , dum_na_timestep[k] - 1]
        }
      }
    }
    ##### accumulate #####
    res <- apply(dum_dat, c(1, 2), cumsum)
    result <- aperm(res, c(2, 3, 1))
    ncvar_put(nc_out, vars[[1]], result)
  } else {
    nr_of_possible_x_dims <-
      round((limit / length(file_data$dimension_data$x)) / length(file_data$dimension_data$t))
    dimsteps_start <-
      seq(1,
          length(file_data$dimension_data$y),
          nr_of_possible_x_dims)
    dimsteps_count <-
      rep(nr_of_possible_x_dims, length(dimsteps_start))
    cor <-
      nr_of_possible_x_dims * length(dimsteps_start) - length(file_data$dimension_data$y)
    dimsteps_count[length(dimsteps_start)] <-
      dimsteps_count[length(dimsteps_start)] - cor

    for (i in seq_along(dimsteps_start)) {
      if (!is.null(nc)) nc_in <- nc
      else nc_in <- nc_open(infile)
      dum_dat <- ncvar_get(
        nc_in,
        file_data$variable$name,
        start = c(1, dimsteps_start[i], 1),
        count = c(-1, dimsteps_count[i], -1),
        collapse_degen = FALSE
      )
      dum_times <-
        get_date_time(ncvar_get(nc_in, "time"), file_data$time_info$units)
      if (is.null(nc)) nc_close(nc_in)

      ##### replace NAs by monthly mean #####
      dum_na <- which(is.na(dum_dat), arr.ind = TRUE)
      if (nrow(dum_na) > 0) {
        if (na_replace == "mean") {
        if (i == 1) {
          of <- tempfile(fileext = ".nc")
          cmsafops::monmean(
            file_data$variable$name,
            infile = infile,
            outfile = of,
            overwrite = TRUE,
            nc = nc
          )

          nc_of <- nc_open(of)
          meandata <- ncvar_get(nc_of, file_data$variable$name)
          meantimes <-
            get_date_time(ncvar_get(nc_of, "time"),
                          file_data$time_info$units)
          mean_na <- which(is.na(meandata), arr.ind = TRUE)
          nc_close(nc_of)
        }

        na_ind <-
          which(dum_na[, 1] %in% mean_na[, 1] &
                  dum_na[, 2] %in% mean_na[, 2])
        if (length(na_ind) > 0)
          dum_na <- dum_na[-na_ind,]
          if (length(dum_na) == 3){
            dum_na2 <- array(NA, dim = c(1,3))
            dum_na2[1,] <- dum_na
            dum_na <- dum_na2
            rm(dum_na2)
          }
          dum_na_timestep <- unique(dum_na[, 3])
          dum_na_times <- dum_times[dum_na_timestep, ]
        for (k in seq_along(dum_na_timestep)) {
          mo <- which(meantimes$month == dum_na_times$month[k])
          yr <- which(meantimes$year == dum_na_times$year[k])
          dum_dat[, , dum_na_timestep[k]] <-
            meandata[, dimsteps_start[i]:(dimsteps_start[i] + dimsteps_count[i] - 1), mo[which(mo %in% yr)]]
        }
        } else {
          ref_na <- which(is.na(dum_dat[,,1]), arr.ind = TRUE)
          if (length(ref_na) > 0 && all(is.na(ref_na)))
            stop("Reference data invalid. The first record of your data is corrupt. Maybe try na_replace = 'mean' to repair.")
          na_ind <-
            which(dum_na[, 1] %in% ref_na[, 1] &
                    dum_na[, 2] %in% ref_na[, 2])
          if (length(na_ind) > 0)
            dum_na <- dum_na[-na_ind,]
            if (length(dum_na) == 3){
              dum_na2 <- array(NA, dim = c(1,3))
              dum_na2[1,] <- dum_na
              dum_na <- dum_na2
              rm(dum_na2)
            }
            dum_na_timestep <- unique(dum_na[, 3])
          for (k in seq_along(dum_na_timestep)) {
            dum_dat[, , dum_na_timestep[k]] <-
              dum_dat[, , dum_na_timestep[k] - 1]
          }
        }
      }

      ##### accumulate #####
      res <- apply(dum_dat, c(1, 2), cumsum)
      result <- aperm(res, c(2, 3, 1))
      ncvar_put(
        nc_out,
        vars[[1]],
        result,
        start = c(1, dimsteps_start[i], 1),
        count = c(-1, dimsteps_count[i], -1)
      )
      # ncvar_put(nc_out, dims$y, file_data$dimension_data$y[dimsteps_start[i]:(dimsteps_start[i] + dimsteps_count[i] - 1)],
      #           start = dimsteps_start[i], count = dimsteps_count[i])

    }
  }

  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose)
    message(get_processing_time_string(calc_time_start, calc_time_end))
}
