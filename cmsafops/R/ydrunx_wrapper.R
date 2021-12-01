ydrunx_wrapper <- function(op, var, nts, infile, outfile, nc34, overwrite, verbose, 
                           nc = NULL) {
  calc_time_start <- Sys.time()
  
  check_variable(var)
  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)
  
  ##### extract data from file #####
  file_data <- read_file(infile, var, nc = nc)
  
  cmsaf_info <- switch(
    op,
    paste0("cmsafops::ydrunsum for variable ", file_data$variable$name),
    paste0("cmsafops::ydrunmean for variable ", file_data$variable$name),
    paste0("cmsafops::ydrunsd for variable ", file_data$variable$name),
  )

  switch(
    op,
    {
      if (verbose) message(paste0("apply multi-year daily running sum "))
      runsum(var = "SIS", nts = nts, infile = infile, outfile = file.path(tempdir(),"temp_ydrunsum.nc"), overwrite = TRUE, verbose = verbose, nc = nc)
      ydaysum(var = "SIS", infile = file.path(tempdir(),"temp_ydrunsum.nc"), outfile = outfile, overwrite = overwrite, verbose = verbose)
    },
    {
      if (verbose) message(paste0("apply multi-year daily running mean ", file_data$variable$name))
      runmean(var = "SIS", nts = nts, infile = infile, outfile = file.path(tempdir(),"temp_ydrunmean.nc"), overwrite = TRUE, verbose = verbose, nc = nc)
      ydaymean(var = "SIS", infile = file.path(tempdir(),"temp_ydrunmean.nc"), outfile = outfile, verbose = verbose)
    },
    {
      if (verbose) message(paste0("apply multi-year daily running standard deviation ", file_data$variable$name))
      runsd(var = "SIS", nts = nts, infile = infile, outfile = file.path(tempdir(),"temp_ydrunsd.nc"), overwrite = TRUE, verbose = verbose, nc = nc)
      ydaysd(var = "SIS", infile = file.path(tempdir(),"temp_ydrunsd.nc"), outfile = outfile, verbose = verbose)
    },
  )
  
  # change cmsaf_info attribute
  file.object <- nc_open(outfile, write = TRUE)
  ncatt_put(nc = file.object, varid = file_data$variable$name,
            attname = "cmsaf_info",
            attval = cmsaf_info, prec = PRECISIONS_ATT$TEXT, definemode = TRUE)
  nc_close(file.object)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}