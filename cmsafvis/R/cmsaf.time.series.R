#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a time series plot of two variables.
#' 
#'@param var1 Name of the first NetCDF variable (character).
#'@param infile1 Filename of the first input NetCDF file. This may include the directory
#'  (character).
#'@param var2 Name of the second NetCDF variable (character).
#'@param infile2 Filename of the second input NetCDF file. This may include the directory
#'  (character).
#'@param outfile1 Filename of the first output NetCDF file. This may include the directory
#'  (character).
#'@param outfile2 Filename of the second output NetCDF file. This may include the directory
#'  (character).
#'@param plot.out logical; if TRUE, the plot will be stored in the same folder as outfile1. 
#'  If FALSE, the plot will not be saved.
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'@param toolbox logical; if TRUE, toolbox mode enabled. The two files are adjusted in space 
#'  and time so that they can be plotted.
#'@param nc1 Alternatively to \code{infile1} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'@param nc2 Alternatively to \code{infile2} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#'@return Two NetCDF files are written.
#'@export
#'
#'@family 1d visualization 
#'
cmsaf.time.series <- function(var1, infile1, var2, infile2, outfile1, outfile2, plot.out = FALSE, nc34 = 4, overwrite = FALSE, verbose = FALSE, toolbox = FALSE, nc1 = NULL, nc2 = NULL) {
  gc()
  calc_time_start <- Sys.time()
  
  if(overwrite){
    if(file.exists(outfile1)){
      unlink(outfile1)
    }
    if(file.exists(outfile2)){
      unlink(outfile2)
    }
  }
  
  temp_time_series_one <- file.path(tempdir(), "temp_time_series_one.nc")
  temp_time_series_two <- file.path(tempdir(), "temp_time_series_two.nc")
  
  # unlink two tmp files
  if(file.exists(temp_time_series_one)){
    unlink(temp_time_series_one)
  }
  if(file.exists(temp_time_series_two)){
    unlink(temp_time_series_two)
  }
  
  cmsafops::cmsaf.adjust.two.files(var1, infile1, var2, infile2, temp_time_series_one, temp_time_series_two, nc34, overwrite, FALSE, nc1 = nc1, nc2 = nc2)
  
  cmsafops::fldmean(var1, temp_time_series_one, outfile1, nc34, overwrite, FALSE)
  cmsafops::fldmean(var2, temp_time_series_two, outfile2, nc34, overwrite, FALSE)
  
  if(!toolbox) {
    id1 <- ncdf4::nc_open(outfile1)
    data1 <- try(ncdf4::ncvar_get(id1, var1, collapse_degen = FALSE))
    time <- try(ncdf4::ncvar_get(id1, "time"))
    t_unit <- ncdf4::ncatt_get(id1, "time", "units")$value
    date.time <- as.character(cmsafops::get_time(t_unit, time))
    date.time <- as.Date(date.time)
    id2 <- ncdf4::nc_open(outfile2)
    data2 <- try(ncdf4::ncvar_get(id2, var2, collapse_degen = FALSE))
    
    if(plot.out) {
      plot_filepath <- dirname(outfile1)
      plot_filename <- paste0("cmsaf_time_series_plot", ".png")
      if(file.exists(paste0(plot_filepath, "/", plot_filename))){
        unlink(paste0(plot_filepath, "/", plot_filename))
      }
      
      grDevices::png(paste0(plot_filepath, "/", plot_filename), width = 512, height = 512)
    }
    
    dum_tick <-
      seq(1, length(date.time), length.out = 5)
    dum_tick2 <- NULL
    for (j in 2:length(dum_tick)) {
      dummy <- seq(dum_tick[j - 1], dum_tick[j], length.out = 4)
      if (j > 2 & j != length(dum_tick))
        (dummy <- dummy[2:4])
      dum_tick2 <- append(dum_tick2, dummy)
    }
    dateformat <- 2
    date.lab <- switch(
      as.numeric(dateformat),
      format(date.time[dum_tick], "%Y"),
      format(date.time[dum_tick], "%Y-%m"),
      format(date.time[dum_tick], "%Y-%m-%d"),
      format(date.time[dum_tick], "%Y-%m-%d %R")
    )
    # data one
    graphics::plot(
      date.time,
      data1,
      type = "l",
      col = "white",
      main = "text1_1d",
      xlab = "time",
      ylab = "ylabel",
      axes = FALSE
    )
    graphics::par(new=TRUE)
    # data two
    graphics::plot(
      date.time,
      data2,
      #type = plotting_type[strtoi(checkGroup_type)],
      type = "l",
      #xlim = visualizeVariables$date.time[sliderx],
      #ylim = slidery,
      col = "white",
      main = "text1_1d",
      xlab = "time",
      ylab = "ylabel",
      axes = FALSE
    )
    
    graphics::abline(h = 0, lwd = 1, col = "gray")
    graphics::grid(NA, NULL, lwd = 0.8)
    graphics::abline(
      v = date.time,
      col = "lightgray",
      lty = 3,
      lwd = 0.8
    )
    
    # data one 
    graphics::points(
      date.time,
      data1,
      #type = plotting_type[strtoi(checkGroup_type)],
      type = "l",
      #xlim = visualizeVariables$date.time[sliderx],
      #ylim = slidery,
      #col = col,
      col = "blue",
      lwd = 1.5
    )
    
    # data two
    graphics::points(
      date.time,
      data2,
      #type = plotting_type[strtoi(checkGroup_type)],
      type = "l",
      #xlim = visualizeVariables$date.time[sliderx],
      #ylim = slidery,
      col = "red",
      lwd = 1.5
    )
    
    graphics::axis(
      side = 1,
      at = date.time[dum_tick],
      labels = date.lab,
      tck = -0.025,
      col.ticks = "gray20",
      col.axis = "gray20"
    )

    graphics::axis(
      side = 1,
      at = date.time[dum_tick],
      labels = FALSE,
      tck = 0.015,
      col.ticks = "gray20",
      col.axis = "gray20"
    )
    graphics::rug(
      x = date.time[dum_tick2],
      ticksize = 0.015,
      side = 1,
      quiet = TRUE
    )
    
    graphics::axis(
      side = 2,
      tck = -0.025,
      col.ticks = "gray20",
      col.axis = "gray20"
    )
    graphics::axis(
      side = 2,
      tck = 0.015,
      col.ticks = "gray20",
      col.axis = "gray20",
      labels = FALSE
    )
    
    graphics::box(col = "gray20", lwd = 1)
    #graphics::mtext(text2_1d)
    if(plot.out)
      grDevices::dev.off()
    
    ncdf4::nc_close(id1)
    ncdf4::nc_close(id2)
  }
  calc_time_end <- Sys.time()
  if (verbose) message(cmsafops::get_processing_time_string(calc_time_start, calc_time_end))
}