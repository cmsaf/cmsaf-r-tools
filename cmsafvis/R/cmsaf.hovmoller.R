#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a hovmoller plot of two variables.
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
cmsaf.hovmoller <- function(var1, infile1, var2, infile2, outfile1, outfile2, plot.out = FALSE, nc34 = 4, overwrite = FALSE, verbose = FALSE, toolbox = FALSE, nc1 = NULL, nc2 = NULL) {
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
  
  cmsafops::cmsaf.adjust.two.files(var1, infile1, var2, infile2, outfile1, outfile2, nc34, overwrite, FALSE, nc1 = nc1, nc2 = nc2)
  
  if(!toolbox) {
    id1 <- ncdf4::nc_open(outfile1)
    data1 <- try(ncdf4::ncvar_get(id1, var1, collapse_degen = FALSE))
    time <- try(ncdf4::ncvar_get(id1, "time"))
    t_unit <- ncdf4::ncatt_get(id1, "time", "units")$value
    date.time <- as.character(cmsafops::get_time(t_unit, time))
    
    id2 <- ncdf4::nc_open(outfile2)
    data2 <- try(ncdf4::ncvar_get(id2, var2, collapse_degen = FALSE))
    
    if(plot.out) {
      plot_filepath <- dirname(outfile1)
      plot_filename <- paste0("cmsaf_hovmoller_plot", ".png")
      if(file.exists(paste0(plot_filepath, "/", plot_filename))){
        unlink(paste0(plot_filepath, "/", plot_filename))
      }
      
      grDevices::png(paste0(plot_filepath, "/", plot_filename), width = 2048, height = 1024)
    }
    
    # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
    # regex1 <- regmatches(infile1, regexpr(pattern, infile1))
    # regex2 <- regmatches(infile2, regexpr(pattern, infile2))
    # Same as above and works for URLs
    regex1 <- get_basename_vis(infile1, nc = nc1)
    regex2 <- get_basename_vis(infile2, nc = nc2)
    
    graphics::par(cex = 1.2)
    
    stack_var_1 <- raster::stack(outfile1)
    stack_var_1_z <- raster::setZ(stack_var_1, date.time)
    
    stack_var_2 <- raster::stack(outfile1)
    stack_var_2_z <- raster::setZ(stack_var_2, date.time)
    
    x <- NULL
    y <- NULL
    p1 <- rasterVis::hovmoller(stack_var_1_z, dirXY=x, xlab='Longitude', main=paste(var1, regex1), par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
    p2 <- rasterVis::hovmoller(stack_var_1_z, dirXY=y, xlab='Latitude', main=paste(var1, regex1), par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
  
    p3 <- rasterVis::hovmoller(stack_var_2_z, dirXY=x, xlab='Longitude', main=paste(var2, regex2), par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
    p4 <- rasterVis::hovmoller(stack_var_2_z, dirXY=y, xlab='Latitude', main=paste(var2, regex2), par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
    gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
   
    if(plot.out)
      grDevices::dev.off()
    
    ncdf4::nc_close(id1)
    ncdf4::nc_close(id2)
  }
  calc_time_end <- Sys.time()
  if (verbose) message(cmsafops::get_processing_time_string(calc_time_start, calc_time_end))
}