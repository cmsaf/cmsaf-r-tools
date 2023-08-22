#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a scatter plot of two variables.
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

cmsaf.scatter <- function(var1, infile1, var2, infile2, outfile1, outfile2, plot.out = FALSE, nc34 = 4, overwrite = FALSE, verbose = FALSE, toolbox = FALSE, nc1 = NULL, nc2 = NULL) {
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
  
  cmsafops::cmsaf.adjust.two.files(var1, infile1, var2, infile2, outfile1, outfile2, nc34, FALSE, FALSE, nc1 = nc1, nc2 = nc2)
  
  if(!toolbox) {
    id1 <- ncdf4::nc_open(outfile1)
    data1 <- try(ncdf4::ncvar_get(id1, var1, collapse_degen = FALSE))
    time <- try(ncdf4::ncvar_get(id1, "time"))
    t_unit <- ncdf4::ncatt_get(id1, "time", "units")$value
    date.time <- as.character(cmsafops::get_time(t_unit, time))
    
    id2 <- ncdf4::nc_open(outfile2)
    data2 <- try(ncdf4::ncvar_get(id2, var2, collapse_degen = FALSE))
    
    for(i in seq_along(time)) {
      if(plot.out) {
        plot_filepath <- dirname(outfile1)
        plot_filename <- paste0("cmsaf_scatter_plot_", date.time[i], ".png")
        if(file.exists(paste0(plot_filepath, "/", plot_filename))){
          unlink(paste0(plot_filepath, "/", plot_filename))
        }
        
        grDevices::png(paste0(plot_filepath, "/", plot_filename), width = 1024, height = 1024)
      }
      
      x <- as.data.frame(t(data1[,,i]))
      x <- data.frame(V1=unlist(x, use.names = FALSE))
    
      y <- as.data.frame(t(data2[,,i]))
      y <- data.frame(V1=unlist(y, use.names = FALSE))
      x <- as.data.frame(cbind(x,y))
      x <- x[stats::complete.cases(x), ]
      
      if(!(length(x[,1]) == 0)) {
        fudgeit <- function(){
          xm <- get('xm', envir = parent.frame(1))
          ym <- get('ym', envir = parent.frame(1))
          z  <- get('dens', envir = parent.frame(1))
          colramp <- get('colramp', parent.frame(1))
          fields::image.plot(xm,ym,z, col = colramp(256), legend.lab = "Density", legend.line=2.7, legend.only = T, add =F)
        }
        
        ## a different color scheme:
        #Lab.palette <- grDevices::colorRampPalette(c("white", "orange", "red"))
        #Lab.palette = grDevices::colorRampPalette(rev(rainbow(10, end = 4/6)))
        Lab.palette = grDevices::colorRampPalette(c("white","darkmagenta","orangered4","darkorange","goldenrod","gold"))
        graphics::par(mar = c(5,4,4,5) + .1)
        graphics::smoothScatter(x, colramp = Lab.palette,
                                nrpoints = 100,
                                ret.selection=TRUE,
                                xlab = var1,
                                pch = 19,
                                cex= 1,
                                ylab = var2, 
                                bandwidth=2,
                                postPlotHook = fudgeit)
        graphics::abline(0,1)
        if(plot.out)
          grDevices::dev.off()
      } else {
        warning(paste0("The data contains only NA values at timestemp ", date.time[i]))
      }
      
    }
    ncdf4::nc_close(id1)
    ncdf4::nc_close(id2)
  }
  calc_time_end <- Sys.time()
  if (verbose) message(cmsafops::get_processing_time_string(calc_time_start, calc_time_end))
}