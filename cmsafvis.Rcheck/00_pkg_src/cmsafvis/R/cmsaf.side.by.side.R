#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a side by side plot of two variables.
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
#'@family 2d visualization 
#'
cmsaf.side.by.side <- function(var1, infile1, var2, infile2, outfile1, outfile2, plot.out = FALSE, nc34 = 4, overwrite = FALSE, verbose = FALSE, toolbox = FALSE, nc1 = NULL, nc2 = NULL) {
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
    # data one
    id <- ncdf4::nc_open(outfile1)
    data <- try(ncdf4::ncvar_get(id, var1, collapse_degen = FALSE))
    time <- try(ncdf4::ncvar_get(id, "time"))
    t_unit <- ncdf4::ncatt_get(id, "time", "units")$value
    date.time <- as.character(cmsafops::get_time(t_unit, time))
    lon <- ncdf4::ncvar_get(id, "lon")
    lat <- ncdf4::ncvar_get(id, "lat")
    varname <- ncdf4::ncatt_get(id, var1, "long_name")$value
    
    if (varname == 0)
      (varname <- ncdf4::ncatt_get(id, var1, "standard_name")$value)
    if (varname == 0)
      (varname <- var1)
    text1 <- varname
    
    unit <- ncdf4::ncatt_get(id, var1, "units")$value
    if (unit == 0)
      (unit <- "-")
    text3 <- paste0(text1, " [", unit, "]")
    
    # data two
    id2 <- ncdf4::nc_open(outfile2)
    data2 <- try(ncdf4::ncvar_get(id2, var2, collapse_degen = FALSE))
    
    varname2 <- ncdf4::ncatt_get(id2, var2, "long_name")$value
    
    if (varname2 == 0)
      (varname2 <- ncdf4::ncatt_get(id2, var2, "standard_name")$value)
    if (varname == 0)
      (varname2 <- var2)
    text1_2 <- varname2
    
    unit2 <- ncdf4::ncatt_get(id2, var2, "units")$value
    if (unit2 == 0)
      (unit2 <- "-")
    text3_2 <- paste0(text1_2, " [", unit2, "]")
  
    for(i in seq_along(time)) {
      if(plot.out) 
      {
        plot_filepath <- dirname(outfile1)
        plot_filename <- paste0("cmsaf_side_by_side_plot_", date.time[i], ".png")
        if(file.exists(paste0(plot_filepath, "/", plot_filename)))
        {
          unlink(paste0(plot_filepath, "/", plot_filename))
        }
        grDevices::png(paste0(plot_filepath, "/", plot_filename), width = 1024, height = 512)
      }
      suppressWarnings({
        min_data1 <- min(data[,,i], na.rm = TRUE)
        max_data1 <- max(data[,,i], na.rm = TRUE)
        
        min_data2 <- min(data2[,,i], na.rm = TRUE)
        max_data2 <- max(data2[,,i], na.rm = TRUE)
        
        max_data <- max(c(max_data1, max_data2))
        min_data <- min(c(min_data1, min_data2))
      
        num_tick <- 5
        num_rmin <- min_data
        num_rmax <- max_data
  
        tlab <- break_num(
          ln = num_tick,
          bn = num_tick,
          minn = num_rmin,
          maxn = num_rmax,
          max_data = max_data
        )
        
        min_lon <- min(lon, na.rm = TRUE)
        max_lon <- max(lon, na.rm = TRUE)
        
        min_lat <- min(lat, na.rm = T)
        max_lat <- max(lat, na.rm = T)
        lon_bounds <- c(max(round(as.numeric(min_lon)), -180), min(round(as.numeric(max_lon)), 180))
        lat_bounds <- c(max(round(as.numeric(min_lat)), -90), min(round(as.numeric(max_lat)), 90))
        
        xtick  <- grDevices::axisTicks(lon_bounds, log = FALSE)
        ytick  <- grDevices::axisTicks(lat_bounds, log = FALSE)
        
        xlab <-
          unlist(lapply(xtick, function(x)
            ifelse(
              x < 0,
              paste0(abs(x), " W"), ifelse(x > 0, paste0(abs(x), " E"), x)
            )))
        ylab <-
          unlist(lapply(ytick, function(x)
            ifelse(
              x < 0, paste0(abs(x), " S"),
              ifelse(x > 0, paste0(abs(x), " N"), x)
            )))
        
        if (min(xtick) == round(lon_bounds[1])) {
          xlab[1] <- " "
        }
        if (max(xtick) == round(lon_bounds[2])) {
          xlab[length(xlab)] <- " "
        }
        if (min(ytick) == round(lat_bounds[1])) {
          ylab[1] <- " "
        }
        if (max(ytick) == round(lat_bounds[2])) {
          ylab[length(ylab)] <- " "
        }
        
        # default Color palette = "sunny"
        col <- c("#000000", "#120000", "#250101", "#380202", "#460202", "#530101", 
                 "#610000", "#710600", "#820D00", "#921500", "#A32300", "#B33300", 
                 "#C34400", "#D35500", "#E36500", "#F37600", "#FC8709", "#FD9719",
                 "#FEA82A", "#FFB83A", "#FFC94A", "#FFD95A", "#FFE76A", "#FFEF7A", 
                 "#FFF88B", "#FFFF9B", "#FFFFAB", "#FFFFBC", "#FFFFCC", "#FFFFDD", 
                 "#FFFFEE", "#FFFFFF")
        
        textsize <- 1.5
        graphics::par(cex = textsize)
        
        graphics::par(mar = c(2, 2, 2.6, 3))
        graphics::par(mfrow=c(1,2))
      
        # Plot with legend and title
        graphics::image(
          lon,
          lat,
          data[,,i],
          main = text1,
          xlab = " ",
          ylab = " ",
          xlim = lon_bounds,
          ylim = lat_bounds,
          zlim = c(num_rmin, num_rmax),
          col = col,
          axis.args = list(
            cex.axis = 1,
            at = as.numeric(tlab[tlab != ""]),
            labels = tlab[tlab != ""],
            mgp = c(1, 0.4, 0),
            tck = c(-0.3)
          ),
          #legend.lab = text3,
          #legend.line = -2,
          axes = FALSE
        )
        
        na.color <- "gray80"
        # linesize, bordercolor, plot_grid, and grid_col, na.color can be found in global.R
        graphics::image(
          lon,
          lat,
          array(1:2, dim(data[,,i])),
          xlab = " ",
          ylab = " ",
          col = na.color,
          axes = FALSE,
          xlim = lon_bounds,
          ylim = lat_bounds,
          add = TRUE
        )
        
        graphics::image(
          lon,
          lat,
          data[,,i],
          xlab = " ",
          ylab = " ",
          xlim = lon_bounds,
          ylim = lat_bounds,
          zlim = c(num_rmin, num_rmax),
          col = col,
          axes = FALSE,
          add = TRUE
        )
        
        bordercolor <- "gray20"
        linesize <- 1.5
        
        countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
        utils::data("countriesHigh", package = "rworldxtra", envir = environment())
        world_countries <- methods::as(countriesHigh, "SpatialLines")
        raster::plot(world_countries,
                     add = TRUE,
                     lwd = linesize,
                     col = bordercolor)
        
        grid_col <- "cornsilk2"
        graphics::grid(NULL, NULL, lty = 3, col = grid_col)
        
        # Add axes
        graphics::axis(
          1,                            # below the image
          mgp = c(0, -2.5, 0),          # label margins
          tck = c(0.01),                # tickmarks length
          col.axis = bordercolor,       # axis color
          cex.axis = 0.8 * textsize,    # label textsize
          at = xtick,                   # tickmarks positions
          labels = xlab                 # tickmarks labels
        )
        
        graphics::axis(
          2,                            # left side
          mgp = c(0, -2.5, 0),          # label margins
          tck = c(0.01),                # tickmarks length
          las = 1,                      # vertical orientation
          col.axis = bordercolor,       # axis color
          cex.axis = 0.8 * textsize,    # label textsize
          at = ytick,                   # tickmarks positions
          labels = ylab                 # tickmarks labels
        )
        
        graphics::box(col = bordercolor, lwd = linesize)
        # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
        # regex1 <- regmatches(infile1, regexpr(pattern, infile1))
        # Same as above and works for URLs
        regex1 <- get_basename_vis(infile1, nc = nc1)
        graphics::mtext(regex1)
        creator_att <- ncdf4::ncatt_get(id, 0, "creator_name")
        creator <- ifelse(creator_att$hasatt, creator_att$value, "-")
        copyrightText <- paste0("Data Source: ", creator)
        graphics::mtext(copyrightText,
                        side = 1,
                        adj = 1)
        
        # data two
        fields::image.plot(
          lon,
          lat,
          data2[,,i],
          main = text1_2,
          xlab = " ",
          ylab = " ",
          xlim = lon_bounds,
          ylim = lat_bounds,
          zlim = c(num_rmin, num_rmax),
          col = col,
          axis.args = list(
            cex.axis = 1,
            at = as.numeric(tlab[tlab != ""]),
            labels = tlab[tlab != ""],
            mgp = c(1, 0.4, 0),
            tck = c(-0.3)
          ),
          legend.lab = text3_2,
          legend.line = -1.6,
          legend.cex = 1,
          axes = FALSE
        )
        
        na.color <- "gray80"
        # linesize, bordercolor, plot_grid, and grid_col, na.color can be found in global.R
        graphics::image(
          lon,
          lat,
          array(1:2, dim(data2[,,i])),
          xlab = " ",
          ylab = " ",
          col = na.color,
          axes = FALSE,
          xlim = lon_bounds,
          ylim = lat_bounds,
          add = TRUE
        )
        
        graphics::image(
          lon,
          lat,
          data2[,,i],
          xlab = " ",
          ylab = " ",
          xlim = lon_bounds,
          ylim = lat_bounds,
          zlim = c(num_rmin, num_rmax),
          col = col,
          axes = FALSE,
          add = TRUE
        )
        
        
        
        bordercolor <- "gray20"
        linesize <- 1.5
        
        countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
        utils::data("countriesHigh", package = "rworldxtra", envir = environment())
        world_countries <- methods::as(countriesHigh, "SpatialLines")
        raster::plot(world_countries,
                     add = TRUE,
                     lwd = linesize,
                     col = bordercolor)
        
        grid_col <- "cornsilk2"
        graphics::grid(NULL, NULL, lty = 3, col = grid_col)
        
        
        # Add axes
        graphics::axis(
          1,                            # below the image
          mgp = c(0, -2.5, 0),          # label margins
          tck = c(0.01),                # tickmarks length
          col.axis = bordercolor,       # axis color
          cex.axis = 0.8 * textsize,    # label textsize
          at = xtick,                   # tickmarks positions
          labels = xlab                 # tickmarks labels
        )
        
        graphics::axis(
          2,                            # left side
          mgp = c(0, -2.5, 0),          # label margins
          tck = c(0.01),                # tickmarks length
          las = 1,                      # vertical orientation
          col.axis = bordercolor,       # axis color
          cex.axis = 0.8 * textsize,    # label textsize
          at = ytick,                   # tickmarks positions
          labels = ylab                 # tickmarks labels
        )
        
        graphics::box(col = bordercolor, lwd = linesize)
        
        # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
        # regex2 <- regmatches(infile2, regexpr(pattern, infile2))
        # Same as above and works for URLs
        regex2 <- get_basename_vis(infile2, nc = nc2)
        graphics::mtext(regex2)
        
        creator_att2 <- ncdf4::ncatt_get(id2, 0, "creator_name")
        creator2 <- ifelse(creator_att2$hasatt, creator_att2$value, "-")
        copyrightText2 <- paste0("Data Source: ", creator2)
        graphics::mtext(copyrightText2,
                        side = 1,
                        adj = 1)
      
      })
      
      if(plot.out)
        grDevices::dev.off()
    }
    ncdf4::nc_close(id)
  }
  calc_time_end <- Sys.time()
  if (verbose) message(cmsafops::get_processing_time_string(calc_time_start, calc_time_end))
}