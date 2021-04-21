#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a scatter plot of two variables.
#' 
#' @inheritParams render_plot
#' @param ticknumber Number of ticks (numeric).
#' @param dateformat Date format for constructing a date label.
#' @param text1_1d Title text (character).
#' @param text2_1d Text to be passed to graphics::mtext (character).
#' @param x_axis_label_1d x-label (first data set)
#' @param y_axis_label_1d y-label (second data set)
#' @param timestep_1d_visualize Selected timestemp
#'
#' @export
render_plot_scatter <- function(outfile = NULL,
                                fileExtension = ".png",
                                visualizeVariables,
                                dateformat,
                                ticknumber,
                                imagewidth,
                                imageheight,
                                text1_1d,
                                text2_1d,
                                textsize,
                                linesize,
                                x_axis_label_1d,
                                y_axis_label_1d,
                                timestep_1d_visualize) {
  suppressWarnings({
    if (is.null(outfile)) {
      outfile <- tempfile(fileext = fileExtension)
    }
    
    if(is.data.frame(visualizeVariables$data2)){   # second input file is a csv or RData file
      a <- visualizeVariables$data2
      data_nc <- visualizeVariables$data
      date.time <- visualizeVariables$date.time
      
      lon <- visualizeVariables$lon
      lat <- visualizeVariables$lat
      min_lon <- min(lon, na.rm = TRUE)
      max_lon <- max(lon, na.rm = TRUE)
      min_lat <- min(lat, na.rm = T)
      max_lat <- max(lat, na.rm = T)
      
      # lon
      slider1 <- c(max(round(as.numeric(min_lon)), -180), min(round(as.numeric(max_lon)), 180))
      
      # lat
      slider2 <- c(max(round(as.numeric(min_lat)), -90), min(round(as.numeric(max_lat)), 90))
      
      lo_dummy <- c("lon", "longitude", "laenge", "x", "lon_rep")
      la_dummy <- c("lat", "latitude", "breite", "y", "lat_rep")
      ti_dummy <- c("time", "date", "zeit", "t", "get_time.file_data.time_info.units..file_data.dimension_data.t.")
      da_dummy <- c("data", "daten", "z", "element", "result")
      
      dn <- attr(a, "element_name")
      if (!is.null(dn)) {
        da_dummy <- append(da_dummy, dn)
      } else {
        dn <- attr(a, "data_name")
        if (!is.null(dn)) {
          da_dummy <- append(da_dummy, dn)
        }
      }
      
      instat_names <- names(a)
      
      lo_n <- 0
      la_n <- 0
      ti_n <- 0
      da_n <- 0
      
      for (i in seq_along(instat_names)) {
        if (toupper(instat_names[i]) %in% toupper(lo_dummy)) (lo_n <- i)
        if (toupper(instat_names[i]) %in% toupper(la_dummy)) (la_n <- i)
        if (toupper(instat_names[i]) %in% toupper(ti_dummy)) (ti_n <- i)
        if (toupper(instat_names[i]) %in% toupper(da_dummy)) (da_n <- i)
      }
      
      if (lo_n > 0 & la_n > 0 & ti_n > 0 & da_n > 0) {
        # check monthly or daily
        # station
        time_station <- a[, ti_n]
        if (length(time_station) > 500) (time_station <- time_station[1:500])
        mon_station  <- format(as.Date(time_station), "%m")
        year_station <- format(as.Date(time_station), "%Y")
        day_station  <- format(as.Date(time_station), "%d")
        
        dummy <- which(mon_station == mon_station[1] & year_station == year_station[1])
        mmdm <- "d"
        
        if (length(unique(day_station[dummy])) == 1) {
          mmdm <- "m"
        }
        
        # satellite
        time_sat <- date.time
        if (length(time_sat) > 40) (time_sat <- time_sat[1:40])
        mon_sat  <- format(as.Date(time_sat), "%m")
        year_sat <- format(as.Date(time_sat), "%Y")
        day_sat  <- format(as.Date(time_sat), "%d")
        dummy <- which(mon_sat == mon_sat[1] & year_sat == year_sat[1])
        mmdm_sat <- "d"
        if (length(unique(day_sat[dummy])) == 1) {
          mmdm_sat <- "m"
        }
        
        # extract data for chosen time step
        if (mmdm == "m" & mmdm_sat == "m") {
          match_time   <- which(format(as.Date(a[, ti_n]), "%Y-%m") == format(as.Date(timestep_1d_visualize), "%Y-%m"), arr.ind = TRUE)
        } else {
          match_time   <- which(a[, ti_n] == timestep_1d_visualize, arr.ind = TRUE)
        }
        
        lon_station  <- a[, lo_n][match_time]
        lat_station  <- a[, la_n][match_time]
        data_station <- a[, da_n][match_time]
        
        # delete NAs
        dummy <- !is.na(data_station)
        data_station <- data_station[dummy]
        data_station <- data_station
        lon_station  <- lon_station[dummy]
        lat_station  <- lat_station[dummy]
        # Extract corresponding data points
        
        data_sat <- c(seq_along(data_station))
        
        result_x <- c()
        result_y <- c()
        
        result_x <- rep(lon, length(lat))
        
        for(j in seq_along(lat)){
          result_y <- append(result_y, rep(lat[j], length(lon)))
        }
        
        coor_sat <- cbind(x=result_x, y=result_y)
        A <- sp::SpatialPoints(coor_sat)
        
        for (istation in seq_along(data_station)) {
          B <- sp::SpatialPoints(cbind(x=c(lon_station[istation]), y=c(lat_station[istation])))
          tree <- SearchTrees::createTree(sp::coordinates(A))
          inds <- SearchTrees::knnLookup(tree, newdat=sp::coordinates(B), k=1)
          
          lon_coor <- coor_sat[inds,1]
          lat_coor <- coor_sat[inds,2]
          
          data_sat[istation] <- data_nc[which(lon == lon_coor),which(lat == lat_coor), which(date.time == timestep_1d_visualize)]
        }
        cd <- data.frame(data_sat, data_station, lon_station, lat_station)
      }
      
      xlabs <- NULL
      for (i in seq_along(lon_station)) {
        dummy <- paste0("[", round(lon_station[i], digits = 1), ";", round(lat_station[i], digits = 1), "]")
        xlabs <- append(xlabs, dummy)
      }
      row.names(cd) <- xlabs
      
      # In the following textsize, and linesize can be found in global.R
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
    
      x <- cbind(cd[1:length(cd[,1]),1], cd[1:length(cd[,1]),2])
      
      fudgeit <- function(){
        xm <- get('xm', envir = parent.frame(1))
        ym <- get('ym', envir = parent.frame(1))
        z  <- get('dens', envir = parent.frame(1))
        colramp <- get('colramp', parent.frame(1))
        fields::image.plot(xm,ym,z, col = colramp(256), legend.lab = "Density", legend.line=-2, legend.only = T, add =F)
      }
  
      ## a different color scheme:
      #Lab.palette <- colorRampPalette(c("white", "orange", "red"))
      #Lab.palette = grDevices::colorRampPalette(rev(rainbow(10, end = 4/6)))
      Lab.palette = grDevices::colorRampPalette(c("white","darkmagenta","orangered4","darkorange","goldenrod","gold"))
      graphics::par(mar = c(5,4,4,5) + .1)
      graphics::smoothScatter(x, colramp = Lab.palette,
                    nrpoints = 100,
                    ret.selection=TRUE,
                    pch = 19,
                    cex= 1,
                    xlab = x_axis_label_1d,
                    ylab = y_axis_label_1d,
                    postPlotHook = fudgeit,
                    main = text1_1d)
      graphics::text(x, labels = rownames(cd), cex = 0.7, pos = 4)
      graphics::abline(0,1)
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    } else {
      # In the following textsize, and linesize can be found in global.R
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      
      data1 <- visualizeVariables$data
      data2 <- visualizeVariables$data2
      
      x <- as.data.frame(t(data1[,,which(visualizeVariables$date.time == timestep_1d_visualize)]))
      x <- data.frame(V1=unlist(x, use.names = FALSE))
      y <- as.data.frame(t(data2[,,which(visualizeVariables$date.time == timestep_1d_visualize)]))
      y <- data.frame(V1=unlist(y, use.names = FALSE))
      x <- as.data.frame(cbind(x,y))
      x <- x[stats::complete.cases(x), ]
      
      if(!(length(x[,1]) == 0)) {
        fudgeit <- function(){
          xm <- get('xm', envir = parent.frame(1))
          ym <- get('ym', envir = parent.frame(1))
          z  <- get('dens', envir = parent.frame(1))
          colramp <- get('colramp', parent.frame(1))
          fields::image.plot(xm,ym,z, col = colramp(256), legend.lab = "Density", legend.line=-2, legend.only = T, add =F)
        }
        
        ## a different color scheme:
        #Lab.palette <- colorRampPalette(c("white", "orange", "red"))
        #Lab.palette = grDevices::colorRampPalette(rev(rainbow(10, end = 4/6)))
        Lab.palette = grDevices::colorRampPalette(c("white","darkmagenta","orangered4","darkorange","goldenrod","gold"))
        graphics::par(mar = c(5,4,4,5) + .1)
        graphics::smoothScatter(x, colramp = Lab.palette,
                      nrpoints = 100,
                      ret.selection=TRUE,
                      pch = 19,
                      cex= 1,
                      xlab = x_axis_label_1d,
                      ylab = y_axis_label_1d,
                      postPlotHook = fudgeit,
                      main = text1_1d)
        graphics::abline(0,1)
        graphics::mtext(text2_1d)
        on.exit(grDevices::dev.off())
      } else {
        stop(paste0("The data contains only NA values at timestemp ", timestep_1d_visualize))
      }
    }
  })
  return(
    list(
      src = outfile,
      contentType = getMimeType(outfile),
      width = iwidth,
      height = iheight,
      alt = "Scatterplot"
    )
  )
}