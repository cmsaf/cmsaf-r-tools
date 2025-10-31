#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a histogram of two variables.
#' 
#' @param outfile Name of the outfile (NULL or character). Should match the fileExtension.
#' @param fileExtension The file extension of the image (character). Has to be one of the following: 'png', 'jpg', 'tif', 'kml', 'pdf'.
#' @param visualizeVariables A data frame containing all meta data for the plotting process (data.frame).
#' @param imagewidth Width of the image (numeric).
#' @param imageheight Height of the image (numeric).
#' @param text1_1d Title text (character).
#' @param text2_1d Text to be passed to graphics::mtext (character).
#' @param textsize Textsize to be used (cex).
#' @param legend_label1 Legend label of the first data set
#' @param legend_label2 Legend label of the second data set
#' @param timestep_1d_visualize Selected timestemp
#' 
#' @export
render_plot_hist_compare <- function(outfile = NULL,
                                fileExtension = ".png",
                                visualizeVariables,
                                imagewidth,
                                imageheight,
                                text1_1d,
                                text2_1d,
                                textsize,
                                legend_label1,
                                legend_label2,
                                timestep_1d_visualize) {
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  
  if(is.data.frame(visualizeVariables$data2)){   # second input file is a csv or RData file
    suppressWarnings({
      list_data_station <- list()
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
        
        A <- cbind(x=result_x, y=result_y)
        
        for (istation in seq_along(data_station)) {
          B <- cbind(x=c(lon_station[istation]), y=c(lat_station[istation]))
          tree <- SearchTrees::createTree(A)
          inds <- SearchTrees::knnLookup(tree, newdat=B, k=1)
          
          lon_coor <- A[inds,1]
          lat_coor <- A[inds,2]
          
          data_sat[istation] <- data_nc[which(lon == lon_coor),which(lat == lat_coor), which(date.time == timestep_1d_visualize)]
        }
        cd <- data.frame(data_sat, data_station, lon_station, lat_station)
      }

      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      
      # In the following textsize can be found in global.R
      ylab <- visualizeVariables$ylabel
      assertthat::assert_that(is.character(text1_1d))
      assertthat::assert_that(is.character(ylab))
      
      lo <- as.numeric(cd$lon_station)
      la <- as.numeric(cd$lat_station)
      st <- cd$data_station
      sa <- cd$data_sat
      st <- st[order(la)]
      sa <- sa[order(la)]
      lo <- lo[order(la)]
      la <- la[order(la)]
      xlabs <- NULL
      for (i in seq_along(st)) {
        dummy <- paste0("[", round(lo[i], digits = 1), ";", round(la[i], digits = 1), "]")
        xlabs <- append(xlabs, dummy)
      }
      rd <- rbind(st, sa)
      rownames(rd) <- c("R-Instat data", "Your data")
      graphics::par(mar = c(6, 5, 3, 2))
      graphics::barplot(rd,
                        beside = TRUE,
                        main = paste0("Comparison of ", text1_1d),
                        ylab = ylab,
                        names.arg = xlabs,
                        col = c(grDevices::rgb(0, 32, 91, maxColorValue = 255),
                                grDevices::rgb(242, 169, 0, maxColorValue = 255)),
                        las = 2)
      graphics::rect(graphics::par("usr")[1],
                     graphics::par("usr")[3],
                     graphics::par("usr")[2],
                     graphics::par("usr")[4],
                     col = "light grey")
      grid_col  <- "cornsilk2"
      graphics::grid(NULL,
                     NULL,
                     lty = 3,
                     col = grid_col,
                     lwd = 1.5)
      graphics::barplot(rd,
                        beside = TRUE,
                        ylab = ylab,
                        names.arg = xlabs,
                        col = c(
                          grDevices::rgb(0, 32, 91, maxColorValue = 255),
                          grDevices::rgb(242, 169, 0, maxColorValue = 255)),
                        las = 2,
                        add = TRUE,
                        legend.text = c(legend_label2, legend_label1))
      bordercolor <- "gray20"
      linesize <- 1.5
      graphics::box(col = bordercolor, lwd = linesize)
      
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    })
  } else {
    suppressWarnings({
      # In the following textsize can be found in global.R
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      
      data1 <- visualizeVariables$data
      data2 <- visualizeVariables$data2
      
      graphics::hist(data1[,,which(visualizeVariables$date.time == timestep_1d_visualize)], 
                     main = text1_1d, xlab = visualizeVariables$xlabel,
                     col = grDevices::rgb(91, 127, 149, maxColorValue = 255, alpha = 170), 
                     freq = TRUE)
      graphics::rug(data1[,,which(visualizeVariables$date.time == timestep_1d_visualize)], col = grDevices::rgb(91, 127, 149, maxColorValue = 255, alpha = 170), lwd = 2)
      graphics::hist(data2[,,which(visualizeVariables$date.time == timestep_1d_visualize)],
                     col = grDevices::rgb(230, 50, 50, maxColorValue = 255, alpha = 100), 
                     freq = TRUE, add = TRUE)
      graphics::rug(data2[,,which(visualizeVariables$date.time == timestep_1d_visualize)], col = grDevices::rgb(230, 50, 50, maxColorValue = 255, alpha = 100), lwd = 2)
      leg.txt <- c(legend_label1, legend_label2)
      graphics::legend("topright", leg.txt, pch = 15, 
                       col = c(grDevices::rgb(91, 127, 149, maxColorValue = 255, alpha = 170), 
                               grDevices::rgb(230, 50, 50, maxColorValue = 255, alpha = 100)),
                       cex = textsize)
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    })
  }
  return(
    list(
      src = outfile,
      contentType = getMimeType(outfile),
      width = iwidth,
      height = iheight,
      alt = "Histogram"
    )
  )
}