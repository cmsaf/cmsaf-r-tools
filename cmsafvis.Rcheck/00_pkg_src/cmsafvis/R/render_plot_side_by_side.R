#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a 2D image of two files usually called by the CM SAF R Toolbox.
#'
#' @param plot_rinstat Whether to create an R-Instat plot (logical).
#' @param outfile Name of the outfile (NULL or character). Should match the fileExtension.
#' If NULL is passed a file is created in the R session temporary directory.
#' @param fileExtension The file extension of the image (character). Has to be one of the following: 'png', 'jpg', 'tif', 'kml', 'pdf'.
#' @param visualizeVariables A data frame containing all meta data for the plotting process (data.frame).
#' @param visualizeDataTimestep The data to be plotted.
#' @param nc_path_visualize The nc file path of which the plot is generated for.
#' @param visualizeDataMax Maximal data for computing breaks.
#' @param timestep_2d The time step to be visualized.
#' @param lon_bounds Array containing two values for longitude min and max (numeric).
#' @param lat_bounds Array containing two values for latitude min and max (numeric).
#' @param location Whether points specified by (lat_loc_vec, lon_loc_vec, name_loc_vec) should be added to the map (logical).
#' @param lon_loc_vec All longitude entries for points at (lat_loc_vec, lon_loc_vec) to be specified on the map (numeric).
#' @param lat_loc_vec All latitude entries for points at (lat_loc_vec, lon_loc_vec) to be specified on the map (numeric).
#' @param name_loc_vec Names for the points at (lat_loc_vec, lon_loc_vec) to be specified on the map (numeric).
#' @param num_tick Number of ticks (numeric).
#' @param num_rmin Color scale range minimum (numeric).
#' @param num_rmax Color scale range maximium (numeric).
#' @param num_brk Number of breaks (numeric).
#' @param co.data Data to be plotted in R-Instat mode (data.frame).
#' @param xort Centering the globe at longitude xort (numeric). Only in orthographic mode.
#' @param yort Centering the globe at latitude yort (numeric). Only in orthographic mode.
#' @param rort Rotation of the globe (numeric). Only in orthographic mode.
#' @param slider1 Controlling the horizontal plot position as vector of two values min and max (numeric).
#' @param slider2 Controlling the vertical plot position as vector of two values min and max (numeric).
#' @param imagewidth Width of the image (numeric).
#' @param imageheight Height of the image (numeric).
#' @param int Whether interior country borders should be added (logical).
#' @param text1 Title text data set 1 (character).
#' @param text2 Text to be passed to graphics::mtext for data set 1 (character).
#' @param text3 Text to be added to the legend (character).
#' @param PAL Color palette.
#' @param timestep The current timestep chosen.
#' @param proj The chosen projection (either 'rect' for rectangular or 'ortho' for orthographic).
#' @param plot_grid Whether to plot a grid using color grid_col (logical).
#' @param grid_col Color used for the grid.
#' @param bordercolor Color used for borders.
#' @param linesize Line width to be used (positive numeric).
#' @param reverse Whether to revert the color palette (logical).
#' @param na.color The color to be used for NA values.
#' @param textsize Textsize to be used (cex).
#' @param palettes Color palettes to be used.
#' @param text1_2 Title text data set 2 (character).
#' @param text2_2 Text to be passed to graphics::mtext for data set 2 (character).
#' 
#' @export
render_plot_side_by_side <- function(plot_rinstat,
                        outfile = NULL,
                        fileExtension = ".png",
                        visualizeVariables,
                        visualizeDataTimestep,
                        nc_path_visualize,
                        visualizeDataMax,
                        timestep_2d,
                        lon_bounds,
                        lat_bounds,
                        lon_loc_vec,
                        lat_loc_vec,
                        name_loc_vec,
                        timestep,
                        num_tick,
                        num_rmin,
                        num_rmax,
                        num_brk,
                        co.data,
                        proj,
                        xort,
                        yort,
                        rort,
                        slider1,
                        slider2,
                        imagewidth,
                        imageheight,
                        location,
                        int,
                        text1,
                        text2,
                        text3,
                        textsize,
                        bordercolor,
                        linesize,
                        na.color,
                        PAL,
                        palettes,
                        reverse,
                        plot_grid,
                        grid_col,
                        text1_2,
                        text2_2) {
  # A temp file to save the output.
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  if(is.data.frame(visualizeVariables$data2)){   # second input file is a csv or RData file
    suppressWarnings({
      tlab <- break_num(
        ln = num_tick,
        bn = num_tick,
        minn = num_rmin,
        maxn = num_rmax,
        max_data = visualizeDataMax
      )
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
      
      # If rectangular projection
      if (proj == "rect") {
        # Use colorspace pallete
        col <- getColors(
          PAL = PAL,
          palettes = palettes,
          num_brk = num_brk,
          reverse = reverse
        )
        
        iwidth  <- imagewidth
        iheight <- imageheight
        
        # Handle different files
        if (fileExtension == ".png") {
          grDevices::png(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".kml") {
          dta <- as.vector(visualizeDataTimestep)
          grd_dta <-
            cbind(expand.grid(visualizeVariables$lon, visualizeVariables$lat),
                  dta)
          ras <-
            raster::rasterFromXYZ(
              grd_dta,
              crs = sp::CRS("+proj=longlat +datum=WGS84"),
              digits = 1
            )
          
          kml_toolbox <- raster::rasterToPolygons(ras)
          
          check_package_dependency("plotKML", reason = "exporting KML files")

          plotKML::plotKML(
            kml_toolbox,
            file = outfile,
            kmz = FALSE,
            open.kml = FALSE,
            plot.labpt = FALSE,
            overwrite = TRUE,
            outline = 0
          )
           
          # cat("Sorry, but the plotKML R-package was removed from CRAN 
          #     and KML output is not possible at the moment.
          #     We are working on a solution for the next update.","\n")
          
        } else if (fileExtension == ".tif") {
          dta <- as.vector(visualizeDataTimestep)
          grd_dta <-
            cbind(expand.grid(visualizeVariables$lon, visualizeVariables$lat),
                  dta)
          ras <-
            raster::rasterFromXYZ(
              grd_dta,
              crs = sp::CRS("+proj=longlat +datum=WGS84"),
              digits = 1
            )
          ras_col <- raster::RGB(ras, col = col)
          
          check_package_dependency("rgdal", "exporting GeoTIFF files")
          raster::writeRaster(ras_col, filename = outfile, format = "GTiff")  # Requires package rgdal
        } else if (fileExtension == ".jpg") {
          grDevices::jpeg(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".pdf") {
          #needs different values (in inches) for width/height
          pdfWidth <- iwidth / 72
          pdfHeight <- iheight / 72
          grDevices::pdf(outfile, width = pdfWidth, height = pdfHeight)
        }
        
        graphics::par(cex = textsize)
        
        graphics::par(mar = c(2, 2, 2.6, 2))
      
        # Plot with legend and title
        fields::image.plot(
          visualizeVariables$lon,
          visualizeVariables$lat,
          visualizeDataTimestep,
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
          legend.lab = text3,
          legend.line = -2,
          axes = FALSE
        )
        
        # linesize, bordercolor, plot_grid, and grid_col, na.color can be found in global.R
        graphics::image(
          visualizeVariables$lon,
          visualizeVariables$lat,
          array(1:2, dim(visualizeDataTimestep)),
          xlab = " ",
          ylab = " ",
          col = na.color,
          axes = FALSE,
          xlim = lon_bounds,
          ylim = lat_bounds,
          add = TRUE
        )
        
        graphics::image(
          visualizeVariables$lon,
          visualizeVariables$lat,
          visualizeDataTimestep,
          xlab = " ",
          ylab = " ",
          xlim = lon_bounds,
          ylim = lat_bounds,
          zlim = c(num_rmin, num_rmax),
          col = col,
          axes = FALSE,
          add = TRUE
        )
        
        # Add borderlines or coastlines
        if (as.logical(int)) {
          countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
          utils::data("countriesHigh", package = "rworldxtra", envir = environment())
          world_countries <- methods::as(countriesHigh, "SpatialLines")
          raster::plot(world_countries,
                       add = TRUE,
                       lwd = linesize,
                       col = bordercolor)
        } else {
          #graphics::par(mfrow = c(1, 2))
          maps::map(
            "world",
            add = TRUE,
            interior = FALSE,
            resolution = 0,
            col = bordercolor,
            lwd = linesize
          )
        }
        
        # Add grid
        if (plot_grid) {
          graphics::grid(NULL, NULL, lty = 3, col = grid_col) #linetype dotted
        }
        
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
        
        # Add own location
        if (location) {
          if (length(lon_loc_vec) > 0 &&
              length(lon_loc_vec) == length(lat_loc_vec) &&
              length(lon_loc_vec) == length(name_loc_vec)) {
            for (i in seq_along(lon_loc_vec)) {
              graphics::points(lon_loc_vec[i],
                               lat_loc_vec[i],
                               pch = 16,
                               col = bordercolor)
              graphics::text(
                lon_loc_vec[i],
                lat_loc_vec[i],
                name_loc_vec[i],
                pos = 1,
                col = bordercolor,
                cex = textsize
              )
            }
          }
        }
        
        # Add subtitle and copyright tag
        graphics::mtext(text2)
        graphics::mtext(visualizeVariables$copyrightText,
                        side = 1,
                        adj = 1)
        
        ##### station data #####
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
            match_time   <- which(format(as.Date(a[, ti_n]), "%Y-%m") == format(as.Date(timestep_2d), "%Y-%m"), arr.ind = TRUE)
          } else {
            match_time   <- which(a[, ti_n] == timestep_2d, arr.ind = TRUE)
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
            
            data_sat[istation] <- data_nc[which(lon == lon_coor),which(lat == lat_coor), which(date.time == timestep_2d)]
          }
          cd <- data.frame(data_sat, data_station, lon_station, lat_station)
        }
        
        vec <- seq(num_rmin, num_rmax, length.out = num_brk + 1)
        data_station <- cd$data_station
        lon_station  <- cd$lon_station
        lat_station  <- cd$lat_station
        data_station[data_station >= num_rmax] <- num_rmax
        data_station[data_station <= num_rmin] <- num_rmin
        
        for (i in seq_along(data_station)) {
          point_col <-
            col[findInterval(data_station[i], vec, all.inside = TRUE)]
          graphics::points(
            lon_station[i],
            lat_station[i],
            pch = 21,
            bg = point_col,
            col = "gray30",
            cex = 3,
            lwd = 2
          )
        }
        
        on.exit(grDevices::dev.off())
      }
      ######################################
      # If orthographic projection
      if (proj == "ortho") {
        # prepare plot
        ori  <- c(xort, yort, rort)             #orientation
        nx <- length(visualizeVariables$lon)
        ny <- length(visualizeVariables$lat)
        landcol  <- "navajowhite3"
        oceancol <- "cadetblue3"
        outcol   <- "cornsilk4"
        
        rep.row <- function(x, n) {
          matrix(rep(x, each = n), nrow = n)
        }
        
        lonv  <-
          replicate(length(visualizeVariables$lat), visualizeVariables$lon)
        latv  <-
          rep.row(visualizeVariables$lat, length(visualizeVariables$lon))
        datav <-
          as.vector(visualizeDataTimestep)
        
        a <-
          mapproj::mapproject(
            x = lonv,
            y = latv,
            projection = "orthographic",
            orientation = ori
          )
        m <- maps::map("world", plot = FALSE)
        
        # filter Nas
        if (sum(is.na(a$x)) > 0 | sum(is.na(a$y)) > 0) {
          dummy <- NULL
          dummy <- !is.na(a$x)
          a$x   <- a$x[dummy]
          a$y   <- a$y[dummy]
          datav <- datav[dummy]
          dummy <- NULL
          dummy <- !is.na(a$y)
          a$x   <- a$x[dummy]
          a$y   <- a$y[dummy]
          datav <- datav[dummy]
        }
        
        # define grid factors
        xr <-
          abs(range(visualizeVariables$lon, na.rm = TRUE)[1]) + abs(range(visualizeVariables$lon, na.rm = TRUE)[2])
        yr <-
          abs(range(visualizeVariables$lat, na.rm = TRUE)[1]) + abs(range(visualizeVariables$lat, na.rm = TRUE)[2])
        l1 <- 3.1  # max value for nx/xf
        l2 <- 2.0  # max value for ny/yf
        
        x1 <- c(40, 360)
        y1 <- c(1, l1)
        c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
        c2 <- stats::lm(y1 ~ x1)$coeff[[2]]
        
        if (xr > 40 & xr <= 360) {
          xf <- c2 * xr + c1
          xf <- round(xf, digits = 1)
        } else {
          xf <- 1
        }
        
        x1 <- c(40, 180)
        y1 <- c(1, l2)
        c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
        c2 <- stats::lm(y1 ~ x1)$coeff[[2]]
        
        if (yr > 40 & yr <= 180) {
          yf <- c2 * yr + c1
          yf <- round(yf, digits = 1)
        } else {
          yf <- 1
        }
        
        iwidth  <- 800
        iheight <- 800
        
        graphics::par(mar = c(2, 2, 2.6, 2))
        
        # Get colors
        pcol <- getColors(
          PAL = PAL,
          palettes = palettes,
          num_brk = num_brk,
          reverse = reverse
        )
        
        
        # Plot orthographic image
        # Handle different files
        if (fileExtension == ".png") {
          grDevices::png(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".tif") {
          grDevices::tiff(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".jpg") {
          grDevices::jpeg(outfile, width = iwidth, height = iheight)
        }  else if (fileExtension == ".pdf") {
          #needs different values (in inches) for width/height
          pdfWidth <- iwidth / 72
          pdfHeight <- iheight / 72
          grDevices::pdf(outfile, width = pdfWidth, height = pdfHeight)
        }
        
        fields::quilt.plot(
          a$x,
          a$y,
          datav,
          xlim = c(-1, 1),
          ylim = c(-1, 1),
          zlim = c(num_rmin, num_rmax),
          nx = nx / xf,
          ny = ny / yf,
          xlab = " ",
          ylab = " ",
          main = text1,
          col = pcol,
          axis.args = list(
            cex.axis = 1,
            at = as.numeric(tlab[tlab != ""]),
            labels = tlab[tlab != ""],
            mgp = c(1, 0.4, 0),
            tck = c(-0.3)
          ),
          legend.lab = text3,
          legend.line = -2,
          axes = FALSE
        )
        
        graphics::polygon(
          sin(seq(0, 2 * pi, length.out = 100)),
          cos(seq(0, 2 * pi, length.out = 100)),
          col = oceancol,
          border = grDevices::rgb(1, 1, 1, 0.5),
          lwd = 1
        )
        suppressWarnings(
          maps::map(
            "world",
            projection = "orthographic",
            orientation = ori,
            add = TRUE,
            interior = FALSE
            ,
            fill = TRUE,
            col = landcol,
            lwd = linesize,
            resolution = 0,
            border = NA
          )
        )
        fields::quilt.plot(
          a$x,
          a$y,
          datav,
          xlim = c(-1, 1),
          ylim = c(-1, 1),
          zlim = c(num_rmin, num_rmax),
          nx = nx / xf,
          ny = ny / yf,
          xlab = " ",
          ylab = " ",
          main = text1,
          col = pcol,
          axis.args = list(
            cex.axis = 1,
            at = as.numeric(tlab[tlab != ""]),
            labels = tlab[tlab != ""],
            mgp = c(1, 0.4, 0),
            tck = c(-0.3)
          ),
          legend.lab = text3,
          legend.line = -2,
          axes = FALSE,
          add = TRUE
        )
        # Plot borders
        if (!as.logical(int)) {
          suppressWarnings(
            maps::map(
              "world",
              projection = "orthographic",
              orientation = ori,
              add = TRUE,
              interior = FALSE,
              col = outcol,
              lwd = linesize,
              resolution = 0
            )
          )
        } else {
          suppressWarnings(
            maps::map(
              "world",
              projection = "orthographic",
              orientation = ori,
              add = TRUE,
              interior = TRUE,
              col = bordercolor,
              lwd = linesize,
              resolution = 0
            )
          )
        }
        if (plot_grid) {
          mapproj::map.grid(
            m,
            nx = 18,
            ny = 9,
            lty = 3,
            col = grid_col,
            cex = linesize
          )
        }
        graphics::mtext(text2)
        graphics::mtext(visualizeVariables$copyrightText,
                        side = 1,
                        adj = 1)
        
        on.exit(grDevices::dev.off())
      }
    })
  } else {
    suppressWarnings({
      min_data1 <- min(visualizeDataTimestep, na.rm = TRUE)
      max_data1 <- max(visualizeDataTimestep, na.rm = TRUE)
      
      min_data2 <- min(visualizeVariables$data2[,,which(visualizeVariables$date.time == timestep_2d)], na.rm = TRUE)
      max_data2 <- max(visualizeVariables$data2[,,which(visualizeVariables$date.time == timestep_2d)], na.rm = TRUE)
      
      max_data <- max(c(max_data1, max_data2))
      min_data <- min(c(min_data1, min_data2))
      
      num_rmin <- min_data
      num_rmax <- max_data
      
      tlab <- break_num(
        ln = num_tick,
        bn = num_tick,
        minn = num_rmin,
        maxn = num_rmax,
        max_data = max_data
      )
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
      
      # If rectangular projection
      if (proj == "rect") {
        # Use colorspace pallete
        col <- getColors(
          PAL = PAL,
          palettes = palettes,
          num_brk = num_brk,
          reverse = reverse
        )
        
        iwidth  <- imagewidth * 2
        iheight <- imageheight
        
        # Handle different files
        if (fileExtension == ".png") {
          grDevices::png(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".kml") {
          dta <- as.vector(visualizeDataTimestep)
          grd_dta <-
            cbind(expand.grid(visualizeVariables$lon, visualizeVariables$lat),
                  dta)
          ras <-
            raster::rasterFromXYZ(
              grd_dta,
              crs = sp::CRS("+proj=longlat +datum=WGS84"),
              digits = 1
            )
          
          kml_toolbox <- raster::rasterToPolygons(ras)
          
          check_package_dependency("plotKML", reason = "exporting KML files")

          plotKML::plotKML(
            kml_toolbox,
            file = outfile,
            kmz = FALSE,
            open.kml = FALSE,
            plot.labpt = FALSE,
            overwrite = TRUE,
            outline = 0
          )
          
          # cat("Sorry, but the plotKML R-package was removed from CRAN 
          #     and KML output is not possible at the moment.
          #     We are working on a solution for the next update.","\n")
          
        } else if (fileExtension == ".tif") {
          dta <- as.vector(visualizeDataTimestep)
          grd_dta <-
            cbind(expand.grid(visualizeVariables$lon, visualizeVariables$lat),
                  dta)
          ras <-
            raster::rasterFromXYZ(
              grd_dta,
              crs = sp::CRS("+proj=longlat +datum=WGS84"),
              digits = 1
            )
          ras_col <- raster::RGB(ras, col = col)
          
          check_package_dependency("rgdal", "exporting GeoTIFF files")
          raster::writeRaster(ras, filename = outfile, format = "GTiff")
          # raster::writeRaster(ras_col, filename = outfile, format = "GTiff")  # Requires package rgdal
        } else if (fileExtension == ".jpg") {
          grDevices::jpeg(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".pdf") {
          #needs different values (in inches) for width/height
          pdfWidth <- iwidth / 72
          pdfHeight <- iheight / 72
          grDevices::pdf(outfile, width = pdfWidth, height = pdfHeight)
        }
        
        graphics::par(cex = textsize)
        
        graphics::par(mar = c(2, 2, 2.6, 3))
        graphics::par(mfrow = c(1,2))
        # Plot with legend and title
        graphics::image(
          visualizeVariables$lon,
          visualizeVariables$lat,
          visualizeDataTimestep,
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
       
        # linesize, bordercolor, plot_grid, and grid_col, na.color can be found in global.R
        graphics::image(
          visualizeVariables$lon,
          visualizeVariables$lat,
          array(1:2, dim(visualizeDataTimestep)),
          xlab = " ",
          ylab = " ",
          col = na.color,
          axes = FALSE,
          xlim = lon_bounds,
          ylim = lat_bounds,
          add = TRUE
        )
        
        graphics::image(
          visualizeVariables$lon,
          visualizeVariables$lat,
          visualizeDataTimestep,
          xlab = " ",
          ylab = " ",
          xlim = lon_bounds,
          ylim = lat_bounds,
          zlim = c(num_rmin, num_rmax),
          col = col,
          axes = FALSE,
          add = TRUE
        )
        
        # Add borderlines or coastlines
        if (as.logical(int)) {
          countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
          utils::data("countriesHigh", package = "rworldxtra", envir = environment())
          world_countries <- methods::as(countriesHigh, "SpatialLines")
          raster::plot(world_countries,
                       add = TRUE,
                       lwd = linesize,
                       col = bordercolor)
        } else {
          #graphics::par(mfrow = c(1, 2))
          maps::map(
            "world",
            add = TRUE,
            interior = FALSE,
            resolution = 0,
            col = bordercolor,
            lwd = linesize
          )
        }
        
        # Add grid
        if (plot_grid) {
          graphics::grid(NULL, NULL, lty = 3, col = grid_col) #linetype dotted
        }
        
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
        
        # Add own location
        if (location) {
          if (length(lon_loc_vec) > 0 &&
              length(lon_loc_vec) == length(lat_loc_vec) &&
              length(lon_loc_vec) == length(name_loc_vec)) {
            for (i in seq_along(lon_loc_vec)) {
              graphics::points(lon_loc_vec[i],
                               lat_loc_vec[i],
                               pch = 16,
                               col = bordercolor)
              graphics::text(
                lon_loc_vec[i],
                lat_loc_vec[i],
                name_loc_vec[i],
                pos = 1,
                col = bordercolor,
                cex = textsize
              )
            }
          }
        }
        
        # Add subtitle and copyright tag
        graphics::mtext(text2)
        graphics::mtext(visualizeVariables$copyrightText,
                        side = 1,
                        adj = 1)
        
        # data two
        fields::image.plot(
          visualizeVariables$lon,
          visualizeVariables$lat,
          visualizeVariables$data2[,,which(visualizeVariables$date.time == timestep_2d)],
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
          legend.lab = text3,
          legend.line = -1.6,
          legend.cex = 1,
          axes = FALSE
        )
    
        # linesize, bordercolor, plot_grid, and grid_col, na.color can be found in global.R
        graphics::image(
          visualizeVariables$lon,
          visualizeVariables$lat,
          array(1:2, dim(visualizeVariables$data2[,,which(visualizeVariables$date.time == timestep_2d)])),
          xlab = " ",
          ylab = " ",
          col = na.color,
          axes = FALSE,
          xlim = lon_bounds,
          ylim = lat_bounds,
          add = TRUE
        )
    
        graphics::image(
          visualizeVariables$lon,
          visualizeVariables$lat,
          visualizeVariables$data2[,,which(visualizeVariables$date.time == timestep_2d)],
          xlab = " ",
          ylab = " ",
          xlim = lon_bounds,
          ylim = lat_bounds,
          zlim = c(num_rmin, num_rmax),
          col = col,
          axes = FALSE,
          add = TRUE
        )
    
        # Add borderlines or coastlines
        if (as.logical(int)) {
          countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
          utils::data("countriesHigh", package = "rworldxtra", envir = environment())
          world_countries <- methods::as(countriesHigh, "SpatialLines")
          raster::plot(world_countries,
                       add = TRUE,
                       lwd = linesize,
                       col = bordercolor)
        } else {
          graphics::par(mfrow = c(1, 2))
          maps::map(
            "world",
            add = TRUE,
            interior = FALSE,
            resolution = 0,
            col = bordercolor,
            lwd = linesize
          )
        }
    
        # Add grid
        if (plot_grid) {
          graphics::grid(NULL, NULL, lty = 3, col = grid_col) #linetype dotted
        }
    
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
    
        # Add own location
        if (location) {
          if (length(lon_loc_vec) > 0 &&
              length(lon_loc_vec) == length(lat_loc_vec) &&
              length(lon_loc_vec) == length(name_loc_vec)) {
            for (i in seq_along(lon_loc_vec)) {
              graphics::points(lon_loc_vec[i],
                               lat_loc_vec[i],
                               pch = 16,
                               col = bordercolor)
              graphics::text(
                lon_loc_vec[i],
                lat_loc_vec[i],
                name_loc_vec[i],
                pos = 1,
                col = bordercolor,
                cex = textsize
              )
            }
          }
        }
    
        # Add subtitle and copyright tag
        graphics::mtext(text2_2)
        graphics::mtext(visualizeVariables$copyrightText,
                        side = 1,
                        adj = 1)
        
        
        on.exit(grDevices::dev.off())
      }
      ######################################
      # If orthographic projection
      if (proj == "ortho") {
        # prepare plot
        ori  <- c(xort, yort, rort)             #orientation
        nx <- length(visualizeVariables$lon)
        ny <- length(visualizeVariables$lat)
        landcol  <- "navajowhite3"
        oceancol <- "cadetblue3"
        outcol   <- "cornsilk4"
        
        rep.row <- function(x, n) {
          matrix(rep(x, each = n), nrow = n)
        }
        
        lonv  <-
          replicate(length(visualizeVariables$lat), visualizeVariables$lon)
        latv  <-
          rep.row(visualizeVariables$lat, length(visualizeVariables$lon))
        datav <-
          as.vector(visualizeDataTimestep)
        
        a <-
          mapproj::mapproject(
            x = lonv,
            y = latv,
            projection = "orthographic",
            orientation = ori
          )
        m <- maps::map("world", plot = FALSE)
        
        # filter Nas
        if (sum(is.na(a$x)) > 0 | sum(is.na(a$y)) > 0) {
          dummy <- NULL
          dummy <- !is.na(a$x)
          a$x   <- a$x[dummy]
          a$y   <- a$y[dummy]
          datav <- datav[dummy]
          dummy <- NULL
          dummy <- !is.na(a$y)
          a$x   <- a$x[dummy]
          a$y   <- a$y[dummy]
          datav <- datav[dummy]
        }
        
        # define grid factors
        xr <-
          abs(range(visualizeVariables$lon, na.rm = TRUE)[1]) + abs(range(visualizeVariables$lon, na.rm = TRUE)[2])
        yr <-
          abs(range(visualizeVariables$lat, na.rm = TRUE)[1]) + abs(range(visualizeVariables$lat, na.rm = TRUE)[2])
        l1 <- 3.1  # max value for nx/xf
        l2 <- 2.0  # max value for ny/yf
        
        x1 <- c(40, 360)
        y1 <- c(1, l1)
        c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
        c2 <- stats::lm(y1 ~ x1)$coeff[[2]]
        
        if (xr > 40 & xr <= 360) {
          xf <- c2 * xr + c1
          xf <- round(xf, digits = 1)
        } else {
          xf <- 1
        }
        
        x1 <- c(40, 180)
        y1 <- c(1, l2)
        c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
        c2 <- stats::lm(y1 ~ x1)$coeff[[2]]
        
        if (yr > 40 & yr <= 180) {
          yf <- c2 * yr + c1
          yf <- round(yf, digits = 1)
        } else {
          yf <- 1
        }
        
        iwidth  <- 800
        iheight <- 800
        
        graphics::par(mar = c(2, 2, 2.6, 2))
        
        # Get colors
        pcol <- getColors(
          PAL = PAL,
          palettes = palettes,
          num_brk = num_brk,
          reverse = reverse
        )
        
        
        # Plot orthographic image
        # Handle different files
        if (fileExtension == ".png") {
          grDevices::png(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".tif") {
          grDevices::tiff(outfile, width = iwidth, height = iheight)
        } else if (fileExtension == ".jpg") {
          grDevices::jpeg(outfile, width = iwidth, height = iheight)
        }  else if (fileExtension == ".pdf") {
          #needs different values (in inches) for width/height
          pdfWidth <- iwidth / 72
          pdfHeight <- iheight / 72
          grDevices::pdf(outfile, width = pdfWidth, height = pdfHeight)
        }
        
        fields::quilt.plot(
          a$x,
          a$y,
          datav,
          xlim = c(-1, 1),
          ylim = c(-1, 1),
          zlim = c(num_rmin, num_rmax),
          nx = nx / xf,
          ny = ny / yf,
          xlab = " ",
          ylab = " ",
          main = text1,
          col = pcol,
          axis.args = list(
            cex.axis = 1,
            at = as.numeric(tlab[tlab != ""]),
            labels = tlab[tlab != ""],
            mgp = c(1, 0.4, 0),
            tck = c(-0.3)
          ),
          legend.lab = text3,
          legend.line = -2,
          axes = FALSE
        )
        
        graphics::polygon(
          sin(seq(0, 2 * pi, length.out = 100)),
          cos(seq(0, 2 * pi, length.out = 100)),
          col = oceancol,
          border = grDevices::rgb(1, 1, 1, 0.5),
          lwd = 1
        )
        suppressWarnings(
          maps::map(
            "world",
            projection = "orthographic",
            orientation = ori,
            add = TRUE,
            interior = FALSE
            ,
            fill = TRUE,
            col = landcol,
            lwd = linesize,
            resolution = 0,
            border = NA
          )
        )
        fields::quilt.plot(
          a$x,
          a$y,
          datav,
          xlim = c(-1, 1),
          ylim = c(-1, 1),
          zlim = c(num_rmin, num_rmax),
          nx = nx / xf,
          ny = ny / yf,
          xlab = " ",
          ylab = " ",
          main = text1,
          col = pcol,
          axis.args = list(
            cex.axis = 1,
            at = as.numeric(tlab[tlab != ""]),
            labels = tlab[tlab != ""],
            mgp = c(1, 0.4, 0),
            tck = c(-0.3)
          ),
          legend.lab = text3,
          legend.line = -2,
          axes = FALSE,
          add = TRUE
        )
        # Plot borders
        if (!as.logical(int)) {
          suppressWarnings(
            maps::map(
              "world",
              projection = "orthographic",
              orientation = ori,
              add = TRUE,
              interior = FALSE,
              col = outcol,
              lwd = linesize,
              resolution = 0
            )
          )
        } else {
          suppressWarnings(
            maps::map(
              "world",
              projection = "orthographic",
              orientation = ori,
              add = TRUE,
              interior = TRUE,
              col = bordercolor,
              lwd = linesize,
              resolution = 0
            )
          )
        }
        if (plot_grid) {
          mapproj::map.grid(
            m,
            nx = 18,
            ny = 9,
            lty = 3,
            col = grid_col,
            cex = linesize
          )
        }
        graphics::mtext(text2)
        graphics::mtext(visualizeVariables$copyrightText,
                        side = 1,
                        adj = 1)
        
        on.exit(grDevices::dev.off())
      }
    })
  }
  # Return a list containing the filename
  return(
    list(
      src = outfile,
      contentType = getMimeType(fileExtension),
      width = iwidth,
      height = iheight,
      alt = "Side by side plot",
      position = c(slider1[1], slider2[2], slider1[2], slider2[1])
    )
  )
}
