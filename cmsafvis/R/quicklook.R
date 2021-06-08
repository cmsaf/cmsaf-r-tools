#'Create a quicklook of NetCDF data
#'
#'The function creates a plot of the variables in NetCDF file(s) specified in the config file.
#'Only NetCDF files that conform to the [CMSAF naming convention](https://www.cmsaf.eu/EN/Products/NamingConvention/Naming_Convention_node.html) are supported.
#'
#'@param filelist list of NetCDF file to create plots from (character).
#'@param config filename of configuration file. This may include the directory
#'  (character).
#'@param outpath directory in which to save the output files.
#'  (character).
#'@param jpeg_quality jpeg quality for the image in percent, see [grDevices::jpeg()][grDevices::png()]
#'@param dpi resolution of the image in dots per inch, see [grDevices::jpeg()][grDevices::png()]
#'@param iwidth width of the resulting image in pixels, see [grDevices::jpeg()][grDevices::png()]
#'@param logo logical; should the cmsaf logo be added to the plot?
#'@param copyright logical; should the copyright text be added to the plot?
#'@param bluemarble logical; should the data be plotted onto a NASA bluemarble (only available for MSG/Seviri based data)?
#'   Due to data size this option is not available for the cmsafvis package on CRAN. Please have a look at
#'   our website https://www.cmsaf.eu/R_toolbox
#'
#'@return A jpeg file with the same name as the original NetCDF file.
#'@export
#'@importFrom assertthat assert_that is.count is.flag is.readable is.writeable

quicklook <- function(config,
                      filelist,
                      outpath = getwd(),
                      jpeg_quality = 75,
                      dpi = 150,
                      iwidth = 760,
                      logo = TRUE,
                      copyright = TRUE,
                      bluemarble = FALSE) {
  # Make sure that any user settings are reset when the function exits
  # This is a requirement by CRAN
  oldpar <- graphics::par(no.readonly = TRUE)
  # Warning: In graphics::par(oldpar) : par(new) ohne Plot aufgerufen
  on.exit(suppressWarnings(graphics::par(oldpar)))
  
  ### check parameters ###
  assert_that(is.string(config))
  assert_that(file.exists(normalizePath(config, mustWork = FALSE)))
  assert_that(!is.dir(config))
  assert_that(is.readable(config))
  
  assert_that(all(file.exists(filelist)))
  for (file_ in filelist) {
    assert_that(is.readable(file_))
  }
  
  assert_that(is.dir(outpath))
  assert_that(is.writeable(outpath))
  
  assert_that(is.count(jpeg_quality))
  assert_that(0 <= jpeg_quality && jpeg_quality <= 100)
  
  assert_that(is.count(dpi))
  assert_that(is.count(iwidth))
  assert_that(is.flag(logo))
  assert_that(is.flag(copyright))
  assert_that(is.flag(bluemarble))
  
  ### Build colorpalettes ###
  
  palettes <- GetPaletteConfig(gui = TRUE)
  names(palettes) <- tolower(names(palettes))
  names(palettes)[names(palettes) == "typ"] <- "type"
  
  # add more color schemes
  new_row <- data.frame("more", NA, NA, NA, NA, NA, NA, NA, NA, NA, 1)
  names(new_row) <- names(palettes)
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[75] <- "tim.colors"
  
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[76] <- "sunny"
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[77] <- "cloud_mask1"
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[78] <- "cloud_mask2"
  
  cloud_mask1 <- c("black", "transparent", "gray60", "white")
  cloud_mask2 <- c("black", "transparent", "gray60", "white", "pink")
  
  ### Read and format logo ###
  
  if (logo) {
    lf_black <- ifelse(logo == "color","CMSAF_NoName_Colour_crop.png","CMSAF_NoName_Black.png")
    
    logo_cmsaf_path_black <- system.file(
      "extdata",
      lf_black,
      package = "cmsafvis",
      mustWork = TRUE
    )
    
    # Size and location of the logo
    logo.scale_black <- 0.3
    
    logo.x <- 0
    logo.y <- 0
    
    logo_cmsaf_black <- png::readPNG(logo_cmsaf_path_black)
    
    dims_black <- dim(logo_cmsaf_black)[1:2]
    
    logo.height_black <- logo.scale_black * iwidth * dims_black[1] / dims_black[2]
    
    if (logo.scale_black * iwidth * dims_black[1] / dims_black[2] < 45) {
      logo.height_black <- 45
      logo.scale_black <- logo.height_black / dims_black[1] * dims_black[2] / iwidth
    }
    
    # Prepare color logo
    lf_color <- "CMSAF_NoName_Colour_crop.png"
    
    logo_cmsaf_path_color <- system.file(
      "extdata",
      lf_color,
      package = "cmsafvis",
      mustWork = TRUE
    )
    
    # Size and location of the logo
    logo.scale_color <- 0.3
    
    logo.x <- 0
    logo.y <- 0
    
    logo_cmsaf_color <- png::readPNG(logo_cmsaf_path_color)
    
    dims_color <- dim(logo_cmsaf_color)[1:2]
    
    logo.height_color <- logo.scale_color * iwidth * dims_color[1] / dims_color[2]
    
    if (logo.scale_color * iwidth * dims_color[1] / dims_color[2] < 45) {
      logo.height_color <- 45
      logo.scale_color <- logo.height_color / dims_color[1] * dims_color[2] / iwidth
    }
    
    text.x <- 0.99
    text.y <- 0.01
  }
  
  ### Read config file ###
  
  configParams <- yaml::read_yaml(config)
  ref_file <- filelist[1]
  file_info <- get_file_info(ref_file)
  
  varnames <- c()
  units <- c()
  
  plot_lim <- c()
  col_from_config <- c()
  legends <- c()
  logos <- c()
  slots <- c()
  
  # define plotting area in case of polar projection
  area <- ""
  if (grepl("North", file_info$area)) area <- "NP"
  if (grepl("South", file_info$area)) area <- "SP"
  if (grepl("Global", file_info$area)) area <- "GL"
  
  if (bluemarble && !file_info$grid == "Satellite projection MSG/Seviri") {
    stop("Bluemarble plotting is only available for CLAAS data on MSG grid.")
  }
  
  vars <- names(configParams[[file_info$product_type]][[file_info$id]])[names(configParams[[file_info$product_type]][[file_info$id]]) != "Dataset"]
  nvars <- length(vars)
  
  # no plot variables found
  assert_that(nvars > 0)  # TODO Improve error message so that user knows how to fix the problem.
  is_multiplot <- nvars > 1
  
  dataset_name <- configParams[[file_info$product_type]][[file_info$id]]$Dataset
  
  for (i in seq_along(vars)) {
    limits <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$limits
    plot_lim <- rbind(plot_lim, c(limits$min, limits$max))
    legends <- c(legends, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$legend)
    slots <- c(slots, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$slot)
    col_from_config <- c(col_from_config, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$colorscale)
    if (logo) logos <- c(logos, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$logo)
  }
  
  ### Read infiles ###
  
  nc    <- ncdf4::nc_open(ref_file)
  vars2 <- names(nc$var)[toupper(names(nc$var)) %in% vars]
  vars  <- vars2[order(match(toupper(vars),toupper(vars2)))]
  nvars <- length(vars)
  
  # no plot variables found
  assert_that(nvars > 0)  # TODO Improve error message so that user knows how to fix the problem.
  
  for (k in seq_along(vars)) {
    varnames <- c(varnames, ncdf4::ncatt_get(nc, vars[k], "long_name")$value)
    units <- c(units, ncdf4::ncatt_get(nc, vars[k], "units")$value)
  }
  
  if ("lon" %in% names(nc$dim)) {
    lon_min <- min(ncdf4::ncvar_get(nc, "lon"), na.rm = TRUE)
    lon_max <- max(ncdf4::ncvar_get(nc, "lon"), na.rm = TRUE)
    lat_min <- min(ncdf4::ncvar_get(nc, "lat"), na.rm = TRUE)
    lat_max <- max(ncdf4::ncvar_get(nc, "lat"), na.rm = TRUE)
  } else if (ncdf4::ncatt_get(nc, 0, "geospatial_lon_max")$hasatt) {
    lon_min <- ncdf4::ncatt_get(nc, 0, "geospatial_lon_min")$value
    lon_max <- ncdf4::ncatt_get(nc, 0, "geospatial_lon_max")$value
    lat_min <- ncdf4::ncatt_get(nc, 0, "geospatial_lat_min")$value
    lat_max <- ncdf4::ncatt_get(nc, 0, "geospatial_lat_max")$value
  } else {
    stop("unable to get a lon/lat reference")
  }
  
  if (area == "NP" | area == "SP") {
    lond <- ncdf4::ncvar_get(nc, "lon")
    latd <- ncdf4::ncvar_get(nc, "lat")
  }
  
  # get time info for all slots
  date.time   <- ncdf4::ncvar_get(nc,"time") 
  t_unit      <- ncdf4::ncatt_get(nc,"time","units")$value
  date.time   <- cmsafops::get_time(t_unit,date.time)
  date.time   <- date.time[slots]
  
  ncdf4::nc_close(nc)
  
  ### Set aspect ###
  
  lon_range <- lon_max - lon_min
  lat_range <- lat_max - lat_min
  
  aspect <- lon_range/lat_range
  
  if (area == "NP" | area == "SP") {
    iheight <- round(iwidth*0.94)
    } else if (area == "GL") {
      iheight <- round((iwidth/aspect)*0.98)
      } else {
          iheight <- round((iwidth/aspect)*0.95)
  }
  
  if (logo) {
    AR_color <- dims_color[1] / dims_color[2] * iwidth / iheight
    AR_black <- dims_black[1] / dims_black[2] * iwidth / iheight
  }
  
  # Prepare polar projection
  
  if (area == "NP" | area == "SP") {
    if (area == "NP") {
      ori  <- c(89.9, 0, 0)             #orientation North Pole
    }
    if (area == "SP") {
      ori  <- c(-89.9, 0, 0)             #orientation North Pole
    }
    nx <- dim(lond)[1]
    ny <- dim(lond)[2]
    landcol   <- "cornsilk3"     #"navajowhite3"
    oceancol  <- "cornsilk2"     #"cadetblue3"
    outcol    <- "cornsilk4"
    bordercol <- "gray"
  
    m <- maps::map("world", plot = FALSE)
  }
  
  ### Plot ###
  
  for (i in 1:length(filelist)) {
    # read data
    stacks <- c()
    for (k in seq_along(vars)) {
      stacks <- c(stacks, raster::stack(filelist[i], quick = TRUE, varname = vars[k]))
    }
    if (exists("nc_crs")) {
      for (l in seq_along(stacks)) {
        raster::crs(stacks[[l]]) <- nc_crs
      }
    }
    for (l in seq_along(stacks)) {
      raster::extent(stacks[[l]]) <- c(lon_min, lon_max, lat_min, lat_max)
    }
    
    
    # filename and timestamp for title
    filename <- unlist(strsplit(basename(filelist[i]), "\\."))
    outfile <- file.path(outpath, paste0(filename[1], ".jpg"))
    fi <- get_file_info(filename[1])
    if (is.null(slots[i])) {
      file_time <- fi$date_time
      slot_i <- i
    } else {
      file_time <- date.time[i]
      slot_i <- slots[i]
    }
    
    if (file_info$time_interval == "instantaneous")
      file_time <- format(file_time, "%Y-%m-%d %R")
    if (!is.null(file_info$statistics)){
      if (file_info$statistics == "mean diurnal cycle")
        file_time <- format(file_time, "%Y-%m-%d %R")
    }
    if (!is.null(file_info$statistics)){
      if (file_info$time_interval == "monthly" & file_info$statistics != "mean diurnal cycle")
        file_time <- format(file_time, "%Y-%m")
    } else {
      if (file_info$time_interval == "monthly")
        file_time <- format(file_time, "%Y-%m")
    }
    
    grDevices::jpeg(outfile,
                    quality = jpeg_quality,
                    width = iwidth,
                    height = iheight,
                    res = dpi,
                    pointsize = round(12 * (1/(dpi/72)))
    )
    
    # Parameters for multiple plots
    
    if (is_multiplot) {
      ncols <- ceiling(sqrt(nvars))
      nrows <- ceiling(nvars/ncols)
      graphics::par(mfrow = c(nrows, ncols))
    }
    
    graphics::par(mar = c(2, 4, 4, 6) + 0.1)
    graphics::par(oma = c(0, 0, 1, 1))
    
    for (j in seq_along(vars)) {
      
      # Set color palette
      if (col_from_config[[1]] == "clouds") {
        stacks[[j]][[i]][is.na(stacks[[j]][[i]])] <- 0
        if (raster::maxValue(stacks[[j]][[i]]) == 3) {
          col <- cloud_mask1
        } else {
          col <- cloud_mask2
        }
        plot_lim[j,] <- range(raster::values(stacks[[j]][[i]]),na.rm = TRUE)
      } else {
        col <- getColors(col_from_config[[j]], palettes, 32, FALSE)
      }
      
      # Polar Projection Plot
      if (area == "NP" | area == "SP") {
       
        rotate_cc <- function(x) {apply(t(x), 2, rev)}
        
        datav <- raster::as.matrix(stacks[[j]][[i]])
        # for some reason the data are mirrored; this has to be corrected
        datav <- rotate_cc(datav)
        if (area == "NP") {
          datav <- datav[dim(datav)[1]:1,dim(datav)[2]:1]
        }
        if (area == "SP") {
          datav <- datav[,dim(datav)[2]:1]
        }
        datav <- as.vector(datav)
        
        lonv  <- as.vector(lond)
        latv  <- as.vector(latd)
        
        a <-
          mapproj::mapproject(
            x = lonv,
            y = latv,
            projection = "orthographic",
            orientation = ori
          )

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
          abs(range(lonv, na.rm = TRUE)[1]) + abs(range(lonv, na.rm = TRUE)[2])
        yr <-
          abs(range(latv, na.rm = TRUE)[1]) + abs(range(latv, na.rm = TRUE)[2])
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
        
        fields::quilt.plot(
          a$x,
          a$y,
          datav,
          xlim = c(-0.7, 0.7),
          ylim = c(-0.7, 0.7),
          nx = nx / xf,
          ny = ny / yf,
          xlab = " ",
          ylab = " ",
          main = " ",
          col = col,
          add.legend = FALSE,
          axes = FALSE,
          asp = 1
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
            interior = FALSE,
            fill = TRUE,
            col = landcol,
            lwd = 0.5,
            resolution = 0,
            border = NA
          )
        )
        
        fields::quilt.plot(
          a$x,
          a$y,
          datav,
          xlim = c(-0.7, 0.7),
          ylim = c(-0.7, 0.7),
          nx = nx / xf,
          ny = ny / yf,
          xlab = " ",
          ylab = " ",
          main = "text1",
          col = col,
          add.legend = FALSE,
          axes = FALSE,
          add = TRUE,
          asp = 1
        )
        
        # Plot borders
        suppressWarnings(
          maps::map(
            "world",
            projection = "orthographic",
            orientation = ori,
            add = TRUE,
            interior = FALSE,
            col = outcol,
            lwd = 0.5,
            resolution = 0
          )
        )
        
        mapproj::map.grid(
          m,
          nx = 18,
          ny = 9,
          lty = 3,
          col = "gray",
          cex = 0.55
        )
      } else {
        
      # bluemarble plot
       if (bluemarble) {
         if (exists("blue_marble")) {
          raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
          fields::quilt.plot(
          # This is generated in data-raw/generate_internal_data.R
          blue_marble$projection$x,
          blue_marble$projection$y,
          blue_marble$data_values,
          xlim = c(-1, 1),
          ylim = c(-1, 1),
          nx = blue_marble$n_lon_unique / blue_marble$xf,
          ny = blue_marble$n_lat_unique / blue_marble$yf,
          xlab = " ",
          ylab = " ",
          main = "",
          col = blue_marble$colors,
          add.legend = FALSE,
          axes = FALSE
          )
        
          raster::image(stacks[[j]], y = slot_i,
          main = "",
          axes = FALSE,
          xlab = "",
          ylab = "",
          col = col,
          add = TRUE)
         } else {
           stop("Bluemarble plotting is not available. See https://www.cmsaf.eu/R_toolbox")
         }
       } else {
        # borderline plots for scale
        if (file_info$grid == "Satellite projection MSG/Seviri") {
          if (exists("blue_marble")) {
            graphics::par(pty = "s")
            raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
            fields::quilt.plot(
            # This is generated in data-raw/generate_internal_data.R
            blue_marble$projection$x,
            blue_marble$projection$y,
            blue_marble$data_values,
            xlim = c(-1, 1),
            ylim = c(-1, 1),
            nx = blue_marble$n_lon_unique / blue_marble$xf,
            ny = blue_marble$n_lat_unique / blue_marble$yf,
            xlab = " ",
            ylab = " ",
            col = "gray",
            add.legend = FALSE,
            axes = FALSE
            )
          } else {
              stop("Bluemarble plotting is not available. See https://www.cmsaf.eu/R_toolbox")
            }
        } else {
          graphics::image(lon_min:(lon_max*1.2),
                          lat_min:lat_max,
                          outer(lon_min:(lon_max*1.2),lat_min:lat_max,"+"),
                          main = "",
                          xlim = c(lon_min, lon_max),
                          ylim = c(lat_min, lat_max),
                          xlab = " ",
                          ylab = " ",
                          col = "gray",
                          axes = FALSE,
                          asp = 1
          )
        }
        
        # plot image
        raster::image(stacks[[j]], y = slot_i,
                      main = "",
                      xlim = c(lon_min, lon_max),
                      ylim = c(lat_min, lat_max),
                      axes = FALSE,
                      xlab = "",
                      ylab = "",
                      zlim = plot_lim[j,],
                      col = col,
                      colNA = "gray",
                      asp = 1,
                      add = TRUE
        )
        
        # borderline plot
        if (file_info$grid == "Satellite projection MSG/Seviri") {
          raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
          suppressWarnings(
            maps::map("world", projection = "orthographic", interior = FALSE, orientation = c(0,0,0), add = TRUE)
          )
        } else {
          maps::map("world", interior = FALSE, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), add = TRUE)
        }
       }
      } # end if polar projection 
      
      # plot logo and copyright text
      if (logo || copyright) {
        graphics::par(usr = c(0, 1, 0, 1))
        
        if (logos[j] == "color") {
          logo_cmsaf <- logo_cmsaf_color
          AR <- AR_color
          logo.scale <- logo.scale_color
          logo.height <- logo.height_color
        } else {
          logo_cmsaf <- logo_cmsaf_black
          AR <- AR_black
          logo.scale <- logo.scale_black
          logo.height <- logo.height_black
        }
      }
      if (logo) {
        graphics::rasterImage(array(0.75, dim = dim(logo_cmsaf)),
                              logo.x + 0.01,
                              logo.y + 0.01,
                              logo.x + logo.scale,
                              logo.y + (AR * logo.scale),
                              interpolate = TRUE,
                              bg = "white")
        
        graphics::rasterImage(logo_cmsaf,
                              logo.x + 0.01,
                              logo.y + 0.01,
                              logo.x + logo.scale,
                              logo.y + (AR * logo.scale),
                              interpolate = TRUE,
                              asp = 1
        )
      }
      if (copyright) {
        txt <- paste0("\u00a9 EUMETSAT, ", format.Date(Sys.Date(), "%Y"))
        cr.scale <- (logo.scale - 0.02)/graphics::strwidth(txt)
        dims_text <- c(round(dim(logo_cmsaf)[1]/2), dim(logo_cmsaf)[2], dim(logo_cmsaf)[3])
        graphics::rasterImage(array(0.75, dim = dim(logo_cmsaf)),
                              text.x - logo.scale,
                              text.y,
                              text.x,
                              text.y + ((AR * logo.scale)/2),
                              interpolate = TRUE)
        
        graphics::text(text.x, text.y + 0.01, txt,
                       cex = cr.scale,
                       adj = c(1,0))
      }
      
      # plot legend
      if (legends[j]) {
        raster::plot(stacks[[j]], y = slot_i,
                     main = "",
                     axes = FALSE,
                     xlab = "",
                     ylab = "",
                     zlim = plot_lim[j,],
                     legend.only = TRUE,
                     legend.shrink = 0.9,
                     legend.width = 1,
                     legend.mar = 5.1,
                     legend.args=list(text = units[j], 
                                      side = 2, 
                                      font = 2, 
                                      line = 0.2, 
                                      cex = 1.25),
                     col = col,
                     add = TRUE)
      }
      
      
      # figure title
      if (is_multiplot) graphics::mtext(CapWords(varnames[j]), line = 0.4, cex = 1.25)
      
    }
    
    # main title
    if (is_multiplot) {
      graphics::mtext(paste(dataset_name, file_time, sep = ", "),
                      side = 3,
                      line = -2,
                      outer = TRUE,
                      cex = 1.45,
                      
                      font = 2
      )
    } else {
      graphics::mtext(paste(CapWords(varnames[1]), dataset_name, file_time, sep = ", "),
                      side = 3,
                      line = -3,
                      
                      outer = TRUE,
                      cex = 1.45,
                      
                      font = 2
      )
    }
    grDevices::dev.off()
  }
  # Clean up 
  if (file.exists("Rplots.pdf")) {
    file.remove("Rplots.pdf")
  }
}