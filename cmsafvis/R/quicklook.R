#'Create a quicklook of NetCDF data
#'
#'The function creates a plot of the variables in NetCDF file(s) specified in the config file.
#'Only NetCDF files that conform to the [CM SAF naming convention](https://www.cmsaf.eu/EN/Products/NamingConvention/Naming_Convention_node.html) are supported.
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
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A jpeg file with the same name as the original NetCDF file.
#'@details This operator can be applied using a configuration file (quicklook_config.yml).
#'An example config file can be found in the extdata folder of this package. The following
#'parameters can be defined: 
#'\itemize{
#'  \item{logo: }{color / black}
#'  \item{slot: }{numeric (e.g., 13)}
#'  \item{invert_col: }{TRUE / FALSE}
#'  \item{Dataset: }{character (e.g., ICDR Seviri Radiation)}
#'  \item{limits: }{min: numeric; max: numeric}
#'  \item{legend: }{TRUE / FALSE}
#'  \item{colorscale: }{character (e.g., Viridis)}
#'  \item{unit: }{character (e.g., Percent / '%')}
#' }
#'@export
#'@importFrom assertthat assert_that is.count is.flag is.readable is.writeable

quicklook <- function(config,
                      filelist,
                      outpath = getwd(),
                      jpeg_quality = 100,
                      dpi = 150,
                      iwidth = 1242,
                      logo = TRUE,
                      copyright = TRUE,
                      bluemarble = FALSE,
                      verbose = TRUE) {
  # Make sure that any user settings are reset when the function exits
  # This is a requirement by CRAN
  oldpar <- graphics::par(no.readonly = TRUE)
  # Warning: In graphics::par(oldpar) : par(new) ohne Plot aufgerufen
  on.exit(suppressWarnings(graphics::par(oldpar)))
  
  # temporarly switch warnings off due to unwanted crs warnings
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))
  
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
  
  # define some global variables, which are part of the bluemarble data
  nc_crs <- blue_marble <- NULL
  
  ### Build colorpalettes ###
  
  palettes <- GetPaletteConfig(gui = TRUE)
  names(palettes) <- tolower(names(palettes))
  names(palettes)[names(palettes) == "typ"] <- "type"
  
  # add more color schemes
  new_row <- data.frame("more", NA, NA, NA, NA, NA, NA, NA, NA, NA, 1)
  names(new_row) <- names(palettes)
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[79] <- "tim.colors"
  
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[80] <- "sunny"
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[81] <- "cloud_mask1"
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[82] <- "cloud_mask2"
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[83] <- "larry"
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[84] <- "albedo"
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[85] <- "albedo2"
  
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
    xwidth <- iwidth
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
    
    lsb <- logo.scale_black
    lsc <- logo.scale_color
  }
  
  ind360 <- FALSE # indicator for longitude 0 to 360
 
  ### Read config file ###
  configParams <- yaml::read_yaml(config)
  
  # loop over files in filelist
  for (ifile in 1:length(filelist)) {
  
  suppressWarnings(reset_par()) # just to be sure
  invisible(grDevices::dev.off())
    
  plotfile <- filelist[ifile]   
  ref_file <- plotfile[1]
  file_info <- get_file_info(ref_file)
  iwidth <- xwidth 
  
  # check config entry
  if (sum(grepl(file_info$id, configParams)) == 1) {
    if (verbose) {
      cat("Config entry found for: ", basename(ref_file), sep = "")
      cat ("\n")
    }
  } else {
    if (verbose) {
      cat("!!! Warning !!! Config entry missing for: ", 
          basename(ref_file)," ", file_info$id, sep = "")
      cat ("\n")
    }
    next
  }
  
  varnames <- c()
  units <- c()
  
  plot_lim <- c()
  col_from_config <- c()
  legends <- c()
  logos <- c()
  slots <- c()
  invert <- c()
  set_unit <- c()
  
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
    set_unit <- c(set_unit, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$unit)
    iinvert <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$invert_col
    if (is.null(iinvert)) {
      iinvert <- FALSE
    }
    invert <- append(invert, iinvert)
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
    if (!is.null(set_unit)){
      if (!is.na(set_unit[k])){
        units[k] <- set_unit[k]
      }
    }
  }
  
  # check lon lat names (not elegant, but should work for now)
  lonvar <- "lon"
  latvar <- "lat"
  if ("lon" %in% names(nc$dim)) lonvar <- "lon" 
  if ("longitude" %in% names(nc$dim)) lonvar <- "longitude"
  if ("Longitude" %in% names(nc$dim)) lonvar <- "Longitude"
  if ("lat" %in% names(nc$dim)) latvar <- "lat"
  if ("latitude" %in% names(nc$dim)) latvar <- "latitude"
  if ("Latitude" %in% names(nc$dim)) latvar <- "Latitude"
  
  if (lonvar %in% names(nc$dim)) {
    lon_min <- min(ncdf4::ncvar_get(nc, lonvar), na.rm = TRUE)
    lon_max <- max(ncdf4::ncvar_get(nc, lonvar), na.rm = TRUE)
    lat_min <- min(ncdf4::ncvar_get(nc, latvar), na.rm = TRUE)
    lat_max <- max(ncdf4::ncvar_get(nc, latvar), na.rm = TRUE)
  } else if (ncdf4::ncatt_get(nc, 0, "geospatial_lon_max")$hasatt) {
    lon_min <- ncdf4::ncatt_get(nc, 0, "geospatial_lon_min")$value
    lon_max <- ncdf4::ncatt_get(nc, 0, "geospatial_lon_max")$value
    lat_min <- ncdf4::ncatt_get(nc, 0, "geospatial_lat_min")$value
    lat_max <- ncdf4::ncatt_get(nc, 0, "geospatial_lat_max")$value
  } else {
    stop("unable to get a lon / lat reference")
  }
  
  if (lon_max > 350) {
    ind360      <- TRUE  
    lon_max_org <- lon_max
    lon_min_org <- lon_min
    lon_max     <- 180 - lon_min_org
    lon_min     <- 180 - lon_max_org
  }
  
  if (area == "NP" | area == "SP") {
    lond <- ncdf4::ncvar_get(nc, lonvar)
    latd <- ncdf4::ncvar_get(nc, latvar)
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
      iheight <- round((iwidth/aspect)*1.04)
      } else {
          iheight <- round((iwidth/aspect)*0.95)
  }
  
  if (logo) {
    if (area == "NP" | area == "SP") {
      AR_color <- dims_color[1] / dims_color[2] * iwidth / iheight
      AR_black <- dims_black[1] / dims_black[2] * iwidth / iheight
    } else {
      AR_color <- dims_color[1] / dims_color[2] * lon_range / lat_range
      AR_black <- dims_black[1] / dims_black[2] * lon_range / lat_range
    }
  }
  
  if (logo) {
    if (area == "GL") {
      logo.scale_black <- 0.18
      logo.height_black <- logo.scale_black * iwidth * dims_black[1] / dims_black[2]
      
      if (logo.scale_black * iwidth * dims_black[1] / dims_black[2] < 30) {
        logo.height_black <- 30
        logo.scale_black <- logo.height_black / dims_black[1] * dims_black[2] / iwidth
      }
      
      logo.scale_color <- 0.18
      logo.height_color <- logo.scale_color * iwidth * dims_color[1] / dims_color[2]
      
      if (logo.scale_color * iwidth * dims_color[1] / dims_color[2] < 30) {
        logo.height_color <- 30
        logo.scale_color <- logo.height_color / dims_color[1] * dims_color[2] / iwidth
      }
    } else {
      logo.scale_black <- lsb
      logo.scale_color <- lsc
    } 
  }

  # factor for font size
  hcor <- (iheight / 750) - 1
  if (hcor < 0) {
    hcor <- 0
  }
  fsf2 <- iwidth / 1021
  fsf <- round((iwidth / 1021) + hcor, digits = 2)
  
  # Prepare polar projection
  
  if (area == "NP" | area == "SP") {
    if (area == "NP") {
      ori  <- c(89.9, 0, 0)             # orientation North Pole
    }
    if (area == "SP") {
      ori  <- c(-89.9, 0, 0)            # orientation South Pole
    }
    nx <- dim(lond)[1]
    ny <- dim(lond)[2]
    landcol   <- "gray75"     # "navajowhite3"
    oceancol  <- "gray85"     # "cadetblue3"
    outcol    <- "gray20"
    bordercol <- "gray20"
  
    m <- maps::map("world", plot = FALSE)
  }
  
  ### Plot ###
  
    # read data
    stacks <- c()
    for (k in seq_along(vars)) {
      stacks <- c(stacks, raster::stack(plotfile[1], quick = TRUE, varname = vars[k]))
    }
    if (!is.null(nc_crs)) {
      for (l in seq_along(stacks)) {
        raster::crs(stacks[[l]]) <- nc_crs
      }
    }
    for (l in seq_along(stacks)) {
      raster::extent(stacks[[l]]) <- c(lon_min, lon_max, lat_min, lat_max)
    }
    
    
    # filename and timestamp for title
    filename <- unlist(strsplit(basename(plotfile[1]), "\\."))
    outfile <- file.path(outpath, paste0(filename[1], ".jpg"))
    fi <- get_file_info(filename[1])
    if (is.null(slots[1])) {
      file_time <- fi$date_time
      slot_i <- 1
    } else {
      file_time <- date.time[1]
      slot_i <- slots[1]
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
      graphics::par(mar = c(2, 4, 4, 6) + 0.1)
      graphics::par(oma = c(0, 0, 2, 5))
    }  else if (area == "GL") {
        graphics::par(mar = c(2, 0, 4, 5) + 0.1)
        graphics::par(oma = c(0, 0, 1, 1))
    } else {
        graphics::par(mar = c(2, 4, 4, 7) + 0.1)
        graphics::par(oma = c(0, 0, 1, 2))
    }

    for (j in seq_along(vars)) {
      
      # Set color palette
      if (col_from_config[[1]] == "clouds") {
        stacks[[j]][[1]][is.na(stacks[[j]][[1]])] <- 0
        if (raster::maxValue(stacks[[j]][[1]]) == 3) {
          col <- cloud_mask1
        } else {
          col <- cloud_mask2
        }
        plot_lim[j,] <- range(raster::values(stacks[[j]][[1]]),na.rm = TRUE)
      } else {
        col <- getColors(col_from_config[[j]], palettes, 32, FALSE)
      }
      
      # invert colors if invert_col is TRUE
      if (!is.null(invert)) {
        if (invert[j]) {
          col <- rev(col)
        }
      }
      
      # Polar Projection Plot
      if (area == "NP" | area == "SP") {
       
        rotate_cc <- function(x) {apply(t(x), 2, rev)}
        
        datav <- raster::as.matrix(stacks[[j]][[1]])
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
          zlim = plot_lim[j,],
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
          zlim = plot_lim[j,],
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
          cex = 0.55*fsf
        )
      } else {
        
      # bluemarble plot
       if (bluemarble) {
         if (!is.null(blue_marble)) {
          raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
          fields::quilt.plot(
          # This is generated in data-raw/generate_internal_data.R
          blue_marble$projection$x,
          blue_marble$projection$y,
          blue_marble$data_values,
          xlim = c(-1, 1),
          ylim = c(-1, 1),
          zlim = plot_lim[j,],
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
          if (!is.null(blue_marble)) {
            graphics::par(pty = "s")
            raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
            fields::quilt.plot(
            # This is generated in data-raw/generate_internal_data.R
            blue_marble$projection$x,
            blue_marble$projection$y,
            blue_marble$data_values,
            xlim = c(-1, 1),
            ylim = c(-1, 1),
            zlim = plot_lim[j,],
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
          graphics::image((lon_min*0.998):(lon_max*1.002),
                          lat_min:lat_max,
                          outer((lon_min*0.998):(lon_max*1.002),lat_min:lat_max,"+"),
                          main = "",
                          xlim = c(lon_min, lon_max),
                          ylim = c(lat_min, lat_max),
                          xlab = " ",
                          ylab = " ",
                          col = "gray85",
                          axes = FALSE,
                          asp = 1
          )
        }
         
         # land plot
         if (file_info$grid == "Satellite projection MSG/Seviri") {
           raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
           suppressWarnings(
             maps::map("world", projection = "orthographic", fill = TRUE, border = NA, 
                       col = "gray75", orientation = c(0,0,0), add = TRUE)
           )
         } else if (area == "GL") {
           if (lon_max >= 359) {
             maps::map("world2", fill = TRUE, border = NA, xlim = c(lon_min, lon_max), 
                       col = "gray75", ylim = c(lat_min, lat_max), wrap = c(-180, 180), add = TRUE)
           } else {
             maps::map("world", fill = TRUE, border = NA, xlim = c(lon_min, lon_max), 
                       col = "gray75", ylim = c(lat_min, lat_max), wrap = c(-180, 180), add = TRUE)
           }
         } else {
           if (lon_max >= 359) {
             maps::map("world2", fill = TRUE, border = NA, xlim = c(lon_min, lon_max), 
                       col = "gray75", ylim = c(lat_min, lat_max), add = TRUE)
           } else {
             maps::map("world", fill = TRUE, border = NA, xlim = c(lon_min, lon_max), 
                       col = "gray75", ylim = c(lat_min, lat_max), add = TRUE)
           }
         }
        
        # plot image
        # UTH data with 0 to 360 grid were plotted wrong,
        # but the solution on Windows did not work on Linux
        if (ind360) {
        #  raster::image(raster::rotate(stacks[[j]]),
            raster::image(stacks[[j]], y = slot_i,
                        main = "",
                        xlim = c(lon_min, lon_max),
                        ylim = c(lat_min, lat_max),
                        axes = FALSE,
                        xlab = "",
                        ylab = "",
                        zlim = plot_lim[j,],
                        col = col,
                        colNA = "gray85",
                        asp = 1,
                        add = TRUE
          )
        } else {
            raster::image(stacks[[j]], y = slot_i,
                          main = "",
                          xlim = c(lon_min, lon_max),
                          ylim = c(lat_min, lat_max),
                          axes = FALSE,
                          xlab = "",
                          ylab = "",
                          zlim = plot_lim[j,],
                          col = col,
                          colNA = "gray85",
                          asp = 1,
                          add = TRUE
            )
        }
        
        # borderline plot
        if (file_info$grid == "Satellite projection MSG/Seviri") {
          raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
          suppressWarnings(
            maps::map("world", projection = "orthographic", interior = FALSE, 
                      col = "gray20", orientation = c(0,0,0), add = TRUE)
          )
        } else if (area == "GL") {
            if (lon_max >= 359) {
              maps::map("world2", interior = FALSE, xlim = c(lon_min, lon_max), 
                        col = "gray20", ylim = c(lat_min, lat_max), wrap = c(-180, 180), add = TRUE)
            } else {
                maps::map("world", interior = FALSE, xlim = c(lon_min, lon_max), 
                          col = "gray20", ylim = c(lat_min, lat_max), wrap = c(-180, 180), add = TRUE)
            }
        } else {
            if (lon_max >= 359) {
              maps::map("world2", interior = FALSE, xlim = c(lon_min, lon_max), 
                        col = "gray20", ylim = c(lat_min, lat_max), add = TRUE)
            } else {
                maps::map("world", interior = FALSE, xlim = c(lon_min, lon_max), 
                          col = "gray20", ylim = c(lat_min, lat_max), add = TRUE)
            }
        }
       }
      } # end if polar projection 
      
      # plot logo and copyright text
      if (logo || copyright) {
        # check figure min and max and calculate a correction
        xmin <- graphics::par("usr")[1]
        xmax <- graphics::par("usr")[2]
        ymin <- graphics::par("usr")[3]
        ymax <- graphics::par("usr")[4]
        
        xoff <- (1 / (abs(xmin) + abs(xmax))) * (abs(xmin - lon_min) + 1)
        yoff <- (1 / (abs(ymin) + abs(ymax))) * (abs(ymin - lat_min) + 1)
        
        if (xoff <= 0 || xoff >= 0.5) xoff <- 0
        if (yoff <= 0 || yoff >= 0.5) yoff <- 0
        
        graphics::par(usr = c(0, 1, 0, 1))
        
        if (logos[j] == "color") {
          logo_cmsaf <- logo_cmsaf_color
          AR <- AR_color
          logo.scale <- logo.scale_color
          logo.height <-
            (dims_color[1]/dims_color[2] *((abs(xmin) + abs(xmax)) * logo.scale)) / (abs(ymin) + abs(ymax))
        } else {
          logo_cmsaf <- logo_cmsaf_black
          AR <- AR_black
          logo.scale <- logo.scale_black
          logo.height <-
            (dims_black[1]/dims_black[2] *((abs(xmin) + abs(xmax)) * logo.scale)) / (abs(ymin) + abs(ymax))
        }
      }
      if (logo) {
        graphics::rasterImage(array(0.75, dim = dim(logo_cmsaf)),
                              logo.x + 0.01 + xoff,
                              logo.y + 0.01 + yoff,
                              logo.x + logo.scale + 0.01 + xoff,
                              logo.y + logo.height + 0.01 + yoff,
                              interpolate = TRUE,
                              bg = "white")
        
        graphics::rasterImage(logo_cmsaf,
                              logo.x + 0.01 + xoff,
                              logo.y + 0.01 + yoff,
                              logo.x + logo.scale + 0.01 + xoff,
                              logo.y + logo.height + 0.01 + yoff,
                              interpolate = TRUE
        )
      }
      if (copyright) {
        txt <- paste0("\u00a9 EUMETSAT, ", format.Date(Sys.Date(), "%Y"))
        cr.scale <- (logo.scale - 0.02)/graphics::strwidth(txt)
        dims_text <- c(round(dim(logo_cmsaf)[1]/2), dim(logo_cmsaf)[2], dim(logo_cmsaf)[3])
        graphics::rasterImage(array(0.75, dim = dim(logo_cmsaf)),
                              text.x - logo.scale - xoff,
                              text.y + yoff,
                              text.x - xoff,
                              text.y + ((AR * logo.scale)/2) + yoff,
                              interpolate = TRUE)
        
        graphics::text(text.x - xoff, text.y + 0.01 + yoff, txt,
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
                     legend.width = 1.5,
                     legend.mar = 5.1,
                     legend.args=list(text = units[j], 
                                      side = 2, 
                                      font = 2, 
                                      line = 0.2, 
                                      cex = 1.25*fsf),
                     axis.args=list(cex.axis = 1*fsf),
                     col = col,
                     add = TRUE)
      }
      
      
      # figure title
      if (is_multiplot) {
        graphics::mtext(CapWords(varnames[j]), line = 0.4, cex = 1.25*fsf2)
      }
      
    }
    
    # main title
    if (is_multiplot) {
      graphics::mtext(paste(dataset_name, file_time, sep = ", "),
                      side = 3,
                      line = -2,
                      outer = TRUE,
                      cex = 1.45*fsf2,
                      font = 2
      )
    } else {
      if (nchar(paste(CapWords(varnames[1]), dataset_name, file_time, sep = ", ")) > 70){
        fsf <- (fsf + fsf2) / 2
      }
      graphics::mtext(paste(CapWords(varnames[1]), dataset_name, file_time, sep = ", "),
                      side = 3,
                      line = -3,
                      outer = TRUE,
                      cex = 1.45*fsf,
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