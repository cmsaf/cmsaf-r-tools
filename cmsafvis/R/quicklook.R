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
#'@param maxpixels logical; use actual number of pixels or default (100000) 
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
#' \item logo: color / black
#' \item slot: numeric (e.g., 13)
#' \item invert_col: TRUE / FALSE
#' \item Dataset: character (e.g., ICDR Seviri Radiation)
#' \item limits: min: numeric; max: numeric
#' \item legend: TRUE / FALSE
#' \item colorscale: character (e.g., Viridis)
#' \item unit: character (e.g., Percent / '%')
#' \item var_name: character (e.g., Percent / '%')
#' \item bluemarble: TRUE / FALSE
#' \item mirror_data: TRUE / FALSE / NP / SP
#' \item namin: numeric (e.g., 2e-04), minimum in case of no data
#' \item logmin: numeric (e.g., 0.2), minimum in case of log-scale
#' \item scale_factor: numeric (e.g., 1)
#' \item smooth_factor: numeric (e.g., 1)
#' \item aux_file: path to optional aux-file, including CLAAS level 2 lon/ lat data
#' \item sysd: path to optional sysdata.rda file, which includes bluemarble data
#' \item remap: remap data to regular grid. TRUE / FALSE
#' \item tri_up: plot upper triangle on colorscale. TRUE / FALSE
#' \item tri_down: plot lower triangle on colorscale. TRUE / FALSE
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
                      maxpixels = TRUE,
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
  
  ### Define helper function: Code from R-package plotrix 3.8-2
  ### Plotrix: a package in the red light district of R
  ### Author: J. Lemon
  getYmult <- function() {
    if(grDevices::dev.cur() == 1) {
      warning("No graphics device open.")
      ymult <- 1
    }
    else {
      # get the plot aspect ratio
      xyasp <- graphics::par("pin")
      # get the plot coordinate ratio
      xycr <- diff(graphics::par("usr"))[c(1,3)]
      ymult <- xyasp[1]/xyasp[2]*xycr[2]/xycr[1]
    }
    return(ymult)
  }
  
  draw.circle <- function(x, y, radius, nv=100, border=NULL, col=NA,
                          lty=1, density=NULL, angle=45, lwd = 1) {
    
    xylim <- graphics::par("usr")
    plotdim <- graphics::par("pin")
    ymult <- getYmult()
    angle.inc <- 2*pi/nv
    angles <- seq(0,2*pi-angle.inc, by = angle.inc)
    if(length(col) < length(radius)) 
      col <- rep(col, length.out = length(radius))
    for(circle in 1:length(radius)) {
      xv <- cos(angles)*radius[circle]+x
      yv <- sin(angles)*radius[circle]*ymult+y
      graphics::polygon(xv, yv, border = border, col = col[circle], lty = lty,
                        density = density, angle = angle, lwd = lwd)
    }
    invisible(list(x = xv, y = yv))
  }
  ### End of code from plotrix R-package
  
  
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
  cloud_mask2 <- c("transparent", "transparent", "gray60", "white", "white")
  
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
  bluemarbles <- c()
  
  plot_lim <- c()
  col_from_config <- c()
  legends <- c()
  logos <- c()
  slots <- c()
  invert <- c()
  set_unit <- c()
  set_vname <- c()
  marble <- c()
  sysd <- c()
  auxf <- c()
  mirror <- c()
  namin <- c()
  logmin <- c()
  logsc <- c()
  scalef <- c()
  smoothf <- c()
  tick_lab <- vector(mode = "list", length = 1)
  remap_q <- FALSE
  triup <- c()
  tridown <- c()
  
  # define plotting area in case of polar projection
  area <- ""
  if (grepl("North", file_info$area)) area <- "NP"
  if (grepl("South", file_info$area)) area <- "SP"
  if (grepl("Global", file_info$area)) area <- "GL"
  
  vars <- names(configParams[[file_info$product_type]][[file_info$id]])
  vars <- vars[!(vars %in% c("remap", "aux_file", "Dataset"))]
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
    marble <- c(marble, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$bluemarble)
    
    isysd <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$sysdata
    if (!is.null(isysd)) sysd <- isysd
    
    imap <- configParams[[file_info$product_type]][[file_info$id]]$remap
    if (!is.null(imap)) remap_q <- imap
    
    iauxf <- configParams[[file_info$product_type]][[file_info$id]]$aux_file
    if (!is.null(iauxf)) auxf <- iauxf
    
    iinvert <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$invert_col
    if (is.null(iinvert)) {
      iinvert <- FALSE
    }
    invert <- append(invert, iinvert)
    
    imirror <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$mirror_data
    if (is.null(imirror)) {
      imirror <- FALSE
    }
    mirror <- append(mirror, imirror)
    
    scfa <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$scale_factor
    if (is.null(scfa)) {
      scfa <- 1.0
    }
    scalef <- append(scalef, scfa)
    
    inamin <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$namin
    if (is.null(inamin)) {
      inamin <- NA
    }
    namin <- append(namin, inamin)
    
    ilogmin <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$logmin
    if (is.null(ilogmin)) {
      ilogmin <- NA
    }
    logmin <- append(logmin, ilogmin)
    
    tl <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$tick_lab
    if (is.null(tl)) {
      tl <- NA
    }
    tick_lab[[i]] <- tl
    
    smfa <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$smooth_factor
    if (is.null(smfa)) {
      smfa <- NA
    }
    smoothf <- append(smoothf, smfa)
    
    iunit <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$unit
    if (is.null(iunit)) {
      iunit <- NA
    }
    set_unit <- append(set_unit, iunit)
    
    ivname <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$var_name
    if (is.null(ivname)) {
      ivname <- NA
    }
    set_vname <- append(set_vname, ivname)

    ilogsc <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$log_scale
    if (is.null(ilogsc)) {
      ilogsc <- FALSE
    }
    logsc <- append(logsc, ilogsc)
    
    itriup <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$tri_up
    if (is.null(itriup)) {
      itriup <- FALSE
    }
    triup <- append(triup, itriup)
    
    itridown <- configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$tri_down
    if (is.null(itridown)) {
      itridown <- FALSE
    }
    tridown <- append(tridown, itridown)
    
    col_from_config <- c(col_from_config, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$colorscale)
    if (logo) logos <- c(logos, configParams[[file_info$product_type]][[file_info$id]][[vars[i]]]$logo)
  }
  
  mirror <- toupper(mirror)
  
  ### Read infiles ###
  
  nc    <- ncdf4::nc_open(ref_file)
  vars2 <- names(nc$var)[toupper(names(nc$var)) %in% toupper(vars)]
  vars  <- vars2[order(match(toupper(vars2),toupper(vars)))]
  nvars <- length(vars)
  
  # no plot variables found
  assert_that(nvars > 0)  # TODO Improve error message so that user knows how to fix the problem.
  
  for (k in seq_along(vars)) {
    varnames <- c(varnames, ncdf4::ncatt_get(nc, vars[k], "long_name")$value)
    units <- c(units, ncdf4::ncatt_get(nc, vars[k], "units")$value)
    bluemarbles <- c(bluemarbles, bluemarble)

    if (!is.null(set_unit)){
      if (!is.na(set_unit[k])){
        units[k] <- set_unit[k]
      }
    }
    
    if (!is.na(set_vname[k])){
      varnames[k] <- set_vname[k]
    }
    
    if (!is.null(marble)){
      if (!is.na(marble[k])){
        bluemarbles[k] <- marble[k]
      }
    }
    
    # define some global variables, which are part of the bluemarble data
    if (!is.null(sysd)) {
      if (file.exists(sysd)) {
        cat("Loading sysdata file...", "\n")
        load(sysd)
      } else {
        cat("WARNING, sysdata file not found!", "\n")
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
    
    dlon <- round(ncdf4::ncvar_get(nc, lonvar)[2] - ncdf4::ncvar_get(nc, lonvar)[1], digits = 2)
    dlat <- round(ncdf4::ncvar_get(nc, latvar)[2] - ncdf4::ncvar_get(nc, latvar)[1], digits = 2)
    
    lond <- ncdf4::ncvar_get(nc, lonvar)
    latd <- ncdf4::ncvar_get(nc, latvar)

    lonv  <- as.vector(lond)
    latv  <- as.vector(latd)
    lonv  <- seq(min(lonv), max(lonv), length.out = length(lonv))
    latv  <- seq(min(latv), max(latv), length.out = length(latv))
  } else {
      if (is.null(auxf) & area != "NP" & area != "SP") {
        stop("unable to get a lon / lat reference")
      }
  }
  
  if (ncdf4::ncatt_get(nc, 0, "geospatial_lon_max")$hasatt) {
    lon_min <- ncdf4::ncatt_get(nc, 0, "geospatial_lon_min")$value
    lon_max <- ncdf4::ncatt_get(nc, 0, "geospatial_lon_max")$value
    lat_min <- ncdf4::ncatt_get(nc, 0, "geospatial_lat_min")$value
    lat_max <- ncdf4::ncatt_get(nc, 0, "geospatial_lat_max")$value
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
  
  if (exists("blue_marble")) {
    nc_crs <- NULL  
  } else {
    nc_crs <- blue_marble <- NULL
  }

  # factor for font size
  hcor <- (iheight / 750) - 1
  if (hcor < 0) {
    hcor <- 0
  }
  fsf2 <- iwidth / 1021
  fsf <- round((iwidth / 1021) + hcor, digits = 2)
  
  # Prepare polar projection
  ori <- c(0, 0, 0)
  if (area == "NP" | area == "SP") {
    if (area == "NP") {
      ori  <- c(89.9, 0, 0)             # orientation North Pole
    }
    if (area == "SP") {
      ori  <- c(-89.9, 0, 0)            # orientation South Pole
    }
    nx <- dim(lond)[1]
    ny <- dim(lond)[2]
  }
   
    landcol   <- "gray75"     # "navajowhite3"
    oceancol  <- "gray85"     # "cadetblue3"
    outcol    <- "gray20"
    bordercol <- "gray20"
  
    m <- maps::map("world", plot = FALSE)
  
  ### Plot ###
  oldw <- getOption("warn")
  options(warn = -1)
    
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
    
    if (!is.null(auxf)) {
      if (file.exists(auxf)) {
        nc <- ncdf4::nc_open(auxf)
          clon <- ncdf4::ncvar_get(nc, "lon")
          clat <- ncdf4::ncvar_get(nc, "lat")
        ncdf4::nc_close(nc)
        
        gc()
        
        carray <- array(0, dim = dim(clon))
        carray <- carray[!is.na(clon)]
        clat <- clat[!is.na(clon)]
        clon <- clon[!is.na(clon)]
        
      } else {
          cat(paste("Couldn't find aux_file: ", auxf, sep=""), "\n")
      }
    }
    
  options(warn = oldw)
    
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
        file_time <- format(file_time, "%Y-%m %R")
    }
    if (!is.null(file_info$statistics)){
      if (file_info$time_interval == "monthly" & file_info$statistics != "mean diurnal cycle")
        file_time <- format(file_time, "%Y-%m")
    } else {
      if (file_info$time_interval == "monthly")
        file_time <- format(file_time, "%Y-%m")
    }
    
    if (is_multiplot && (ceiling(nvars/ceiling(sqrt(nvars))) == 1)) {
      grDevices::jpeg(outfile,
                    quality = jpeg_quality,
                    width = iwidth,
                    height = 2 * iheight,
                    res = dpi,
                    pointsize = round(12 * (1/(dpi/72)))
      )
    } else {
        grDevices::jpeg(outfile,
                      quality = jpeg_quality,
                      width = iwidth,
                      height = iheight,
                      res = dpi,
                      pointsize = round(12 * (1/(dpi/72)))
        )
    }
    
    # Parameters for multiple plots
    
    if (is_multiplot) {
      ncols <- ceiling(sqrt(nvars))
      nrows <- ceiling(nvars/ncols)
      if (nrows == 1) {
        graphics::par(mfrow = c(ncols, nrows))
        graphics::par(mar = c(2, 0, 4, 5) + 0.1)
        graphics::par(oma = c(0, 0, 1, 1))
      } else {
        graphics::par(mfrow = c(nrows, ncols))
        graphics::par(mar = c(2, 4, 4, 6) + 0.1)
        graphics::par(oma = c(0, 0, 2, 5))
      }
      graphics::par(family = "Liberation Sans")
    }  else if (area == "GL") {
        graphics::par(mar = c(2, 0, 4, 5) + 0.1)
        graphics::par(oma = c(0, 0, 1, 1))
        graphics::par(family = "Liberation Sans")
    } else {
        graphics::par(mar = c(2, 4, 4, 7) + 0.1)
        graphics::par(oma = c(0, 0, 1, 2))
        graphics::par(family = "Liberation Sans")
    }

    for (j in seq_along(vars)) {
      
      # Check bluemarble 
      if (bluemarbles[j] && !file_info$grid == "Satellite projection MSG/Seviri") {
        stop("Bluemarble plotting is only available for CLAAS data on MSG grid.")
      }
      
      # Get raster dimensions and set maxpixels
      figdim <- dim(stacks[[j]])
      
      maxp <- 100000
      
      if (maxpixels) {
        maxp <- figdim[1] * figdim[2]
      } else {
        maxp <- 500000
      }
      
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
        if (logsc[j]) {
          get_breaks <- function(x) {
            lo <- floor(log10(min(x, na.rm = TRUE)))
            hi <- ceiling(log10(max(x, na.rm = TRUE)))
            as.vector(10 ^ (lo:hi) %o% 1:9)
          }
          
          logzero <- FALSE
          tick_lim <- plot_lim[j,]
          
          if (plot_lim[j,1] == 0) {
            datav <- raster::as.matrix(stacks[[j]][[slot_i]])
            if (all(is.na(datav))) {
              if (!is.na(namin[j])){
                minval <- as.numeric(namin[j])
              } else {
                  minval <- 0.001
              }
            } else {
                if (!is.na(logmin[j])){
                  minval <- logmin[j]
                } else {
                    minval <- min(datav[datav > 0], na.rm = TRUE)
                }
            }
            tick_lim[1] <- minval
            logzero <- TRUE
            ticks <- get_breaks(tick_lim)
            ticks_labs <- ticks 
            zeroval <- (0 + minval) / 2
            if (ticks[1] < minval) {
              zeroval <- ticks[1]
              ticks_labs[1] <- "0"
            } else if (!is.na(logmin[j])) {
                ticks <- append(zeroval, ticks)
                ticks_labs <- append("0", ticks_labs)
            } else {
                zeroval <- ticks[1] - (0.9 * ticks[1])
                ticks <- append(zeroval, ticks)
                ticks_labs <- append("0", ticks_labs)
            }
            
            # adjust plot_lim
            plot_lim[j,1] <- zeroval
            
          } else {
              ticks <- get_breaks(plot_lim[j,])
              ticks_labs <- ticks
          }
          
          col = getColors(col_from_config[[j]], palettes, length(ticks) - 1L, FALSE)
        } else {
            if (!is.na(tick_lab[[j]][1])) {
              col = getColors(col_from_config[[j]], palettes, length(tick_lab[[j]]), FALSE)
            } else {
                col <- getColors(col_from_config[[j]], palettes, 32, FALSE)
                if (col_from_config[[j]] == "terrain.colors" && remap_q) {
                  col <- col[1:30]
                }
            }
        }
      }
      
      # invert colors if invert_col is TRUE
      if (!is.null(invert)) {
        if (invert[j]) {
          col <- rev(col)
        }
      }
      
      # remap data if remap_q = TRUE
      if (remap_q) {
        if (!is.null(auxf)) {
          if (file.exists(auxf)) {
            rdata <- c()
            if (verbose) {
              cat("Remapping...", "\n")
            }
            rdata <- remap4quicklook(var = vars[j], infile = plotfile, auxfile = auxf)
            rdata$data <- rdata$data * scalef[j]
            if (file_info$grid == "Satellite projection MSG/Seviri" || file_info$grid == "Remapped") {
              file_info$grid <- "Remapped"
              
              rep.row <- function(x, n) {
                matrix(rep(x, each = n), nrow = n)
              }
              
              lon_l2  <-
                replicate(length(rdata$lat), rdata$lon)
              lat_l2  <-
                rep.row(rdata$lat, length(rdata$lon))
              
              datav <- as.vector(rdata$data)

              a <- mapproj::mapproject(
                  x = lon_l2,
                  y = lat_l2,
                  projection = "orthographic",
                  orientation = ori
                )
              
              nx <- figdim[1]
              ny <- figdim[2]
              
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
                abs(range(lon_l2, na.rm = TRUE)[1]) + abs(range(lon_l2, na.rm = TRUE)[2])
              yr <-
                abs(range(lat_l2, na.rm = TRUE)[1]) + abs(range(lat_l2, na.rm = TRUE)[2])
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
            }
          } else {
            cat("No aux-file found! This file is a requirement for remapping.", "\n")
          }
        }
      }
      
      # Polar Projection Plot
      if (area == "NP" || area == "SP") {

        nc <- ncdf4::nc_open(plotfile)
          datav <- ncdf4::ncvar_get(nc, vars[j])
        ncdf4::nc_close(nc)
        # Apply scale factor
        datav <- datav * scalef[j]

        if (!is.null(mirror)) {
          if (mirror[j] == "NP" | mirror[j] == "SP" | mirror[j] == "TRUE") {
            datav <- datav[dim(datav)[1]:1,]
          } 
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
       if (bluemarbles[j]) {
         if (!is.null(sysd)) {
           if (!file.exists(sysd)) {
              cat("No valid bluemarble data found!", "\n")
           } 
         }
         if (!is.null(blue_marble)) {
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
        
          raster::image(stacks[[j]] * scalef[j], y = slot_i,
          main = "",
          axes = FALSE,
          xlab = "",
          ylab = "",
          col = col,
          maxpixels = maxp,
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
              if (!is.null(auxf)) {
                if (file.exists(auxf)) {
                  graphics::par(pty = "s")
                  raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
                  fields::quilt.plot(
                    clon,
                    clat,
                    carray,
                    xlim = c(-1, 1),
                    ylim = c(-1, 1),
                    xlab = " ",
                    ylab = " ",
                    col = "gray85",
                    add.legend = FALSE,
                    axes = FALSE
                  )
                }
              } else {
                  stop("Bluemarble plotting is not available. No valid bluemarble data found! See https://www.cmsaf.eu/R_toolbox")
              }
            }
        } else {
          if (file_info$grid == "Remapped") {
            fields::quilt.plot(
              a$x,
              a$y,
              datav,
              xlim = c(-1, 1),
              ylim = c(-1, 1),
              zlim = plot_lim[j,],
              nx = nx / xf,
              ny = ny / yf,
              xlab = " ",
              ylab = " ",
              main = " ",
              col = col,
              add.legend = FALSE,
              axes = FALSE,
              useRaster = TRUE,
              asp = 1
            )
            
            graphics::polygon(
              sin(seq(0, 2 * pi, length.out = 100)),
              cos(seq(0, 2 * pi, length.out = 100)),
              col = oceancol,
              border = grDevices::rgb(1, 1, 1, 0.5),
              lwd = 1
            )
          } else {
            graphics::image(lonv, latv,
                            outer(lonv, latv, "+"),
                            main = "",
                            xlim = c(lon_min, lon_max),
                            ylim = c(lat_min, lat_max),
                            xlab = " ",
                            ylab = " ",
                            col = "gray85",
                            axes = FALSE,
                            useRaster = TRUE,
                            asp = 1
            )
          }
        }
         
         # land plot
         if (file_info$grid == "Satellite projection MSG/Seviri" || file_info$grid == "Remapped") {
           if (!is.null(auxf)) {
             raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
             suppressWarnings(
               maps::map("world", projection = "orthographic", fill = TRUE, border = NA,
                         col = "gray75", orientation = ori, boundary = FALSE, 
                         xlim = c(min(clon), max(clon)), 
                         ylim = c(min(clat), max(clat)),
                         lforce = "e",
                         add = TRUE)
             )
           } else {
               raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
               suppressWarnings(
               maps::map("world", projection = "orthographic", fill = TRUE, border = NA,
                         col = "gray75", orientation = c(0,0,0), add = TRUE)
               )
           }
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
                       col = "gray75", ylim = c(lat_min, lat_max), lforce = "e", add = TRUE)
           } else {
             maps::map("world", fill = TRUE, border = NA, xlim = c(lon_min, lon_max),
                       col = "gray75", ylim = c(lat_min, lat_max), lforce = "e", add = TRUE)
           }
         }
        
        # plot image
        # In case of values outside plot_lim, marked by a triangle, set min and max
        if (triup[j]){
          raster::values(stacks[[j]])[raster::values(stacks[[j]]) > plot_lim[j,2]] <- plot_lim[j,2]
          
          if (!exists("datav")){
            datav <- raster::as.matrix(stacks[[j]][[slot_i]])
          }
          datav[datav > plot_lim[j,2]] <- plot_lim[j,2]
        }
         
        if (tridown[j]){
          raster::values(stacks[[j]])[raster::values(stacks[[j]]) < plot_lim[j,1]] <- plot_lim[j,1]
         
          if (!exists("datav")){
            datav <- raster::as.matrix(stacks[[j]][[slot_i]])
          }
          datav[datav < plot_lim[j,1]] <- plot_lim[j,1]
        }
         
        # Get subset of raster stack, which contains data to plot
         plotdata <- raster::subset(stacks[[j]], slot_i)
        
         # UTH data with 0 to 360 grid were plotted wrong,
        # but the solution on Windows did not work on Linux
        if (ind360) {
        #  raster::image(raster::rotate(stacks[[j]]),
            raster::image(plotdata * scalef[j],
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
                        maxpixels = maxp,
                        add = TRUE
          )
        } else {
            if (logsc[j] && !remap_q) {

              rotate <- function(x) t(apply(x, 2, rev))
              
              if (!exists("datav")){
                datav <- raster::as.matrix(stacks[[j]][[slot_i]])
              }
              
              # For log-scale values below or equal 0 are not allowed 
              if (logzero) {
                if (!is.na(logmin[j])) {
                  datav[datav >= 0 & datav < logmin[j]] <- zeroval
                } else {
                    datav[datav <= 0] <- zeroval
                }
              }
              
              graphics::image(lonv, latv, log(rotate(datav)), 
                              axis.args=list( at=log(ticks), labels=ticks), 
                              main = "",
                              xlim = c(lon_min, lon_max),
                              ylim = c(lat_min, lat_max),
                              zlim = log(plot_lim[j,]),
                              axes = FALSE,
                              xlab = "",
                              ylab = "",
                              col = col,
                              colNA = "gray85",
                              asp = 1,
                              useRaster = TRUE,
                              add = TRUE
              )
              
            } else {
                if (!is.na(smoothf[j])) {
                  smp <- raster::disaggregate(stacks[[j]], fact = smoothf[j], method='bilinear')
                  
                  raster::image(smp,
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
                    if (remap_q && !is.null(auxf)) {
                      if (logsc[j]) {
                        
                        # For log-scale values below or equal 0 are not allowed 
                        if (logzero) {
                          datav[datav <= 0] <- zeroval
                        }
                        
                        fields::quilt.plot(
                          a$x,
                          a$y,
                          log(datav),
                          xlim = c(-1, 1),
                          ylim = c(-1, 1),
                          zlim = log(plot_lim[j,]),
                          nx = nx / xf,
                          ny = ny / yf,
                          xlab = " ",
                          ylab = " ",
                          main = "text1",
                          col = col,
                          add.legend = FALSE,
                          axes = FALSE,
                          add = TRUE,
                          useRaster = TRUE,
                          asp = 1
                        )
                      } else {
                          fields::quilt.plot(
                            a$x,
                            a$y,
                            datav,
                            xlim = c(-1, 1),
                            ylim = c(-1, 1),
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
                            useRaster = TRUE,
                            asp = 1
                          )
                      }
                    } else {
                        raster::image(plotdata * scalef[j],
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
                                    maxpixels = maxp,
                                    add = TRUE
                      )
                    }
                }
                
            }
        }
        
        # borderline plot
        if (file_info$grid == "Satellite projection MSG/Seviri" || file_info$grid == "Remapped") {
          if (!is.null(auxf)) {
            raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
            suppressWarnings(
              maps::map("world", projection = "orthographic", interior = FALSE, 
                        col = "gray10", orientation = ori, lwd = 1.5,
                        xlim = c(min(clon), max(clon)), 
                        ylim = c(min(clat), max(clat)),
                        lforce = "e",
                        add = TRUE)
            )
            
            draw.circle(0,0,0.992, border = "gray85", lwd = 5)
            draw.circle(0,0,1, border = "gray35", lwd = 1)
          } else {
              raster::extent(stacks[[j]]) <- c(-1, 1, -1, 1)
              suppressWarnings(
              maps::map("world", projection = "orthographic", interior = FALSE, 
                        col = "gray20", orientation = c(0,0,0), add = TRUE)
              )
          }
        } else if (area == "GL") {
            if (lon_max >= 359) {
              maps::map("world2", interior = FALSE, xlim = c(lon_min, lon_max), 
                        col = "gray20", ylim = c(lat_min, lat_max), wrap = c(-180, 180), add = TRUE)
            } else {
                maps::map("world", interior = FALSE, xlim = c(lon_min, lon_max), 
                          col = "gray20", ylim = c(lat_min, lat_max), wrap = c(-180, 180), 
                          lforce = "e", add = TRUE)
            }
        } else {
            if (lon_max >= 359) {
              maps::map("world2", interior = FALSE, xlim = c(lon_min, lon_max), 
                        col = "gray20", ylim = c(lat_min, lat_max), lforce = "e", add = TRUE)
            } else {
                maps::map("world", interior = FALSE, xlim = c(lon_min, lon_max), 
                          col = "gray20", ylim = c(lat_min, lat_max), lforce = "e", add = TRUE)
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
      
      # Get subset of raster stack, which contains data to plot
      if (area == "NP" | area == "SP") {
        plotdata <- raster::subset(stacks[[j]], slot_i)
      }

      if (legends[j]) {
        if (logsc[j]) {
          if (logzero) {
            raster::plot(raster::calc(plotdata, fun=log) * scalef[j],
                         main = "",
                         axes = FALSE,
                         xlab = "",
                         ylab = "",
                         zlim = log(plot_lim[j,]),
                         legend.only = TRUE,
                         legend.shrink = 0.9,
                         legend.width = 1.5,
                         legend.mar = 5.1,
                         legend.args=list(text = units[j],
                                          side = 2,
                                          font = 2,
                                          line = 0.2,
                                          cex = 1.25*fsf),
                         axis.args=list(cex.axis = 1*fsf,
                                        at=log(ticks), labels=ticks_labs),
                         col = col,
                         add = TRUE)  
            
            if (triup[j] || tridown[j]) {
              fields::imagePlot(datav, 
                                zlim = plot_lim[j,],   
                                legend.only = TRUE,
                                legend.shrink = 0.9,
                                legend.width = 1.5,
                                legend.mar = 5.1,
                                axis.args=list(labels = FALSE,
                                               tick = FALSE),
                                col = col,
                                upperTriangle = triup[j],
                                lowerTriangle = tridown[j],
                                add = TRUE)
            }
            
          } else {
              if (triup[j] || tridown[j]) {
                fields::imagePlot(datav, 
                                zlim = plot_lim[j,],   
                                legend.only = TRUE,
                                legend.shrink = 0.9,
                                legend.width = 1.5,
                                legend.mar = 5.1,
                                axis.args=list(labels = FALSE,
                                               tick = FALSE),
                                col = col,
                                upperTriangle = triup[j],
                                lowerTriangle = tridown[j],
                                add = TRUE)
              }
            
              fields::image.plot(log(datav), 
                               zlim = log(plot_lim[j,]),   
                               legend.only = TRUE,
                               legend.shrink = 0.9,
                               legend.width = 1.5,
                               legend.mar = 5.1,
                               legend.args=list(text = units[j], 
                                                side = 2, 
                                                font = 2, 
                                                line = 0.2, 
                                                cex = 1.25*fsf),
                               axis.args=list(cex.axis = 1*fsf,
                                              at=log(ticks), labels=ticks_labs),
                               col = col,
                               add = TRUE)
          }
        } else {
           if (!is.na(tick_lab[[j]][1])) {
            breaks <- seq(plot_lim[j,1], plot_lim[j,2], length.out=(length(col) + 1))
            labstep <- (breaks[2] - breaks[1]) / 2
            labat <- seq(breaks[1]+labstep, breaks[length(breaks)]-labstep, length.out = length(tick_lab[[j]]))
            graphics::par(cex=1.0)
            raster::plot(plotdata * scalef[j],
                       main = "",
                       axes = FALSE,
                       xlab = "",
                       ylab = "",
                       zlim = plot_lim[j,],
                       breaks = breaks,
                       legend.only = TRUE,
                       legend.shrink = 0.9,
                       legend.width = 1.5,
                       legend.mar = 5.1,
                       legend.args=list(text = units[j], 
                                        side = 2, 
                                        font = 2, 
                                        line = 0.2, 
                                        cex = 1.25*fsf),
                       axis.args=list(cex.axis = 1*fsf,
                                      at=labat, 
                                      labels=tick_lab[[j]],
                                      las = 3),
                       col = col,
                       add = TRUE)
           } else {
             if (triup[j] || tridown[j]) {
               fields::imagePlot(datav, 
                                 zlim = plot_lim[j,],   
                                 legend.only = TRUE,
                                 legend.shrink = 0.899,
                                 legend.width = 1.5,
                                 legend.mar = 5.1,
                                 axis.args=list(labels = FALSE,
                                                tick = FALSE),
                                 col = col,
                                 upperTriangle = triup[j],
                                 lowerTriangle = tridown[j],
                                 add = TRUE)
               
               raster::plot(plotdata * scalef[j],
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
             } else {
                 raster::plot(plotdata * scalef[j],
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
             
           }
        }
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
    if (exists("stacks")) rm(stacks)
    if (exists("plotfile")) rm(plotfile)
    if (exists("datav")) rm(datav)
    gc()
  }
  # Clean up 
  if (file.exists("Rplots.pdf")) {
    file.remove("Rplots.pdf")
  }
  gc()
}
