# helper functions: This functions render the trend plot
plot_sig_map <- function(infile, sig.parameter, nc = NULL)
{
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- ncdf4::nc_open(infile)
  
  file_data <- cmsafops::read_file(infile, sig.parameter, nc = nc)
  lon <- file_data$dimension_data$x
  lat <- file_data$dimension_data$y
  
  outfile <- NULL
  
  num_tick <- 5
  
  dum_dat <- ncdf4::ncvar_get(nc_in, sig.parameter, start = c(1, 1, 1), count = c(-1, -1, 1))
  min_data <- min(dum_dat, na.rm = TRUE)
  max_data <- max(dum_dat, na.rm = TRUE)
  
  if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
    num_rmax <- round(max_data, digits = 1) + 0.1
  } else {
    num_rmax <- round(max_data, digits = 1)
  }
  
  num_rmin <- round(min_data, digits = 1)
  
  tlab <- break_num(
    ln = num_tick,
    bn = num_tick,
    minn = num_rmin,
    maxn = num_rmax,
    max_data = max_data
  )
  
  min_lon <- min(lon, na.rm = TRUE)
  max_lon <- max(lon, na.rm = TRUE)
  min_lat <- min(lat, na.rm = TRUE)
  max_lat <- max(lat, na.rm = TRUE)
  
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
  
  
  # default Color palette
  col <- c("#020ef0", "#27b304", "#ffe205", "#ff0505")
  
  image_def <- 800
  iwidth  <- image_def
  iheight <- round(image_def * (2 / 3))
  
  textsize <- 0.7
  graphics::par(cex = textsize)
  graphics::par(mar = c(2, 2, 2.6, 2))
  
  var <- file_data$variable$name
  text1 <- ncdf4::ncatt_get(nc_in, var, "long_name")$value
  
  unit <- ncdf4::ncatt_get(nc_in, var, "units")$value
  text3 <- paste0(text1)
  # Plot with legend and title
  fields::image.plot(
    lon,
    lat,
    dum_dat,
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
    legend.line = -1.3,
    legend.cex = 0.9 * textsize,
    axes = FALSE
  )
  
  na.color    <- "gray80"
  # linesize, bordercolor, plot_grid, and grid_col, na.color can be found in global.R
  graphics::image(
    lon,
    lat,
    array(1:2, dim(dum_dat)),
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
    dum_dat,
    xlab = " ",
    ylab = " ",
    xlim = lon_bounds,
    ylim = lat_bounds,
    zlim = c(num_rmin, num_rmax),
    col = col,
    axes = FALSE,
    add = TRUE
  )
  
  int <- TRUE
  linesize <- 1.5
  bordercolor <- "gray20"
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
    maps::map(
      "world",
      add = TRUE,
      interior = FALSE,
      resolution = 0,
      col = bordercolor,
      lwd = linesize
    )
  }
  
  plot_grid <- FALSE
  grid_col  <- "cornsilk2"
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
  
  text2 <- " "
  # Add subtitle and copyright tag
  graphics::mtext(text2)
  
  creator_att2 <- ncdf4::ncatt_get(nc_in, 0, "creator_name")
  creator2 <- ifelse(creator_att2$hasatt, creator_att2$value, "-")
  copyrightText2 <- paste0("Data Source: ", creator2)
  graphics::mtext(copyrightText2,
                  side = 1,
                  adj = 1,
                  line = 0.3)
  
  if (is.null(nc)) ncdf4::nc_close(nc_in)
}


plot_sig_map_region <- function(var,
                               infile,
                               outfile = NULL,
                               selectedRegion) {
  
  image_def <- 800         # default image size
  ihsf      <- 0.1 
  
  nc_in_region <- ncdf4::nc_open(infile)
  file_data <- cmsafops::read_file(infile, var)
  lon <- file_data$dimension_data$x
  lat <- file_data$dimension_data$y
  
  col <- c("#000000",  "#F37600", "#FFEF7A", "#FFFFFF")
  
  countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
  utils::data("countriesHigh", package = "rworldxtra", envir = environment())
  region <- countriesHigh[countriesHigh$ISO3.1 == selectedRegion,]
  
  # Handle different file formats.
  ras <- raster::brick(infile, varname = var)
  ras <- raster::crop(ras, region)
  ras <- raster::mask(ras, region)
  
  dum_dat <- ncdf4::ncvar_get(nc_in_region, var, start = c(1, 1, 1), count = c(-1, -1, 1))
  
  min_data <- min(dum_dat, na.rm = TRUE)
  max_data <- max(dum_dat, na.rm = TRUE)
  
  if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
    num_rmax <- round(max_data, digits = 1) + 0.1
  } else {
    num_rmax <- round(max_data, digits = 1)
  }
  
  num_rmin <- round(min_data, digits = 1)
  
  num_tick <- 5
  tlab <- break_num(
    ln = num_tick,
    bn = num_tick,
    minn = num_rmin,
    maxn = num_rmax,
    max_data = max_data
  )
  
  lon_bounds <- c(raster::xmin(ras), raster::xmax(ras))
  lat_bounds <- c(raster::ymin(ras), raster::ymax(ras))
  
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
  
  lon <- lon[lon <= lon_bounds[2]]
  lon <- lon[lon_bounds[1] <= lon]
  
  lat <- lat[lat <= lat_bounds[2]]
  lat <- lat[lat_bounds[1] <= lat]
  
  # Update this value if you want to change min width/height of plot.
  minSize <- 200
  tmpWidth  <- max(minSize, image_def)
  tmpHeight <- max(minSize, image_def)
  
  # Update width and height according to visualizeVariables lat and lon vectors
  if (length(lon) >= length(lat)) {
    # Shrink height
    tmpHeight <- round(tmpWidth * length(lat) / length(lon))
    if (tmpHeight < minSize) {
      tmpWidth <- minSize / tmpHeight * tmpWidth
      tmpHeight <- minSize
    }
    
    # Why are we doing this? (And why not in the else block?)
    imageheight <- tmpHeight + (round(ihsf * tmpHeight))
    imagewidth <- tmpWidth
  } else {
    # No need to check against minSize since we will multiply with a value > 1.
    tmpWidth <- round(tmpHeight * length(lat) / length(lon))
    
    imagewidth <- tmpWidth
    imageheight <- tmpHeight
  }
  imDim <- list(imagewidth = imagewidth, imageheight = imageheight)
  
  iwidth  <- imDim$imageheight
  iheight <- imDim$imagewidth
  
  text1 <- ncdf4::ncatt_get(nc_in_region, var, "long_name")$value
  text3 <- paste0(text1)
  textsize <- 0.7
  raster::plot(
    ras[[1]],
    main = text1,
    col = col,
    axes = FALSE,
    xlab = " ",
    ylab = " ",
    xlim = lon_bounds,
    ylim = lat_bounds,
    zlim = c(num_rmin, num_rmax),
    axis.args = list(
      cex.axis = 1,
      at = as.numeric(tlab[tlab != ""]),
      labels = tlab[tlab != ""],
      mgp = c(1, 0.4, 0),
      tck = c(-0.3)
    ),
    legend.args = list(
      text = text3,
      cex = 0.8 * textsize,
      side = 4,
      line = -1.2
    )
  )
  
  bordercolor <- "gray20"
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
  
  text2 <- " "
  graphics::mtext(text2)
  
  creator_att2 <- ncdf4::ncatt_get(nc_in_region, 0, "creator_name")
  creator2 <- ifelse(creator_att2$hasatt, creator_att2$value, "-")
  copyrightText2 <- paste0("Data Source: ", creator2)
  graphics::mtext(copyrightText2,
                  side = 1,
                  adj = 1,
                  line = 0.3)
  
  raster::plot(region, add = TRUE)
  ncdf4::nc_close(nc_in_region)
}

plot_trend <- function(variable,
                       infile,
                       climatology_file,
                       out_dir,
                       climate_year_start,
                       climate_year_end,
                       start_date,
                       end_date,
                       country_code,
                       outfile_name,
                       language,
                       title = "",
                       subtitle = "",
                       trend_line = TRUE,
                       verbose,
                       nc = NULL)
{
  # use wfldmean for trend plot
  cmsafops::wfldmean(variable, infile, outfile = file.path(tempdir(),"tmp_time_series_plot.nc"), overwrite = TRUE, nc = nc)
  temp_file <- file.path(tempdir(),"tmp_time_series_plot.nc")
  file_data <- cmsafops::read_file(temp_file, variable)
  nc_in <- ncdf4::nc_open(temp_file)

  # read data from infile
  dum_dat <- ncdf4::ncvar_get(
    nc_in,
    file_data$variable$name,
    collapse_degen = FALSE
  )
  
  dim_names <- names(nc_in$dim)
  dimensions <- cmsafops::get_dimensions(nc_in, dim_names)
  time_info <- cmsafops::get_time_info(nc_in, dim_names, dimensions$names$t)
  
  dimension.data.t <- nc_in$dim[[dimensions$names$t]]$vals
  dum_dat <- as.vector(dum_dat)
  date_info <- as.Date(cmsafops::get_time(time_info$units, dimension.data.t))
 
  dataT <- data.frame(date_info, dum_dat)
  ncdf4::nc_close(nc_in)
 
  # calc. significance
  # tmp-file for mann-kendall test
  tmp.mk.test.outfile <- file.path(tempdir(), "tmp.mk.test.outfile.nc")
  if(file.exists(tmp.mk.test.outfile)){
    unlink(tmp.mk.test.outfile)
  }
  
  cmsafops::cmsaf.mk.test(
    var = variable,
    infile = infile,
    outfile = tmp.mk.test.outfile,
    nc34 = 4,
    overwrite = TRUE,
    verbose = FALSE,
    nc = nc
  )
  
  tmp.trend.outfile <- file.path(tempdir(), "tmp.trend.outfile.nc")
  if(file.exists(tmp.trend.outfile)){
    unlink(tmp.trend.outfile)
  }
  
  cmsafops::trend(
    var = variable,
    infile = infile,
    outfile = tmp.trend.outfile,
    option = 1,
    nc34 = 4,
    overwrite = TRUE,
    verbose = FALSE,
    nc = nc
  )
  
  grDevices::png(paste0(out_dir, "/", outfile_name), units="in", width=18, height=6, res=100)
  graphics::par(mfrow=c(1,3), mar=c(5.1, 4.1, 4.1, 3.1)) 

  if(country_code == "S_A"){
    plot_sig_map(tmp.trend.outfile, sig.parameter = paste0(variable, "_trend1")) # use trend1
    plot_sig_map(tmp.trend.outfile, sig.parameter = "sig") # use sig 95%
    plot_sig_map(tmp.mk.test.outfile, sig.parameter = "S")
  }
  else{
    plot_sig_map_region(infile = tmp.trend.outfile, var = paste0(variable, "_trend1"), selectedRegion = country_code) # use trend1
    plot_sig_map_region(infile = tmp.trend.outfile, var = "sig", selectedRegion = country_code) # use sig 95%
    plot_sig_map_region(infile = tmp.mk.test.outfile, var = "S", selectedRegion = country_code)
  }
  
  # if(trend_line)
  # {
  #   mod <- lm(dataT$dum_dat ~ dataT$date_info, data = dataT)   # model
  # }
  # 
  # par(mar = c(5.1,5,4.1,2.1))
  # plot(dataT, main = title, sub = subtitle, xlab = "Year", ylab = paste0("Anomaly of ",variable), type = "l", lwd = 2, col = "red")
  # if(trend_line)
  # {
  #   # plot trend line
  #   abline(mod)
  # }
  # clip(x1 = min(dataT$date_info),
  #      x2 = max(dataT$date_info),
  #      y1 = min(dataT$dum_dat),
  #      y2 = 0)
  # lines(dataT, lwd = 2, col = "blue")
  # abline(h = 0, col = "grey")

  grDevices::dev.off()
  
  # calc monitor climate parameters
  tmp_climate_dir <- file.path(tempdir(), "tmp_climate_dir")
  # remove if it exists
  if (dir.exists(tmp_climate_dir)) {
    unlink(tmp_climate_dir, recursive = TRUE)
  }
  # create new temp dic
  if (!dir.exists(tmp_climate_dir)) {
    dir.create(tmp_climate_dir)
  }
  
  # Clim mean value
  tmp_clim_mean_value <- file.path(tmp_climate_dir, paste0("tmp_clim_mean_value.nc"))
  cmsafops::fldmean(var = variable, infile = climatology_file, outfile = tmp_clim_mean_value, overwrite = TRUE)
  nc_in <- ncdf4::nc_open(tmp_clim_mean_value)
  dum_dat_mean <- ncdf4::ncvar_get(nc_in, variable, collapse_degen = FALSE)
  ncdf4::nc_close(nc_in)
  
  years_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$years
  months_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$months
  days_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$days
  
  ranking <- data.frame(years_all, months_all, days_all, as.vector(dum_dat))
  names(ranking) <- c("Year", "Month", "Day","Value")
  
  titles <- c("Analyzed years", "Climatology Mean Value", "Maximum", "Minimum")
  
  ordered_index_dataT <- order(dataT['dum_dat'])
  ordered_dataT <- dataT[ordered_index_dataT, ]
  row.names(ordered_dataT) <- NULL
  
  standout_years <- c(paste0(climate_year_start, " - " ,format(end_date, format = "%Y")),
                      paste(climate_year_start, climate_year_end, sep = " - "),
                      toString(ordered_dataT[nrow(ordered_dataT),1]),
                      toString(ordered_dataT[1,1]))
  
  standout_values <- c(toString(mean(dataT$dum_dat)), mean(dum_dat_mean), toString(ordered_dataT[nrow(ordered_dataT),2]), toString(ordered_dataT[1,2]))
  
  final_values <- data.frame(title = titles, years = standout_years, value = standout_values)
  calc.parameters.monitor.climate(final_values, ranking[order(ranking$Value),])
  
  # remove if it exists
  if (dir.exists(tmp_climate_dir)) {
    unlink(tmp_climate_dir, recursive = TRUE)
  }
}
