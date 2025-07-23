render_region_plot <- function(infile,
                               outfile = NULL,
                               fileExtension = ".png",
                               visualizeVariables,
                               visualizeDataMax,
                               lon_bounds,
                               lat_bounds,
                               lon_loc_vec,
                               lat_loc_vec,
                               name_loc_vec,
                               division,
                               selectedRegion,
                               region_data,
                               timestep,
                               num_tick,
                               num_rmin,
                               num_rmax,
                               location,
                               text1,
                               text2,
                               text3,
                               PAL,
                               palettes,
                               num_brk,
                               reverse,
                               textsize,
                               bordercolor,
                               plot_grid,
                               grid_col,
                               image_def,
                               ihsf,
                               nc = NULL) {
  if (!is.null(nc)) infile <- nc$filename
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  
  col <- getColors(PAL = PAL, palettes = palettes, num_brk = num_brk, reverse = reverse)
  
  if (division == "COUNTRY") {
    countriesHigh <- numeric(0)
    utils::data("countriesHigh", package = "rworldxtra", envir = environment())
    region <- countriesHigh[countriesHigh$ISO3.1 == selectedRegion, ]
  } else {
    region <- region_data[region_data[[division]] == selectedRegion, ]
  }
  
  ras <- raster::brick(infile, varname = visualizeVariables$vn)
  ras <- raster::crop(ras, region)
  ras <- raster::mask(ras, region)
  
  tlab <- break_num(
    ln = num_tick,
    bn = num_tick,
    minn = num_rmin,
    maxn = num_rmax,
    max_data = visualizeDataMax
  )
  
  lon_seq <- raster::xFromCol(ras)
  lat_seq <- raster::yFromRow(ras)
  
  ras_matrix <- raster::as.matrix(ras[[which(visualizeVariables$date.time == timestep)]])
  ras_matrix <- t(ras_matrix)
  
  if (is.unsorted(lon_seq)) {
    ord_x <- order(lon_seq)
    lon_seq <- lon_seq[ord_x]
    ras_matrix <- ras_matrix[ord_x, ]
  }
  if (is.unsorted(lat_seq)) {
    ord_y <- order(lat_seq)
    lat_seq <- lat_seq[ord_y]
    ras_matrix <- ras_matrix[, ord_y]
  }
  
  imDim <- recalculateImageDimensions(
    visualizeVariables = visualizeVariables,
    lon_bounds = c(min(lon_seq), max(lon_seq)),
    lat_bounds = c(min(lat_seq), max(lat_seq)),
    image_def = image_def,
    ihsf = ihsf
  )
  
  iwidth <- imDim$imageheight
  iheight <- imDim$imagewidth
  
  if (fileExtension == ".png") {
    grDevices::png(outfile, width = iwidth, height = iheight)
  } else if (fileExtension == ".jpg") {
    grDevices::jpeg(outfile, width = iwidth, height = iheight)
  } else if (fileExtension == ".pdf") {
    grDevices::pdf(outfile, width = iwidth / 72, height = iheight / 72)
  }
  
  graphics::par(mar = c(2, 2, 2.6, 2))
  
  fields::image.plot(
    x = lon_seq,
    y = lat_seq,
    z = ras_matrix,
    main = text1,
    cex.main = textsize,                   # title reacts to slider
    cex.lab = textsize,                    # axis labels react to slider
    xlab = " ",
    ylab = " ",
    zlim = c(num_rmin, num_rmax),
    col = col,
    axis.args = list(
      cex.axis = textsize,                 # colorbar ticks
      at = as.numeric(tlab[tlab != ""]),
      labels = tlab[tlab != ""],
      mgp = c(1, 0.4, 0),
      tck = c(-0.3)
    ),
    legend.lab = text3,
    legend.line = -2 * (1 + (textsize - 1.2) / 4),  # dynamic legend placement
    axes = TRUE
  )
  
  raster::plot(region, add = TRUE, border = bordercolor, lwd = 2)
  
  if (location) {
    for (i in seq_along(lon_loc_vec)) {
      graphics::points(lon_loc_vec[i], lat_loc_vec[i], pch = 16, col = bordercolor)
      graphics::text(
        lon_loc_vec[i], lat_loc_vec[i], name_loc_vec[i],
        pos = 1, col = bordercolor, cex = textsize
      )
    }
  }
  
  graphics::mtext(text2, cex = textsize)
  graphics::mtext(visualizeVariables$copyrightText, side = 1, adj = 1, cex = textsize)
  
  on.exit(grDevices::dev.off())
  
  return(list(
    src = outfile,
    contentType = getMimeType(fileExtension),
    width = iwidth,
    height = iheight
  ))
}
