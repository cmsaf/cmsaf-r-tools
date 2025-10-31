#' Render a regional raster plot with optional custom boundaries
#'
#' Produces a map image for a selected region by cropping/masking a NetCDF raster,
#' handling longitude wrap (0–360 -> -180–180), CRS alignment, and setting sane
#' plot limits/aspect. Designed for use in the CM SAF R Toolbox.
#'
#' @param infile Character path to the NetCDF input file.
#' @param outfile Optional path for the output image file. If \code{NULL}, a temp file is used.
#' @param fileExtension Output file extension, one of \code{".png"}, \code{".jpg"}, \code{".pdf"}.
#' @param visualizeVariables List with at least \code{$vn} (variable name) and \code{$date.time}.
#' @param visualizeDataMax Numeric; used for colorbar tick calculation.
#' @param lon_bounds,lat_bounds Numeric vectors of visible longitude/latitude bounds (unused here but part of API).
#' @param lon_loc_vec,lat_loc_vec,name_loc_vec Optional location markers (lon/lat + labels).
#' @param division Character name of the attribute/level used to select the region (e.g. \code{"COUNTRY"} or a shapefile field).
#' @param selectedRegion Character code/value of the selected region (e.g. ISO3).
#' @param region_data Spatial or sf object with user-provided regions (when \code{division != "COUNTRY"}).
#' @param timestep Selected timestep (should match an entry of \code{visualizeVariables$date.time}).
#' @param num_tick,num_rmin,num_rmax Numeric settings for legend ticks and z range.
#' @param location Logical; draw location markers if \code{TRUE}.
#' @param text1,text2,text3 Character strings for title, footer, and legend label.
#' @param PAL,palettes,num_brk,reverse Color palette settings passed to \code{getColors()}.
#' @param textsize Numeric base text size used in plotting.
#' @param bordercolor Color for region borders and markers.
#' @param plot_grid,grid_col Currently unused plotting options (kept for API compatibility).
#' @param image_def,ihsf Image sizing settings from the Toolbox.
#' @param nc Optional opened NetCDF handle; if provided, \code{infile <- nc$filename}.
#'
#' @return A named list with \code{src} (file path), \code{contentType}, \code{width}, \code{height}.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Loads the raster brick for the requested variable.
#'   \item Normalizes longitudes to -180 to 180 if input is 0–360 (via \code{raster::rotate}).
#'   \item Validates/aligns the region geometry (sf -> Spatial, EPSG:4326, transform to raster CRS).
#'   \item Crops/masks the raster by the region and draws the image with geographic aspect.
#' }
#' The package \pkg{lwgeom} is used \emph{optionally} (via \code{requireNamespace}) to fix invalid geometries;
#' a \code{st_buffer(., 0)} fallback is applied if \pkg{lwgeom} is not available.
#'
#' @importFrom methods as
#' @importFrom raster brick crop mask xFromCol yFromRow extent rotate crs
#' @importFrom fields image.plot
#' @importFrom graphics par points text mtext
#' @export
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
  if (is.null(outfile)) outfile <- tempfile(fileext = fileExtension)
  
  col <- getColors(PAL = PAL, palettes = palettes, num_brk = num_brk, reverse = reverse)
  
  # --- helpers: geographic aspect ratio (great-circle) ------------------------
  map_aspect <- function(x_range, y_range) {
    x.center <- sum(range(x_range)) / 2
    y.center <- sum(range(y_range)) / 2
    x.dist <- dist_central_angle(x.center + c(-0.5, 0.5), rep(y.center, 2))
    y.dist <- dist_central_angle(rep(x.center, 2), y.center + c(-0.5, 0.5))
    y.dist / x.dist
  }
  dist_central_angle <- function(lon, lat) {
    lat <- lat * pi / 180; lon <- lon * pi / 180
    hav  <- function(x) sin(x / 2)^2
    ahav <- function(z) 2 * asin(z)
    n <- length(lat)
    ahav(sqrt(hav(diff(lat)) + cos(lat[-n]) * cos(lat[-1]) * hav(diff(lon))))
  }
  
  # --- 1) Load raster and normalize longitudes --------------------------------
  ras <- raster::brick(infile, varname = visualizeVariables$vn)
  
  # If raster longitudes are on [0, 360], rotate to [-180, 180] for consistency
  lon_vals <- try(raster::xFromCol(ras), silent = TRUE)
  if (!inherits(lon_vals, "try-error")) {
    if (is.finite(max(lon_vals, na.rm = TRUE)) && max(lon_vals, na.rm = TRUE) > 180) {
      ras <- raster::rotate(ras)
    }
  }
  r_crs <- raster::crs(ras)
  
  # --- 2) Select region (COUNTRY or user-provided) ----------------------------
  if (division == "COUNTRY") {
    countriesHigh <- numeric(0)
    utils::data("countriesHigh", package = "rworldxtra", envir = environment())
    region <- countriesHigh[countriesHigh$ISO3.1 == selectedRegion, ]
  } else {
    region <- region_data[region_data[[division]] == selectedRegion, ]
  }
  
  # Basic sanity check
  if (is.null(region) ||
      (inherits(region, "sf") && nrow(region) == 0) ||
      (inherits(region, "Spatial") && length(region) == 0)) {
    stop("Selected region is empty. Check 'division' and 'selectedRegion'.")
  }
  
  # --- 3) Validate region & align CRS -----------------------------------------
  # validate polygons without hard dependency on lwgeom
  st_make_valid_safe <- function(geom) {
    if (requireNamespace("lwgeom", quietly = TRUE)) {
      # avoid '::' to silence R CMD check NOTE
      fun <- getExportedValue("lwgeom", "st_make_valid")
      return(fun(geom))
    } else {
      # fallback fix for self-intersections etc.
      return(sf::st_buffer(geom, 0))
    }
  }
  
  fix_geometry_safe <- function(x) {
    # Validate polygons without hard dependency on lwgeom
    if (inherits(x, "sf") || inherits(x, "sfc")) {
      sf::sf_use_s2(FALSE)
      g <- if (inherits(x, "sf")) sf::st_geometry(x) else x
      if (!all(sf::st_is_valid(g))) {
        g <- st_make_valid_safe(g)
      }
      g <- sf::st_collection_extract(g, "POLYGON", warn = FALSE)
      g <- sf::st_cast(g, "MULTIPOLYGON", warn = FALSE)
      g <- g[!sf::st_is_empty(g)]
      if (inherits(x, "sf")) { sf::st_geometry(x) <- g; x } else { g }
    } else x
  }
  
  if (inherits(region, "sf")) {
    region <- fix_geometry_safe(region)
    if (is.na(sf::st_crs(region))) region <- sf::st_set_crs(region, 4326) else region <- sf::st_transform(region, 4326)
    region <- methods::as(region, "Spatial")
  }
  if (inherits(region, "Spatial") && is.na(sp::proj4string(region))) {
    sp::proj4string(region) <- "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  # Align region to the raster CRS (if present)
  if (!is.null(r_crs) && !is.na(r_crs) &&
      inherits(region, "Spatial") && !is.na(sp::proj4string(region))) {
    if (!identical(sp::proj4string(region), as.character(r_crs))) {
      region <- sp::spTransform(region, r_crs)
    }
  }
  
  # --- 4) Crop & mask (NO trim here) ------------------------------------------
  ras <- raster::crop(ras, region)
  if (is.null(ras)) stop("Cropping returned NULL. Region may be outside data extent.")
  ras <- raster::mask(ras, region)
  
  # Keep this a warning (not stop), as in your earlier working version
  if (all(is.na(raster::getValues(ras[[1]])))) {
    warning("All values are NA after masking. Check overlap/CRS of region and data.")
  }
  
  # --- 5) Build lon/lat sequences and z-matrix --------------------------------
  # Safe timestep lookup
  idx_ts <- which(visualizeVariables$date.time == timestep)
  if (length(idx_ts) != 1L) {
    warning(sprintf("Requested timestep not found (got %s); falling back to first layer.",
                    paste(idx_ts, collapse = ",")))
    idx_ts <- 1L
  }
  
  # Sequences from the (masked) raster
  lon_seq <- raster::xFromCol(ras)
  lat_seq <- raster::yFromRow(ras)
  
  # Matrix for the selected layer
  lyr <- ras[[idx_ts]]
  ras_matrix <- raster::as.matrix(lyr)  # rows = nrow, cols = ncol
  ras_matrix <- t(ras_matrix)           # rows = ncol (lon), cols = nrow (lat)
  
  # Ensure monotonic axes and keep matrix shape aligned
  if (is.unsorted(lon_seq)) {
    ord_x <- order(lon_seq)
    lon_seq    <- lon_seq[ord_x]
    ras_matrix <- ras_matrix[ord_x, , drop = FALSE]
  }
  if (is.unsorted(lat_seq)) {
    ord_y <- order(lat_seq)
    lat_seq    <- lat_seq[ord_y]
    ras_matrix <- ras_matrix[, ord_y, drop = FALSE]
  }
  
  # Guard against degenerate 1xN or Nx1 matrices (rare; duplicate if needed)
  if (nrow(ras_matrix) < 2L) {
    ras_matrix <- rbind(ras_matrix, ras_matrix)
    lon_seq <- c(lon_seq, lon_seq[length(lon_seq)] + max(1e-9, diff(range(lon_seq, na.rm = TRUE)) * 1e-6))
  }
  if (ncol(ras_matrix) < 2L) {
    ras_matrix <- cbind(ras_matrix, ras_matrix)
    lat_seq <- c(lat_seq, lat_seq[length(lat_seq)] + max(1e-9, diff(range(lat_seq, na.rm = TRUE)) * 1e-6))
  }
  
  # --- 6) Plot limits and aspect ratio ----------------------------------------
  # --- unified limits: raster extent ∪ region bbox, then small padding ----------
  rext <- raster::extent(ras)
  xlim_r <- c(rext@xmin, rext@xmax)
  ylim_r <- c(rext@ymin, rext@ymax)
  
  if (inherits(region, "Spatial")) {
    bb <- sp::bbox(region)
    xlim_reg <- c(bb["x", 1], bb["x", 2])
    ylim_reg <- c(bb["y", 1], bb["y", 2])
  } else {
    xlim_reg <- xlim_r
    ylim_reg <- ylim_r
  }
  
  # union of raster and region
  xlim <- range(c(xlim_r, xlim_reg), na.rm = TRUE)
  ylim <- range(c(ylim_r, ylim_reg), na.rm = TRUE)
  
  # small breathing room to avoid cut-offs (1% of span; min ~0.05°)
  pad_frac <- 0.01
  xpad <- max(0.05, diff(xlim) * pad_frac)
  ypad <- max(0.05, diff(ylim) * pad_frac)
  xlim <- c(xlim[1] - xpad, xlim[2] + xpad)
  ylim <- c(ylim[1] - ypad, ylim[2] + ypad)
  
  # geographic aspect from the final limits
  aspect_ratio <- map_aspect(xlim, ylim)
  
  # --- 7) Legend ticks ---------------------------------------------------------
  tlab <- break_num(
    ln = num_tick,
    bn = num_tick,
    minn = num_rmin,
    maxn = num_rmax,
    max_data = visualizeDataMax
  )
  
  # --- 8) Device dimensions (use current bounds) -------------------------------
  imDim <- recalculateImageDimensions(
    visualizeVariables = visualizeVariables,
    lon_bounds = xlim,
    lat_bounds = ylim,
    image_def = image_def,
    ihsf = ihsf
  )
  
  iwidth  <- imDim$imagewidth
  # iheight <- imDim$imageheight
  
  # --- make device fit the map aspect so 'asp' won't expand axes ----------------
  device_ratio <- (diff(ylim) * aspect_ratio) / diff(xlim)  # ~ height/width
  # keep your computed width; derive height to match map aspect
  iheight <- max(300L, round(iwidth * device_ratio))
  
  if (fileExtension == ".png") {
    grDevices::png(outfile, width = iwidth, height = iheight)
  } else if (fileExtension == ".jpg") {
    grDevices::jpeg(outfile, width = iwidth, height = iheight)
  } else if (fileExtension == ".pdf") {
    grDevices::pdf(outfile, width = iwidth / 72, height = iheight / 72)
  }
  
  ####### DEBUG ######
  # Diagnostics: raster/grid and region
  # xr <- range(raster::xFromCol(ras), na.rm = TRUE)
  # yr <- range(raster::yFromRow(ras), na.rm = TRUE)
  # rb <- raster::extent(ras)  # raster extent in lon/lat
  # reg_bb <- if (inherits(region, "Spatial")) sp::bbox(region) else NULL
  # 
  # message(sprintf("lon range grid: [%.3f, %.3f]  | lat range grid: [%.3f, %.3f]", xr[1], xr[2], yr[1], yr[2]))
  # if (!is.null(reg_bb)) {
  #   message(sprintf("region bbox lon: [%.3f, %.3f] | lat: [%.3f, %.3f]",
  #                   reg_bb["x",1], reg_bb["x",2], reg_bb["y",1], reg_bb["y",2]))
  # }
  # message(sprintf("raster extent lon: [%.3f, %.3f] | lat: [%.3f, %.3f]",
  #                 rb@xmin, rb@xmax, rb@ymin, rb@ymax))
  # message(sprintf("z dims (cols x rows): %d x %d", nrow(ras_matrix), ncol(ras_matrix)))
  
  # --- 9) Plot -----------------------------------------------------------------
  graphics::par(mar = c(2, 2, 2.6, 2), xaxs = "i", yaxs = "i")
  
  fields::image.plot(
    x = lon_seq,
    y = lat_seq,
    z = ras_matrix,
    main = text1,
    cex.main = textsize,
    cex.lab  = textsize,
    xlab = " ",
    ylab = " ",
    zlim = c(num_rmin, num_rmax),
    col  = col,
    axis.args = list(
      cex.axis = textsize,
      at = as.numeric(tlab[tlab != ""]),
      labels = tlab[tlab != ""],
      mgp = c(1, 0.4, 0),
      tck = -0.3
    ),
    legend.lab  = text3,
    legend.line = -2 * (1 + (textsize - 1.2) / 4),
    axes = TRUE,
    xlim = xlim,
    ylim = ylim,
    asp  = aspect_ratio
  )
  
  raster::plot(region, add = TRUE, border = bordercolor, lwd = 2)
  
  if (location) {
    for (i in seq_along(lon_loc_vec)) {
      graphics::points(lon_loc_vec[i], lat_loc_vec[i], pch = 16, col = bordercolor)
      graphics::text(lon_loc_vec[i], lat_loc_vec[i], name_loc_vec[i], pos = 1, col = bordercolor, cex = textsize)
    }
  }
  
  graphics::mtext(text2, cex = textsize)
  graphics::mtext(visualizeVariables$copyrightText, side = 1, adj = 1, cex = textsize)
  
  on.exit(grDevices::dev.off())
  
  list(
    src = outfile,
    contentType = getMimeType(fileExtension),
    width = iwidth,
    height = iheight
  )
}
