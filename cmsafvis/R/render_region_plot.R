#' Function to create a plot of a selected country.
#'
#' This function generates a plot of within a certain region (e.g. a country).
#'
#' @inheritParams render_plot
#' @param infile The nc file to be visualized.
#' @param division Division to contain region (either 'COUNTRY' or something coherent to region_data).
#' @param region_data If Division is not 'COUNTRY' then region_data has to contain spatial data of the given division.
#' @param selectedRegion The region to be cropped according to division. If `division == "COUNTRY"`, the country's [3-character ISO code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) should be used. Otherwise it has to fit to the passed region data.
#' @param timestep Timestep to visualize at (character).
#' @param image_def Default size (positive numeric).
#' @param ihsf Image height scaling factor (positive numeric).
#' @param nc Alternatively to \code{infile} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
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
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  col <- getColors(
    PAL = PAL,
    palettes = palettes,
    num_brk = num_brk,
    reverse = reverse
  )

  if (division == "COUNTRY") {
    countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
    utils::data("countriesHigh", package = "rworldxtra", envir = environment())
    region <- countriesHigh[countriesHigh$ISO3.1 == selectedRegion,]
  } else {
    region <- region_data[region_data[[division]] == selectedRegion,]
  }

  # Handle different file formats.
  if (fileExtension == ".tif" | fileExtension == ".kml") {
    ras <- raster::brick(infile, varname = visualizeVariables$vn)
    ras <- ras[[which(visualizeVariables$date.time == timestep)]]
    ras <- raster::mask(ras, region)
    if (fileExtension == ".tif") {
      ras_col <- raster::RGB(ras, col = col)

      check_package_dependency("rgdal", "exporting GeoTIFF files")
      raster::writeRaster(ras_col, filename = outfile, format = "GTiff")  # Requires package rgdal
    } else if (fileExtension == ".kml") {
      # kml_toolbox <- raster::rasterToPolygons(ras)
      # 
      # check_package_dependency("plotKML", reason = "exporting KML files")
      # 
      # plotKML::plotKML(
      #   kml_toolbox,
      #   file = outfile,
      #   kmz = FALSE,
      #   open.kml = FALSE,
      #   plot.labpt = FALSE,
      #   overwrite = TRUE,
      #   outline = 0
      # )
      
      cat("Due to issues with the plotKML R-package we decided to remove
              KML output from the CM SAF R Toolbox.
              We are working on a solution for the next update.","\n")
      
    }
    return(list(
      src = outfile,
      contentType = getMimeType(fileExtension)
    ))
  } else {
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

    imDim <- recalculateImageDimensions(
      visualizeVariables = visualizeVariables,
      lon_bounds = lon_bounds,
      lat_bounds = lat_bounds,
      image_def = image_def,
      ihsf = ihsf
    )

    iwidth  <- imDim$imageheight
    iheight <- imDim$imagewidth
    if (fileExtension == ".png") {
      grDevices::png(outfile, width = iwidth, height = iheight)
    } else if (fileExtension == ".jpg") {
      grDevices::jpeg(outfile, width = iwidth, height = iheight)
    } else if (fileExtension == ".pdf") {
      pdfWidth <- iwidth / 72
      pdfHeight <- iheight / 72
      grDevices::pdf(outfile, width = pdfWidth, height = pdfHeight)
    }

    raster::plot(
      ras[[which(visualizeVariables$date.time == timestep)]],
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
        line = -2
      )
    )

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

    if (plot_grid) {
      graphics::grid(NULL, NULL, lty = 3, col = grid_col)
    }

    graphics::mtext(text2)
    graphics::mtext(visualizeVariables$copyrightText,
                    side = 1,
                    adj = 1)

    raster::plot(region, add = TRUE)

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
    on.exit(grDevices::dev.off())

    # Return a list containing the filename
    return(
      list(
        src = outfile,
        contentType = getMimeType(fileExtension),
        width = iwidth,
        height = iheight
      )
    )
  }
}
