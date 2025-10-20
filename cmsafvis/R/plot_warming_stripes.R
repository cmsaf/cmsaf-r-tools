# helper function: This function renders the warming stripes plot
plot_warming_stripes <- function(variable,
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
                                 pointsTF,
                                 lineTF,
                                 title,
                                 color_pal,
                                 circ_plot,
                                 verbose,
                                 nc = NULL)
{
  # --- Decide whether input is gridded or already a field-mean time series ---
  temp_file <- file.path(tempdir(), "tmp_warming_stripes_plot.nc")
  nc0 <- ncdf4::nc_open(infile)
  dim_names0 <- names(nc0$dim)
  has_lon <- any(tolower(dim_names0) %in% c("lon","longitude","x"))
  has_lat <- any(tolower(dim_names0) %in% c("lat","latitude","y"))
  ncdf4::nc_close(nc0)
  
  if (has_lon && has_lat) {
    # Gridded input -> compute field mean time series
    cmsafops::wfldmean(variable, infile, outfile = temp_file, overwrite = TRUE, nc = nc)
  } else {
    # Already a field-mean series -> use infile directly
    temp_file <- infile
  }
  
  # --- Read the (field-mean) time series ---
  file_data <- cmsafops::read_file(temp_file, variable)
  nc_in <- ncdf4::nc_open(temp_file)
  
  dum_dat <- ncdf4::ncvar_get(nc_in, file_data$variable$name, collapse_degen = FALSE)
  dim_names   <- names(nc_in$dim)
  dimensions  <- cmsafops::get_dimensions(nc_in, dim_names)
  time_info   <- cmsafops::get_time_info(nc_in, dim_names, dimensions$names$t)
  dimension.data.t <- nc_in$dim[[dimensions$names$t]]$vals
  ncdf4::nc_close(nc_in)
  
  dum_dat     <- as.vector(dum_dat)
  date_info_1 <- as.Date(cmsafops::get_time(time_info$units, dimension.data.t))
  date_info   <- as.numeric(date_info_1)
  
  dataT <- data.frame(date_info = date_info, dum_dat = dum_dat)
  
  # --- Use full series (no date windowing; matches original behaviour) ---
  nBins  <- 10
  minT   <- suppressWarnings(min(dum_dat, na.rm = TRUE))
  maxT   <- suppressWarnings(max(dum_dat, na.rm = TRUE))
  if (!is.finite(minT) || !is.finite(maxT)) { minT <- 0; maxT <- 1 }
  rangeT <- maxT - minT
  binWidth <- if (is.finite(rangeT) && rangeT > 0) rangeT / nBins else 1
  
  # X labels (four evenly spaced labels across full series)
  xlabel <- rep(NA, length(date_info_1))
  nidx   <- if (length(xlabel) >= 4) round(seq(1, length(xlabel), length.out = 4)) else seq_along(xlabel)
  if (length(nidx) > 0) xlabel[nidx] <- format(date_info_1[nidx], "%Y")
  if (length(xlabel) >= 1 && !is.na(xlabel[1])) {
    title <- gsub("XXXX", xlabel[1], title)
  }
  
  resfactor <- 1
  
  # =======================
  # Circular stripes branch
  # =======================
  if (circ_plot) {
    grDevices::png(filename = file.path(out_dir, outfile_name),
                   res = 72*resfactor, height = 800*resfactor, width = 800*resfactor)
    
    temp <- data.frame(
      date  = format(date_info_1, "%Y"),
      level = as.numeric(dum_dat)
    )
    
    # Helpers for circular drawing
    getYmult <- function() {
      if (grDevices::dev.cur() == 1) {
        warning("No graphics device open."); return(1)
      } else {
        xyasp <- graphics::par("pin")
        xycr  <- diff(graphics::par("usr"))[c(1,3)]
        return(xyasp[1]/xyasp[2]*xycr[2]/xycr[1])
      }
    }
    draw.circle <- function(x, y, radius, nv = 100, border = NULL, col = NA,
                            lty = 1, density = NULL, angle = 45, lwd = 1) {
      ymult <- getYmult()
      angle.inc <- 2*pi/nv
      angles <- seq(0, 2*pi - angle.inc, by = angle.inc)
      if (length(col) < length(radius)) col <- rep(col, length.out = length(radius))
      for (circle in seq_along(radius)) {
        xv <- cos(angles) * radius[circle] + x
        yv <- sin(angles) * radius[circle] * ymult + y
        graphics::polygon(xv, yv, border = border, col = col[circle], lty = lty,
                          density = density, angle = angle, lwd = lwd)
      }
      invisible(NULL)
    }
    
    # Palettes
    Red   <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(170)
    Blues <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(170)
    colors <- c(rev(Blues), Red)
    if (color_pal == 2) {
      palette <- grDevices::colorRampPalette(c(
        "#474747","#7a7a7a","#a8a8a8","#cdcdcd","#e2e2e2","#f9f9f9",
        "#fdf3db","#fee8b2","#fedf8c","#fed66c","#fdcf45","#fac631"
      ))
      colors <- palette(340)
    }
    if (color_pal == 3) {
      palette <- grDevices::colorRampPalette(c(
        "#5c3209","#96560e","#b27028","#d1a759","#dfc07a","#f5e5bf","#fefefe",
        "#b0dfda","#6fc0b8","#389c94","#078470","#045f5a","#0f3c33"
      ))
      colors <- palette(340)
    }
    
    AssignColor <- function(data, colors) {
      data$colIndex <- rep(-9999, length(data$date))
      data$color    <- rep(-9999, length(data$date))
      borders <- seq(min(data$level, na.rm = TRUE),
                     max(data$level, na.rm = TRUE),
                     length = length(colors) + 1)
      for (i in 1:(length(borders) - 1)) {
        vect <- which(data$level >= borders[i] & data$level <= borders[i + 1])
        data$colIndex[vect] <- i
        data$color[vect]    <- colors[i]
      }
      data
    }
    
    temp$level[!is.finite(temp$level)] <- minT
    temp <- AssignColor(temp, colors)
    
    delta <- 5
    xxx <- seq_len(length(temp$date)) * 0
    temp$radius <- ((temp$level + abs(min(temp$level, na.rm = TRUE)))) * 0.08
    for (i in 2:length(xxx)) {
      xxx[i] <- xxx[i - 1] + temp$radius[i - 1] + temp$radius[i] + delta
    }
    
    FactorShape <- 1
    graphics::par(mfrow = c(1,1))
    graphics::par(mar = c(0, 0, 0, 0), bg = "black")
    YlimVal <- 1.1; XlimVal <- FactorShape * YlimVal
    graphics::plot(mean(xxx), col = "white",
                   xlim = c(-XlimVal, XlimVal),
                   ylim = c(-YlimVal, YlimVal),
                   bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    
    radiusStart <- 0.04
    for (i in length(xxx):1) {
      x <- 0; y <- 0
      radius <- i/length(xxx) + radiusStart
      colorCircle <- temp$color[i]
      draw.circle(x, y, radius, nv = 100,
                  border = colorCircle, col = colorCircle,
                  lty = 1, angle = 45, lwd = 1)
    }
    
    alpha <- 0.04
    graphics::rect(0 - alpha, -radiusStart - 0.001, 10, radiusStart + 0.001,
                   col = "gray20", border = NA)
    
    if (length(temp$date) >= 1) {
      t <- seq_len(length(temp$date))
      t <- unique(c(min(t), stats::median(t), max(t)))
      for (i in t) {
        graphics::text(i/length(xxx) + radiusStart, 0, paste(temp$date[i]),
                       cex = 1.5, col = temp$color[i])
      }
    }
    
    title(title, col.main = "white", cex.main = 1.8, line = -2.5, font.main = 2)
    
  } else {
    
    # ====================
    # Linear stripes plot
    # ====================
    grDevices::png(filename = file.path(out_dir, outfile_name),
                   res = 72 * resfactor, height = 640 * resfactor, width = 1140 * resfactor)
    
    graphics::par(bg = "black", xaxs = "i")  # disable axis padding
    
    # Compute safe y-limits with fallback padding
    ylim_try <- range(dum_dat, na.rm = TRUE)
    if (!all(is.finite(ylim_try))) ylim_try <- c(minT, maxT)
    if (!all(is.finite(ylim_try)) || diff(ylim_try) == 0) {
      center <- if (is.finite(minT)) minT else 0
      half   <- max(1, abs(center) * 0.05)
      ylim_try <- c(center - half, center + half)
    }
    
    # Plot using index-based x-axis (1..N) → all stripes equally wide
    x_idx <- seq_along(date_info_1)
    
    graphics::plot(x = x_idx, y = dum_dat,
                   ylab = "", xlab = "", type = "n", axes = FALSE,
                   ylim = ylim_try, xlim = c(0.5, length(x_idx) + 0.5))
    
    # Define color palette
    nBins <- 10
    palette <- rev(RColorBrewer::brewer.pal(nBins, "RdBu"))
    if (color_pal == 2) {
      palette <- grDevices::colorRampPalette(c(
        "#474747", "#7a7a7a", "#a8a8a8", "#cdcdcd", "#e2e2e2", "#f9f9f9",
        "#fdf3db", "#fee8b2", "#fedf8c", "#fed66c", "#fdcf45", "#fac631"
      ))(nBins)
    }
    if (color_pal == 3) {
      palette <- grDevices::colorRampPalette(c(
        "#5c3209", "#96560e", "#b27028", "#d1a759",
        "#dfc07a", "#f5e5bf", "#fefefe", "#b0dfda",
        "#6fc0b8", "#389c94", "#078470", "#045f5a", "#0f3c33"
      ))(nBins)
    }
    
    # Bin assignment function for temperature/value mapping
    rangeT  <- maxT - minT
    binWidth <- if (is.finite(rangeT) && rangeT > 0) rangeT / nBins else 1
    binCol <- function(Temp) {
      idx <- floor((Temp - minT) / binWidth) + 1
      if (!is.finite(idx) || idx < 1) idx <- 1
      if (idx > nBins) idx <- nBins
      palette[idx]
    }
    
    # Draw full-height stripes with minimal overlap to avoid thin black lines
    y0 <- ylim_try[1]
    y1 <- ylim_try[2]
    epsilon <- 0.01  # small overlap between stripes (in index units)
    dum_plot <- dum_dat
    dum_plot[!is.finite(dum_plot)] <- minT
    
    for (i in seq_along(x_idx)) {
      col_here <- binCol(dum_plot[i])
      graphics::rect(i - 0.5 - epsilon, y0, i + 0.5 + epsilon, y1,
                     col = col_here, border = NA, lwd = 0)
    }
    
    # Add main title
    title(title, col.main = "white", cex.main = 2.0, line = 0.2, font.main = 2)
    
    # X-axis labels: use years (mapped to evenly spaced stripe indices)
    xlabel <- rep(NA, length(date_info_1))
    nlab   <- if (length(xlabel) >= 4) round(seq(1, length(xlabel), length.out = 4)) else seq_along(xlabel)
    xlabel[nlab] <- format(date_info_1[nlab], "%Y")
    if (any(!is.na(xlabel))) {
      graphics::mtext(xlabel, side = 1, line = -0.5, at = x_idx, cex = 1.5, col = "white")
    }
    
    # Optional: draw overlay points or trend line
    if (isTRUE(pointsTF)) graphics::points(x_idx, dum_dat, pch = 21, bg = "white")
    if (isTRUE(lineTF) && any(is.finite(dum_dat)) && length(dum_dat) >= 2) {
      graphics::abline(stats::lm(dum_dat ~ x_idx), col = "white")
    }
    
    grDevices::dev.off()
  }
  
  # ===============================
  # Monitor climate / ranking stats
  # ===============================
  tmp_climate_dir <- file.path(tempdir(), "tmp_climate_dir")
  if (dir.exists(tmp_climate_dir)) unlink(tmp_climate_dir, recursive = TRUE)
  dir.create(tmp_climate_dir, showWarnings = FALSE)
  
  # Climatology mean value (field mean over climatology file)
  tmp_clim_mean_value <- file.path(tmp_climate_dir, "tmp_clim_mean_value.nc")
  cmsafops::fldmean(var = variable, infile = climatology_file,
                    outfile = tmp_clim_mean_value, overwrite = TRUE)
  nc_m <- ncdf4::nc_open(tmp_clim_mean_value)
  dum_dat_mean <- ncdf4::ncvar_get(nc_m, variable, collapse_degen = FALSE)
  ncdf4::nc_close(nc_m)
  
  years_all  <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$years
  months_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$months
  days_all   <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$days
  
  ranking <- data.frame(years_all, months_all, days_all, as.vector(dum_dat))
  names(ranking) <- c("Year", "Month", "Day","Value")
  
  titles <- c("Analyzed years", "Climatology Mean Value", "Maximum", "Minimum")
  
  ordered_index_dataT <- order(dataT[['dum_dat']])
  ordered_dataT <- dataT[ordered_index_dataT, , drop = FALSE]
  row.names(ordered_dataT) <- NULL
  
  standout_years <- c(
    paste0(climate_year_start, " - " , format(end_date, "%Y")),
    paste(climate_year_start, climate_year_end, sep = " - "),
    if (nrow(ordered_dataT) > 0) as.character(as.Date(ordered_dataT[nrow(ordered_dataT), "date_info"], origin = "1970-01-01")) else NA_character_,
    if (nrow(ordered_dataT) > 0) as.character(as.Date(ordered_dataT[1, "date_info"], origin = "1970-01-01")) else NA_character_
  )
  
  standout_values <- c(
    toString(mean(dum_dat, na.rm = TRUE)),
    mean(dum_dat_mean, na.rm = TRUE),
    if (nrow(ordered_dataT) > 0) toString(ordered_dataT[nrow(ordered_dataT), "dum_dat"]) else NA_character_,
    if (nrow(ordered_dataT) > 0) toString(ordered_dataT[1, "dum_dat"]) else NA_character_
  )
  
  final_values <- data.frame(title = titles, years = standout_years, value = standout_values)
  calc.parameters.monitor.climate(final_values, ranking[order(ranking$Value),])
  
  if (dir.exists(tmp_climate_dir)) unlink(tmp_climate_dir, recursive = TRUE)
}
