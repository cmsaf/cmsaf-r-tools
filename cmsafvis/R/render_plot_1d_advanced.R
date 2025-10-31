#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a 1D plot of data at one single lon or one single lat point.
#'
#' @inheritParams render_plot
#' @param ticknumber Number of ticks (numeric).
#' @param addTrend Whether to add a trend line (logical).
#' @param sliderx Limiting the time series with a two valued vector for min and max (numeric).
#' @param slidery Limiting the y axis with a two valued vector for min and max (numeric).
#' @param checkGroup_type Integer in 1-5. See Details. (numeric)
#'
#' @details
#' \describe{
#'   \item{1}{Line}
#'   \item{2}{Points}
#'   \item{3}{Line and Points}
#'   \item{4}{Steps}
#'   \item{5}{Histogram}
#' }
#' @param text1_1d Title text (character).
#' @param text2_1d Text to be passed to graphics::mtext (character).
#' @param col A color chosen via colourpicker::colourInput.
#' @param timestep_1d_visualize The time step to be visualized.
#'
#' @export
render_plot_1d_advanced <- function(outfile = NULL,
                           fileExtension = ".png",
                           visualizeVariables,
                           ticknumber,
                           addTrend,
                           sliderx,
                           slidery,
                           checkGroup_type,
                           imagewidth,
                           imageheight,
                           text1_1d,
                           text2_1d,
                           textsize,
                           linesize,
                           col,
                           timestep_1d_visualize) {
  # TBD: add trend
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  
  if(length(visualizeVariables$lon) == 1 && length(visualizeVariables$lat != 1)){   # plot for x-axis: lat and y-axis: data
    x_plot <- visualizeVariables$lat
    x_label <- "Latitude"
  } else if((length(visualizeVariables$lon) != 1 && length(visualizeVariables$lat) == 1)){   # plot for x-axis: lon and y-axis: data
    x_plot <- visualizeVariables$lon
    x_label <- "Longitude"
  }
  
  # prepare ticks
  dum_tick <-
    seq(1, length(x_plot), length.out = ticknumber)
  dum_tick2 <- NULL
  for (j in 2:length(dum_tick)) {
    dummy <- seq(dum_tick[j - 1], dum_tick[j], length.out = 4)
    if (j > 2 & j != length(dum_tick))
      (dummy <- dummy[2:4])
    dum_tick2 <- append(dum_tick2, dummy)
  }
  
  # plot type
  plotting_type <- c("l", "p", "o", "s", "h")
  
  # In the following textsize, and linesize can be found in global.R
  iwidth  <- imagewidth
  iheight <- imageheight
  grDevices::png(outfile, width = iwidth, height = iheight)
  graphics::par(cex = textsize)
  
  graphics::plot(
    x_plot,
    visualizeVariables$data[,,which(visualizeVariables$date.time == timestep_1d_visualize)],
    type = plotting_type[strtoi(checkGroup_type)],
    xlim = x_plot[sliderx],
    ylim = slidery,
    col = "white",
    main = text1_1d,
    xlab = x_label,
    ylab = visualizeVariables$ylabel,
    axes = FALSE
  )
  
  graphics::abline(h = 0, lwd = 1, col = "gray")
  graphics::grid(NA, NULL, lwd = 0.8)
  graphics::abline(
    v = x_plot,
    col = "lightgray",
    lty = 3,
    lwd = 0.8
  )
  graphics::points(
    x_plot,
    visualizeVariables$data[,,which(visualizeVariables$date.time == timestep_1d_visualize)],
    type = plotting_type[strtoi(checkGroup_type)],
    xlim = x_plot[sliderx],
    ylim = slidery,
    col = col,
    lwd = linesize
  )
  graphics::axis(
    side = 1,
    at = x_plot[dum_tick],
    tck = -0.025,
    col.ticks = "gray20",
    col.axis = "gray20"
  )
  graphics::axis(
    side = 1,
    at = x_plot[dum_tick],
    tck = 0.015,
    col.ticks = "gray20",
    col.axis = "gray20"
  )
  graphics::rug(
    x = x_plot[dum_tick2],
    ticksize = 0.015,
    side = 1,
    quiet = TRUE
  )
  graphics::axis(
    side = 2,
    tck = -0.025,
    col.ticks = "gray20",
    col.axis = "gray20"
  )
  graphics::axis(
    side = 2,
    tck = 0.015,
    col.ticks = "gray20",
    col.axis = "gray20",
    labels = FALSE
  )
  graphics::box(col = "gray20", lwd = 1)
  graphics::mtext(text2_1d)
  on.exit(grDevices::dev.off())
  
  # Return a list containing the filename
  return(
    list(
      src = outfile,
      contentType = getMimeType(outfile),
      width = iwidth,
      height = iheight,
      alt = "This is alternate text"
    )
  )
}
