#' Creating a simple histogram.
#'
#' This routine was implemented for creating histograms in the CM SAF R Toolbox.
#'
#' @inheritParams render_plot
#' @param dastat Statistics given to hist plot (numeric).
#' @param shortDescription A title will be generated using "Histogram of" + description (character).
#' @param xlab Label for x axis (character).
#'
#' @export
render_hist_plot <- function(dastat,
                             shortDescription,
                             grid_col,
                             bordercolor,
                             linesize,
                             xlab) {
  assertthat::assert_that(is.numeric(dastat))
  assertthat::assert_that(is.character(shortDescription))
  assertthat::assert_that(is.character(xlab))

  # Provide that not all values are NA
  dastat <- dastat[!is.na(dastat)]
  min_max <- range(dastat)
  assertthat::is.number(min_max[1])
  assertthat::is.number(min_max[2])

  graphics::hist(dastat, main = paste0("Histogram of ", shortDescription),
                 xlab = xlab, col = grDevices::rgb(91, 127, 149, maxColorValue = 255))
  graphics::rect(graphics::par("usr")[1],
                 graphics::par("usr")[3],
                 graphics::par("usr")[2],
                 graphics::par("usr")[4],
                 col = "light grey")
  graphics::grid(NULL, NULL, lty = 3, col = grid_col, lwd = 1.5)
  graphics::hist(dastat, col = grDevices::rgb(91, 127, 149, maxColorValue = 255), add = TRUE)
  graphics::box(col = bordercolor, lwd = linesize)
}
