#' Creates a preview plot of a selected area
#'
#' This function creates a simple preview plot via maps::map of a given region.
#'
#' @param spatial_lon_range Array containing two values for longitude min and max (numeric).
#' @param spatial_lat_range Array containing two values for latitude min and max (numeric).
#' @param lonRange Array containing two values for longitude min and max (numeric).
#' @param latRange Array containing two values for latitude min and max (numeric).
#'
#' @export
render_preview_plot <- function(spatial_lon_range,
                                spatial_lat_range,
                                lonRange,
                                latRange) {
  x_lims <- spatial_lon_range + c(-10, 10)
  y_lims <- spatial_lat_range + c(-10, 10)

  maps::map("world", fill = TRUE, col = "gray36", bg = "white", xlim = x_lims, ylim = y_lims)
  graphics::rect(lonRange[1], latRange[1], lonRange[2], latRange[2], lwd = 3, col = "brown4", density = 0)
  graphics::rect(lonRange[1], latRange[1], lonRange[2], latRange[2], lwd = 0.5, angle = 36, col = "brown4", density = 30)
  graphics::title(main = "Preview of available spatial coverage")
}
