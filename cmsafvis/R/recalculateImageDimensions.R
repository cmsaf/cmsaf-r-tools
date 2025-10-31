#' Get rectangular image dimensions
#'
#' Given regional bounds image width and height are computed in order to display a region without distortion.
#'
#' @param visualizeVariables A dataframe containing $lon and $lat values which will be bounded by lon/lat_bounds (data.frame).
#' @param lon_bounds Array containing two values for longitude min and max (numeric).
#' @param lat_bounds Array containing two values for latitude min and max (numeric).
#' @param image_def Minimal image default size for width and height (numeric).
#' @param ihsf Image height rescaling factor (numeric).
#'
#' @export
recalculateImageDimensions <- function(visualizeVariables,
                                       lon_bounds,
                                       lat_bounds,
                                       image_def,
                                       ihsf){

  lon <- visualizeVariables$lon[visualizeVariables$lon <= lon_bounds[2]]
  lon <- lon[lon_bounds[1] <= lon]

  lat <- visualizeVariables$lat[visualizeVariables$lat <= lat_bounds[2]]
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
  return(list(imagewidth = imagewidth, imageheight = imageheight))
}
