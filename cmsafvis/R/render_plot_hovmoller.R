#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a Hovmöller plot of two variables.
#'
#' @inheritParams render_plot
#' @param title_data1 Character. Title of the first data set.
#' @param title_data2 Character. Title of the second data set.
#' @param nc  Alternatively to `infile1` you can specify the input as an
#'   object of class `ncdf4` (as returned from `ncdf4::nc_open`).
#' @param nc2 Alternatively to `infile2` you can specify the input as an
#'   object of class `ncdf4` (as returned from `ncdf4::nc_open`).
#'
#' @export
render_plot_hovmoller <- function(outfile = NULL,
                                fileExtension = ".png",
                                visualizeVariables,
                                imagewidth,
                                imageheight,
                                textsize,
                                linesize,
                                title_data1,
                                title_data2,
                                nc = NULL,
                                nc2 = NULL) {
  if (!is.null(nc)) visualizeVariables$file_name <- nc$filename
  if (!is.null(nc2)) visualizeVariables$file_name2 <- nc2$filename
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  suppressWarnings({
    # In the following textsize, and linesize can be found in global.R
    iwidth  <- imagewidth * 2
    iheight <- imageheight * 2
    grDevices::png(outfile, width = iwidth, height = iheight)
    graphics::par(cex = textsize)
    
    stack_var_1 <- raster::stack(visualizeVariables$file_name)
    stack_var_1_z <- raster::setZ(stack_var_1, visualizeVariables$date.time)
    
    
    stack_var_2 <- raster::stack(visualizeVariables$file_name2)
    stack_var_2_z <- raster::setZ(stack_var_2, visualizeVariables$date.time)
    
    x <- NULL
    y <- NULL
    p1 <- rasterVis::hovmoller(stack_var_1_z, dirXY=x, xlab='Longitude', main=title_data1, par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
    p2 <- rasterVis::hovmoller(stack_var_1_z, dirXY=y, xlab='Latitude', main=title_data1, par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
    
    p3 <- rasterVis::hovmoller(stack_var_2_z, dirXY=x, xlab='Longitude', main=title_data2, par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
    p4 <- rasterVis::hovmoller(stack_var_2_z, dirXY=y, xlab='Latitude', main=title_data2, par.settings=rasterVis::rasterTheme(region=c("blue", "yellow", "red")))
    gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
    
    on.exit(grDevices::dev.off())
  })
  return(
    list(
      src = outfile,
      contentType = getMimeType(outfile),
      width = iwidth,
      height = iheight,
      alt = "Hovmoller plot"
    )
  )
}