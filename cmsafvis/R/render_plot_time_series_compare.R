#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function provides a time series comparison of two data sets.
#'
#' @inheritParams render_plot
#' @param ticknumber Number of ticks (numeric).
#' @param dateformat Date format for constructing a date label.
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
#' @param legend_label1 Legend label of the first data set
#' @param legend_label2 Legend label of the second data set
#' @param station_number For station data compare; which station is selected
#'
#' @export
render_plot_time_series_compare <- function(outfile = NULL,
                                    fileExtension = ".png",
                                    visualizeVariables,
                                    ticknumber,
                                    dateformat,
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
                                    legend_label1,
                                    legend_label2,
                                    station_number) {
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  
  if(is.data.frame(visualizeVariables$data2)){   # second input file is a csv or RData file
    suppressWarnings({
      
      labs <- NULL
      for (i in seq_along(visualizeVariables$data2$lon)) {
        dummy <- paste0("[", round(visualizeVariables$data2$lon[i], digits = 1), ";", round(visualizeVariables$data2$lat[i], digits = 1), "]")
        labs <- append(labs, dummy)
      }
      station_seq <- unique(labs)
      station_number <- match(station_number, station_seq)
      station_number <- as.numeric(station_number)
      
      list_data_station <- visualizeVariables$data3
     
      # prepare ticks and date formats
      dum_tick <-
        seq(1, length(visualizeVariables$date.time), length.out = ticknumber)
      dum_tick2 <- NULL
      for (j in 2:length(dum_tick)) {
        dummy <- seq(dum_tick[j - 1], dum_tick[j], length.out = 4)
        if (j > 2 & j != length(dum_tick))
          (dummy <- dummy[2:4])
        dum_tick2 <- append(dum_tick2, dummy)
      }
      
      date.lab <- switch(
        as.numeric(dateformat),
        format(visualizeVariables$date.time[dum_tick], "%Y"),
        format(visualizeVariables$date.time[dum_tick], "%Y-%m"),
        format(visualizeVariables$date.time[dum_tick], "%Y-%m-%d"),
        format(visualizeVariables$date.time[dum_tick], "%Y-%m-%d %R")
      )
      
      # plot type
      plotting_type <- c("l", "p", "o", "s", "h")
      
      # In the following textsize, and linesize can be found in global.R
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      
      # data one
      graphics::plot(
        visualizeVariables$date.time,
        list_data_station[[station_number]]$data_sat,
        type = plotting_type[strtoi(checkGroup_type)],
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = "time",
        ylab = visualizeVariables$ylabel,
        axes = FALSE
      )
      graphics::par(new=TRUE)
      # data two
      graphics::plot(
        visualizeVariables$date.time,
        list_data_station[[station_number]]$data_station,
        #type = plotting_type[strtoi(checkGroup_type)],
        type = "l",
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = "time",
        ylab = visualizeVariables$ylabel,
        axes = FALSE
      )
      
      graphics::abline(h = 0, lwd = 1, col = "gray")
      graphics::grid(NA, NULL, lwd = 0.8)
      graphics::abline(
        v = visualizeVariables$date.time,
        col = "lightgray",
        lty = 3,
        lwd = 0.8
      )
      
      # data one 
      graphics::points(
        visualizeVariables$date.time,
        list_data_station[[station_number]]$data_sat,
        #type = plotting_type[strtoi(checkGroup_type)],
        type = "l",
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        #col = col,
        col = "blue",
        lwd = linesize
      )
      
      # data two
      graphics::points(
        visualizeVariables$date.time,
        list_data_station[[station_number]]$data_station,
        #type = plotting_type[strtoi(checkGroup_type)],
        type = "l",
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        col = "red",
        lwd = linesize
      )
      
      graphics::axis(
        side = 1,
        at = visualizeVariables$date.time[dum_tick],
        labels = date.lab,
        tck = -0.025,
        col.ticks = "gray20",
        col.axis = "gray20"
      )
      
      graphics::axis(
        side = 1,
        at = visualizeVariables$date.time[dum_tick],
        labels = FALSE,
        tck = 0.015,
        col.ticks = "gray20",
        col.axis = "gray20"
      )
      graphics::rug(
        x = visualizeVariables$date.time[dum_tick2],
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
      leg.txt <- c(legend_label1, legend_label2)
      graphics::legend("topright", leg.txt, pch = 15, 
                       col = c("blue", "red"),
                       cex = textsize)
      graphics::box(col = "gray20", lwd = 1)
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    })
  } else {
    suppressWarnings({
      # prepare ticks and date formats
      dum_tick <-
        seq(1, length(visualizeVariables$date.time), length.out = ticknumber)
      dum_tick2 <- NULL
      for (j in 2:length(dum_tick)) {
        dummy <- seq(dum_tick[j - 1], dum_tick[j], length.out = 4)
        if (j > 2 & j != length(dum_tick))
          (dummy <- dummy[2:4])
        dum_tick2 <- append(dum_tick2, dummy)
      }
      
      date.lab <- switch(
        as.numeric(dateformat),
        format(visualizeVariables$date.time[dum_tick], "%Y"),
        format(visualizeVariables$date.time[dum_tick], "%Y-%m"),
        format(visualizeVariables$date.time[dum_tick], "%Y-%m-%d"),
        format(visualizeVariables$date.time[dum_tick], "%Y-%m-%d %R")
      )
    
      # plot type
      plotting_type <- c("l", "p", "o", "s", "h")
      
      # In the following textsize, and linesize can be found in global.R
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      
      # data one
      graphics::plot(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = plotting_type[strtoi(checkGroup_type)],
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = "time",
        ylab = visualizeVariables$ylabel,
        axes = FALSE
      )
      graphics::par(new=TRUE)
      # data two
      graphics::plot(
        visualizeVariables$date.time,
        visualizeVariables$data2,
        #type = plotting_type[strtoi(checkGroup_type)],
        type = "l",
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = "time",
        ylab = visualizeVariables$ylabel,
        axes = FALSE
      )
      
      graphics::abline(h = 0, lwd = 1, col = "gray")
      graphics::grid(NA, NULL, lwd = 0.8)
      graphics::abline(
        v = visualizeVariables$date.time,
        col = "lightgray",
        lty = 3,
        lwd = 0.8
      )
      
      # data one 
      graphics::points(
        visualizeVariables$date.time,
        visualizeVariables$data,
        #type = plotting_type[strtoi(checkGroup_type)],
        type = "l",
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        #col = col,
        col = "blue",
        lwd = linesize
      )
      
      # data two
      graphics::points(
        visualizeVariables$date.time,
        visualizeVariables$data2,
        #type = plotting_type[strtoi(checkGroup_type)],
        type = "l",
        #xlim = visualizeVariables$date.time[sliderx],
        #ylim = slidery,
        col = "red",
        lwd = linesize
      )
      
      graphics::axis(
        side = 1,
        at = visualizeVariables$date.time[dum_tick],
        labels = date.lab,
        tck = -0.025,
        col.ticks = "gray20",
        col.axis = "gray20"
      )
    
      graphics::axis(
        side = 1,
        at = visualizeVariables$date.time[dum_tick],
        labels = FALSE,
        tck = 0.015,
        col.ticks = "gray20",
        col.axis = "gray20"
      )
      graphics::rug(
        x = visualizeVariables$date.time[dum_tick2],
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
      leg.txt <- c(legend_label1, legend_label2)
      graphics::legend("topright", leg.txt, pch = 15, 
                       col = c("blue", "red"),
                       cex = textsize)
      graphics::box(col = "gray20", lwd = 1)
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    })
  }
  # Return a list containing the filename
  return(
    list(
      src = outfile,
      contentType = getMimeType(outfile),
      width = iwidth,
      height = iheight,
      alt = "Time series plot"
    )
  )
}
