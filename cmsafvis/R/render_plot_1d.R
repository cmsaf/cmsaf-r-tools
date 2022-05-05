#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a 1D plot of data at one single lon / lat point.
#'
#' @inheritParams render_plot
#' @param ticknumber Number of ticks (numeric).
#' @param dateformat Date format for constructing a date label.
#' @param analyze_timeseries Whether or not to analyze the timeseries of the given point (logical).
#' @param addTrend Whether to add a trend line (logical).
#' @param sliderx Limiting the time series with a two valued vector for min and max (numeric).
#' @param slidery Limiting the y axis with a two valued vector for min and max (numeric).
#' @param checkGroup_type An integer between 1 and 5 indicating group type (numeric).
#' 1 for Line, 2 for Points, 3 for Line and Points, 4 for steps, 5 for histogram.
#' @param text1_1d Title text (character).
#' @param text2_1d Text to be passed to graphics::mtext (character).
#' @param text3_1d X-label (character).
#' @param text4_1d Y-label (character).
#' @param col A color chosen via colourpicker::colourInput.
#'
#' @export
render_plot_1d <- function(outfile = NULL,
                           fileExtension = ".png",
                           visualizeVariables,
                           ticknumber,
                           dateformat,
                           analyze_timeseries,
                           addTrend,
                           sliderx,
                           slidery,
                           checkGroup_type,
                           imagewidth,
                           imageheight,
                           text1_1d,
                           text2_1d,
                           text3_1d,
                           text4_1d,
                           textsize,
                           linesize,
                           col) {
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  
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

  if (as.logical(analyze_timeseries)) {
    # Set the size of the output window
    iwidth  <- 650
    iheight <- 800

    #different Output formats
    grDevices::png(outfile, width = iwidth, height = iheight)
    # Analyze Timeseries

    # Set the number of rows and columns
    nrow <- 3
    ncol <- 2

    field <- visualizeVariables$data[, , sliderx[1]:sliderx[2]]
    date.time2 <-
      visualizeVariables$date.time[sliderx[1]:sliderx[2]]

    # Create vectors of the months and years, respectively
    timemonth <- format(date.time2, "%m")
    timeyear <- format(date.time2, "%Y")
    nt <- length(date.time2)

    # Create vectors of the months and years, respectively
    timemonth.in <- format(date.time2, "%m")
    timeyear.in <- format(date.time2, "%Y")
    startyear <- timeyear.in[1]
    startmonth <- timemonth.in[1]

    title <- visualizeVariables$varname
    varlabel <- paste0(title, " [", visualizeVariables$unit, "]")

    # The function tapply is very useful for operations that
    # operate along the time axis, e.g., calculating mean monthly values
    field.monmean <- tapply(field, timemonth, mean, na.rm = TRUE)
    field.monmax <- tapply(field, timemonth, max, na.rm = TRUE)
    field.monmin <- tapply(field, timemonth, min, na.rm = TRUE)
    field.monsd <- tapply(field, timemonth, stats::sd, na.rm = TRUE)

    # Annual mean is only calculated if data for all 12 month are available
    field.annmean <- tapply(field, timeyear, mean, na.rm = TRUE)
    # Include only those years with 12 months on data
    years <- dimnames(field.annmean)[[1]]
    nyears <- length(years)
    nmonth <- vector(mode = "numeric", length = nyears)
    for (i in 1:nyears) {
      nmonth[i] <- length(which(timeyear == years[i]))
    }
    ind <- which(nmonth < 12)
    field.annmean[ind] <- NA

    # Calculate anomalies
    field.ano <- vector(mode = "numeric", length = nt)
    field.relano <- vector(mode = "numeric", length = nt)
    for (j in 1:nt) {
      field.ano[j] <- field[j] - field.monmean[timemonth[j]]
      field.relano[j] <- field.ano[j] / field.monsd[timemonth[j]]
    }

    # set the number of rows and columns of the plot
    graphics::par(mfrow = c(nrow, ncol))

    # Determine the min and max of the plotrange
    pmin <- min(field, na.rm = TRUE)
    pmax <- max(field, na.rm = TRUE)
    drange <- pmax - pmin

    #plot the data
    graphics::plot(
      date.time2,
      field,
      ylab = varlabel,
      xlab = "",
      main = paste0(
        title,
        ", ",
        format(
          visualizeVariables$lat,
          digits = 4,
          nsmall = 2
        ),
        " N, ",
        format(
          visualizeVariables$lon,
          digits = 4,
          nsmall = 2
        ),
        " E"
      ),
      ylim = c(pmin, pmax),
      type = "l"
    )

    # calculate the linear trend of the original data
    x <- c(1:nt)
    model <- stats::lm(field ~ x, na.action = stats::na.exclude)
    graphics::lines(date.time2,
                    stats::predict(model),
                    col = col,
                    lwd = 2.0)
    conf <- stats::confint(model, "x", level = 0.95)
    trend <- model$coeff["x"] * 12.
    lconf <- conf[1] * 12.
    uconf <- conf[2] * 12

    mean.out <-
      format(mean(field, na.rm = TRUE),
             digits = 3,
             nsmall = 1)
    trend.out <-
      paste0(
        "[",
        format(lconf, digits = 2, nsmall = 2),
        ",",
        format(trend, digits = 2, nsmall = 2),
        ",",
        format(uconf, digits = 2, nsmall = 2),
        "] "
      )

    # Determine the x-location of the text in date format
    xtext <- as.Date(paste(startyear, startmonth, 01, sep = "-"))

    # textsize can be found in global.R
    graphics::text(
      xtext,
      pmax - 0.01 * drange,
      paste("mean:", mean.out, visualizeVariables$unit),
      pos = 4,
      cex = textsize
    )
    graphics::text(
      xtext,
      pmax - 0.08 * drange,
      paste0("linear trend:", trend.out, visualizeVariables$unit, "/yr"),
      pos = 4,
      cex = textsize
    )

    #--------------------------------------------------
    #Plot the monthly mean seasonal cycle
    # Changed this line (used to be months <- 1:12 but they don't always all exist.)
    months <- unique(names(field.monmax))
    # Determine the min and max of the plotrange
    pmin <- min(field.monmin, na.rm = TRUE)
    pmax <- max(field.monmax, na.rm = TRUE)

    graphics::plot(
      months,
      field.monmax,
      type = "n",
      main = "Average Seasonal Cycle",
      ylab = varlabel,
      xlab = "Months",
      ylim = c(pmin, pmax)
    )
    graphics::lines(months, field.monmax)
    graphics::lines(months, field.monmin)
    graphics::polygon(c(months, rev(months)), c(field.monmin, rev(field.monmax)), col =
                        col)
    graphics::lines(months, field.monmean, lwd = 2)

    #--------------------------------------------------
    # Plot the monthly anomalies
    pmin <- min(field.ano, na.rm = TRUE)
    pmax <-
      max(field.ano, na.rm = TRUE) + 0.15 * (max(field.ano, na.rm = TRUE) - min(field.ano, na.rm =
                                                                                  TRUE))
    drange <- pmax - pmin
    graphics::plot(
      date.time2,
      field.ano,
      ylab = varlabel,
      xlab = "",
      main = "Monthly anomalies",
      ylim = c(pmin, pmax),
      type = "l"
    )
    graphics::abline(h = 0, lwd = 1.0, col = "gray40")

    # calculate the linear trend of the anomaly data
    x <- c(1:nt)
    model <- stats::lm(field.ano ~ x, na.action = stats::na.exclude)
    graphics::lines(date.time2,
                    stats::predict(model),
                    col = col,
                    lwd = 2.0)
    conf <- stats::confint(model, "x", level = 0.95)
    trend <- model$coeff["x"] * 12.
    lconf <- conf[1] * 12.
    uconf <- conf[2] * 12

    trend.out <-
      paste0(
        "[",
        format(lconf, digits = 2, nsmall = 2),
        ",",
        format(trend, digits = 2, nsmall = 2),
        ",",
        format(uconf, digits = 2, nsmall = 2),
        "] "
      )

    # Determine the x-location of the text in date format
    xtext <- as.Date(paste(startyear, startmonth, 01, sep = "-"))

    graphics::text(
      xtext,
      pmax - 0.01 * drange,
      paste0("linear trend:", trend.out, visualizeVariables$unit, "/yr"),
      pos = 4,
      cex = textsize
    )


    #--------------------------------------------------
    # Boxplot of the time series
    # Define the months as a categorical variable
    month_cat <- factor(timemonth)
    graphics::plot(month_cat,
                   field,
                   main = "Box Plot",
                   ylab = varlabel,
                   xlab = "Months")

    #--------------------------------------------------
    # Plot the annual means
    graphics::plot(
      as.integer(years) + 0.5,
      field.annmean,
      type = "p",
      main = "Annual Means",
      ylab = varlabel,
      xlab = "",
      pch = 19
    )
    graphics::abline(
      h = mean(field.annmean, na.rm = TRUE),
      lwd = 1.0,
      col = "gray40"
    )

    #--------------------------------------------------
    # Plot a histogram of the data
    graphics::hist(
      field,
      breaks = 20,
      xlab = varlabel,
      main = paste0("Histogram of ", title),
      col = col
    )
    graphics::box(col = "gray20", lwd = 1)

    on.exit(grDevices::dev.off())
  } else {
    # Generate the PNG with different line types
    if(visualizeVariables$plot_type %in% c("fldcor", "fldcovar")){
      if(visualizeVariables$plot_type == "fldcor")
        y_label <- "Correlation in grid space"
      if(visualizeVariables$plot_type == "fldcovar")
        y_label <- "Covariance in grid space"
    } else
    {
      y_label <- visualizeVariables$ylabel
    }
    # In the following textsize, and linesize can be found in global.R
    if (checkGroup_type == 1) {
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      graphics::plot(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "l",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = text3_1d,
        ylab = text4_1d,
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
      graphics::points(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "l",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = col,
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
      graphics::box(col = "gray20", lwd = 1)
      if (as.logical(addTrend)) {
        # calculate the linear trend
        x <-
          c(seq_along(visualizeVariables$date.time[sliderx[1]:sliderx[2]]))
        model <-
          stats::lm(visualizeVariables$data[sliderx[1]:sliderx[2]] ~ x, na.action = stats::na.exclude)
        graphics::lines(
          visualizeVariables$date.time[sliderx[1]:sliderx[2]],
          stats::predict(model),
          col = "gray20",
          lwd = linesize
        )
        conf <- stats::confint(model, "x", level = 0.95)
        trend <- model$coeff["x"] * 12.
        lconf <- conf[1] * 12.
        uconf <- conf[2] * 12

        mean.out <-
          format(
            mean(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE),
            digits = 3,
            nsmall = 1
          )
        trend.out <-
          paste0(
            "[",
            format(lconf, digits = 2, nsmall = 2),
            ",",
            format(trend, digits = 2, nsmall = 2),
            ",",
            format(uconf, digits = 2, nsmall = 2),
            "] "
          )
        xtext <- as.Date(visualizeVariables$date.time[sliderx[1]])

        pmin <-
          min(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        pmax <-
          max(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        drange <- pmax - pmin
        graphics::text(
          xtext,
          pmax - 0.01 * drange,
          paste("mean:", mean.out, visualizeVariables$unit),
          pos = 4,
          cex = textsize
        )
        graphics::text(
          xtext,
          pmax - 0.08 * drange,
          paste0(
            "linear trend:",
            trend.out,
            visualizeVariables$unit,
            "/yr"
          ),
          pos = 4,
          cex = textsize
        )
      }
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    }

    if (checkGroup_type == 2) {
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      graphics::plot(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "p",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = text3_1d,
        ylab = text4_1d,
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
      graphics::points(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "p",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = col,
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
      graphics::box(col = "gray20", lwd = 1)
      if (as.logical(addTrend)) {
        # calculate the linear trend
        x <-
          c(seq_along(visualizeVariables$date.time[sliderx[1]:sliderx[2]]))
        model <-
          stats::lm(visualizeVariables$data[sliderx[1]:sliderx[2]] ~ x, na.action = stats::na.exclude)
        graphics::lines(
          visualizeVariables$date.time[sliderx[1]:sliderx[2]],
          stats::predict(model),
          col = "gray20",
          lwd = linesize
        )
        conf <- stats::confint(model, "x", level = 0.95)
        trend <- model$coeff["x"] * 12.
        lconf <- conf[1] * 12.
        uconf <- conf[2] * 12

        mean.out <-
          format(
            mean(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE),
            digits = 3,
            nsmall = 1
          )
        trend.out <-
          paste0(
            "[",
            format(lconf, digits = 2, nsmall = 2),
            ",",
            format(trend, digits = 2, nsmall = 2),
            ",",
            format(uconf, digits = 2, nsmall = 2),
            "] "
          )
        xtext <- as.Date(visualizeVariables$date.time[sliderx[1]])

        pmin <-
          min(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        pmax <-
          max(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        drange <- pmax - pmin
        graphics::text(
          xtext,
          pmax - 0.01 * drange,
          paste("mean:", mean.out, visualizeVariables$unit, sep = " "),
          pos = 4,
          cex = textsize
        )
        graphics::text(
          xtext,
          pmax - 0.08 * drange,
          paste0(
            "linear trend:",
            trend.out,
            visualizeVariables$unit,
            "/yr"
          ),
          pos = 4,
          cex = textsize
        )
      }
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    }

    if (checkGroup_type == 3) {
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      graphics::plot(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "o",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = text3_1d,
        ylab = text4_1d,
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
      graphics::points(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "o",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = col,
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
      graphics::box(col = "gray20", lwd = 1)
      if (as.logical(addTrend)) {
        # calculate the linear trend
        x <-
          c(seq_along(visualizeVariables$date.time[sliderx[1]:sliderx[2]]))
        model <-
          stats::lm(visualizeVariables$data[sliderx[1]:sliderx[2]] ~ x, na.action = stats::na.exclude)
        graphics::lines(
          visualizeVariables$date.time[sliderx[1]:sliderx[2]],
          stats::predict(model),
          col = "gray20",
          lwd = linesize
        )
        conf <- stats::confint(model, "x", level = 0.95)
        trend <- model$coeff["x"] * 12.
        lconf <- conf[1] * 12.
        uconf <- conf[2] * 12

        mean.out <-
          format(
            mean(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE),
            digits = 3,
            nsmall = 1
          )
        trend.out <-
          paste0(
            "[",
            format(lconf, digits = 2, nsmall = 2),
            ",",
            format(trend, digits = 2, nsmall = 2),
            ",",
            format(uconf, digits = 2, nsmall = 2),
            "] "
          )
        xtext <- as.Date(visualizeVariables$date.time[sliderx[1]])

        pmin <-
          min(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        pmax <-
          max(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        drange <- pmax - pmin
        graphics::text(
          xtext,
          pmax - 0.01 * drange,
          paste("mean:", mean.out, visualizeVariables$unit, sep = " "),
          pos = 4,
          cex = textsize
        )
        graphics::text(
          xtext,
          pmax - 0.08 * drange,
          paste0(
            "linear trend:",
            trend.out,
            visualizeVariables$unit,
            "/yr"
          ),
          pos = 4,
          cex = textsize
        )
      }
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    }

    if (checkGroup_type == 4) {
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      graphics::plot(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "s",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = text3_1d,
        ylab = text4_1d,
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
      graphics::points(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "s",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = col,
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
      graphics::box(col = "gray20", lwd = 1)
      if (as.logical(addTrend)) {
        # calculate the linear trend
        x <-
          c(seq_along(visualizeVariables$date.time[sliderx[1]:sliderx[2]]))
        model <-
          stats::lm(visualizeVariables$data[sliderx[1]:sliderx[2]] ~ x, na.action = stats::na.exclude)
        graphics::lines(
          visualizeVariables$date.time[sliderx[1]:sliderx[2]],
          stats::predict(model),
          col = "gray20",
          lwd = linesize
        )
        conf <- stats::confint(model, "x", level = 0.95)
        trend <- model$coeff["x"] * 12.
        lconf <- conf[1] * 12.
        uconf <- conf[2] * 12

        mean.out <-
          format(
            mean(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE),
            digits = 3,
            nsmall = 1
          )
        trend.out <-
          paste0(
            "[",
            format(lconf, digits = 2, nsmall = 2),
            ",",
            format(trend, digits = 2, nsmall = 2),
            ",",
            format(uconf, digits = 2, nsmall = 2),
            "] "
          )
        xtext <- as.Date(visualizeVariables$date.time[sliderx[1]])

        pmin <-
          min(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        pmax <-
          max(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        drange <- pmax - pmin
        graphics::text(
          xtext,
          pmax - 0.01 * drange,
          paste("mean:", mean.out, visualizeVariables$unit, sep = " "),
          pos = 4,
          cex = textsize
        )
        graphics::text(
          xtext,
          pmax - 0.08 * drange,
          paste0(
            "linear trend:",
            trend.out,
            visualizeVariables$unit,
            "/yr"
          ),
          pos = 4,
          cex = textsize
        )
      }

      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    }

    if (checkGroup_type == 5) {
      iwidth  <- imagewidth
      iheight <- imageheight
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)
      graphics::plot(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "h",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = "white",
        main = text1_1d,
        xlab = text3_1d,
        ylab = text4_1d,
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
      graphics::points(
        visualizeVariables$date.time,
        visualizeVariables$data,
        type = "h",
        xlim = visualizeVariables$date.time[sliderx],
        ylim = slidery,
        col = col,
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
      graphics::box(col = "gray20", lwd = 1)
      if (as.logical(addTrend)) {
        # calculate the linear trend
        x <-
          c(seq_along(visualizeVariables$date.time[sliderx[1]:sliderx[2]]))
        model <-
          stats::lm(visualizeVariables$data[sliderx[1]:sliderx[2]] ~ x, na.action = stats::na.exclude)
        graphics::lines(
          visualizeVariables$date.time[sliderx[1]:sliderx[2]],
          stats::predict(model),
          col = "gray20",
          lwd = linesize
        )
        conf <- stats::confint(model, "x", level = 0.95)
        trend <- model$coeff["x"] * 12.
        lconf <- conf[1] * 12.
        uconf <- conf[2] * 12

        mean.out <-
          format(
            mean(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE),
            digits = 3,
            nsmall = 1
          )
        trend.out <-
          paste0(
            "[",
            format(lconf, digits = 2, nsmall = 2),
            ",",
            format(trend, digits = 2, nsmall = 2),
            ",",
            format(uconf, digits = 2, nsmall = 2),
            "] "
          )
        xtext <- as.Date(visualizeVariables$date.time[sliderx[1]])

        pmin <-
          min(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        pmax <-
          max(visualizeVariables$data[sliderx[1]:sliderx[2]], na.rm = TRUE)
        drange <- pmax - pmin
        graphics::text(
          xtext,
          pmax - 0.01 * drange,
          paste("mean:", mean.out, visualizeVariables$unit, sep = " "),
          pos = 4,
          cex = textsize
        )
        graphics::text(
          xtext,
          pmax - 0.08 * drange,
          paste0(
            "linear trend:",
            trend.out,
            visualizeVariables$unit,
            "/yr"
          ),
          pos = 4,
          cex = textsize
        )
      }
      graphics::mtext(text2_1d)
      on.exit(grDevices::dev.off())
    }
  } # end if analyze

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
