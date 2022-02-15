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
  # use wfldmean for warming stripes plot
  cmsafops::wfldmean(variable, infile, outfile = file.path(tempdir(),"tmp_warming_stripes_plot.nc"), overwrite = TRUE, nc = nc)
  
  temp_file <- file.path(tempdir(),"tmp_warming_stripes_plot.nc")
  
  file_data <- cmsafops::read_file(temp_file, variable)
  nc_in <- ncdf4::nc_open(temp_file)
  
  # read data from infile
  dum_dat <- ncdf4::ncvar_get(
    nc_in,
    file_data$variable$name,
    collapse_degen = FALSE
  )
  
  dim_names   <- names(nc_in$dim)
  dimensions <- cmsafops::get_dimensions(nc_in, dim_names)
  time_info <- cmsafops::get_time_info(nc_in, dim_names, dimensions$names$t)
  
  dimension.data.t <- nc_in$dim[[dimensions$names$t]]$vals
  
  dum_dat <- as.vector(dum_dat)
  date_info_1 <- as.Date(cmsafops::get_time(time_info$units, dimension.data.t))
  date_info <- as.numeric(date_info_1)
  
  dataT <- data.frame(date_info, dum_dat)
  ncdf4::nc_close(nc_in)
  
  nBins = 10
  
  minT = min(dataT$dum_dat)
  maxT = max(dataT$dum_dat)
  minY = min(dataT$date_info)
  maxY = max(dataT$date_info)
  
  xlabel <- rep(NA, length(date_info_1))
  n <- round(seq(1, length(xlabel), length.out = 4))
  xlabel[n] <- format(date_info_1[n], "%Y")
  
  title <- gsub("XXXX", xlabel[1], title)
 
  resfactor <- 1

  if (circ_plot) {
    
    ### Define helper function: Code from R-package plotrix 3.8-2
    ### Plotrix: a package in the red light district of R
    ### Author: J. Lemon
    
    getYmult <- function() {
      if(grDevices::dev.cur() == 1) {
        warning("No graphics device open.")
        ymult <- 1
      }
      else {
        # get the plot aspect ratio
        xyasp <- graphics::par("pin")
        # get the plot coordinate ratio
        xycr <- diff(graphics::par("usr"))[c(1,3)]
        ymult <- xyasp[1]/xyasp[2]*xycr[2]/xycr[1]
      }
      return(ymult)
    }
    
    draw.circle <- function(x, y, radius, nv=100, border=NULL, col=NA,
                            lty=1, density=NULL, angle=45, lwd = 1) {
      
      xylim <- graphics::par("usr")
      plotdim <- graphics::par("pin")
      ymult <- getYmult()
      angle.inc <- 2*pi/nv
      angles <- seq(0,2*pi-angle.inc, by = angle.inc)
      if(length(col) < length(radius)) 
        col <- rep(col, length.out = length(radius))
      for(circle in 1:length(radius)) {
        xv <- cos(angles)*radius[circle]+x
        yv <- sin(angles)*radius[circle]*ymult+y
        graphics::polygon(xv, yv, border = border, col = col[circle], lty = lty,
                density = density, angle = angle, lwd = lwd)
      }
      invisible(list(x = xv, y = yv))
    }
    
    ### End of code from plotrix R-package
    
    # initialize plot
    grDevices::png(filename=paste0(out_dir, "/", outfile_name), 
                   res = 72*resfactor, height=800*resfactor, width=800*resfactor)
    
    temp <- dataT
    temp$date_info <- format(date_info_1,"%Y")
    colnames(temp) <- c("date", "level")
    
    ### The code for circular stripe plots was kindly provided by 
    ### Dr Emanuele Bevacqua. For more details please have a look at
    ### emanuele.bevacqua.eu
    
    # Define color palette
    Red <- RColorBrewer::brewer.pal(9, "YlOrRd")
    Red <- grDevices::colorRampPalette(Red)(170)
    
    Blues <- RColorBrewer::brewer.pal(9, "Blues")
    Blues <- grDevices::colorRampPalette(Blues)(170)
    
    colors=c(rev(Blues),Red)
    
    if (color_pal == 2){palette <- 
      grDevices::colorRampPalette(c("#474747", "#7a7a7a", "#a8a8a8", "#cdcdcd",
                                    "#e2e2e2", "#f9f9f9", "#fdf3db", "#fee8b2",
                                    "#fedf8c", "#fed66c", "#fdcf45", "#fac631"))
    colors <- palette(340)
    }
    
    if (color_pal == 3){palette <- 
      grDevices::colorRampPalette(c("#5c3209", "#96560e", "#b27028", "#d1a759",
                                    "#dfc07a", "#f5e5bf", "#fefefe","#b0dfda", 
                                    "#6fc0b8", "#389c94", "#078470", "#045f5a", 
                                    "#0f3c33"))
    colors <- palette(340)
    }
    
    AssignColor=function(data,colors)
    {
      data$colIndex=rep(-9999,length(data$date))
      data$color=rep(-9999,length(data$date))
      
      borders=seq(min(data$level),max(data$level),length=length(colors)+1)
      
      for(i in 1:(length(borders)-1))
      {
        vect=which(data$level>=borders[i] & data$level<=borders[i+1])
        data$colIndex[vect]=i
        data$color[vect]=colors[i]
      } 
      return(data)
    }
    
    temp=AssignColor(temp,colors)
    
    ##### Plot ##### 
    # Defining some parameters for plot
    delta=5
    xxx=seq(1,length(temp$date))*0
    temp$radius=((temp$level+abs(min(temp$level))))*0.08
    
    for(i in 2:length(xxx)) {
      xxx[i]=xxx[i-1]+temp$radius[i-1]+temp$radius[i]+delta
    }
    
    # Manual selection of image's shape: rectangle or square
    FactorShape=1;shape="1x1"#square
    # FactorShape=2;shape="2x1"#rectangle 1x2
    
    graphics::par("mar")
    graphics::par(mfrow = c(1,1))
    graphics::par(mar = c(0, 0, 0, 0), bg = "black")
    YlimVal = 1.1
    XlimVal = FactorShape*YlimVal
    plot(mean(xxx), col = "white",
         xlim = c(-XlimVal,XlimVal),
         ylim = c(-YlimVal,YlimVal)
         ,     bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    x = rep(-9999,length(xxx))
    y = rep(-9999,length(xxx))
    
    RangeAt3oClock = 50
    radiusStart = 0.04
    radius = rep(-9999,length(xxx))
    for(i in length(xxx):1) {
      x[i] = 0
      y[i] = 0
      
      radius[i] = i/length(xxx)+radiusStart
      colorCircle = temp$color[i]
      draw.circle(x[i],y[i],radius[i],nv = 100,
                  border = colorCircle, col = colorCircle,
                  lty = 1,
                  angle = 45, lwd = 1)
    }
    
    # Plotting white line
    alpha = 0.04
    colorCircle = "gray20"
    draw.circle(x[i]-alpha, y[i], radiusStart, nv=10000,
                         border = colorCircle, col = colorCircle,
                         lty = 1,
                         angle = 45, lwd = 0.000000001)
    delta = radiusStart
    xleft = 0-alpha
    ybottom = -delta - 0.001
    xright = 10
    ytop = delta + 0.001
    graphics::rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
                   col = "gray20", border = "NA", lty = graphics::par("lty"), lwd = graphics::par("lwd"))
    
    # Writing years
    cexYear = 1.5

    t = c(1:length(temp$date))
    t = c(min(t), stats::median(t), max(t))
    
    for (i in t) {
      graphics::text(radius[i], 0, paste(temp$date[i]), cex=cexYear, col = temp$color[i])
    }
    
    title(title, col.main = "white", cex.main = 1.8, line = -2.5, font.main = 2)
    
  } else {
    # initialize plot
    grDevices::png(filename = paste0(out_dir, "/", outfile_name), 
                   res = 72*resfactor, height = 640*resfactor, width = 1140*resfactor)
    
    graphics::par(bg = "black")
    graphics::plot(x = date_info_1, y = dum_dat, 
                 ylab = '', xlab='', type = "n", axes = FALSE)
  
    rangeT = maxT - minT
    binWidth = rangeT/nBins
    palette <- rev(RColorBrewer::brewer.pal(nBins,"RdBu"))
  
    if (color_pal == 2){palette <- 
        grDevices::colorRampPalette(c("#474747", "#7a7a7a", "#a8a8a8", "#cdcdcd",
                                      "#e2e2e2", "#f9f9f9", "#fdf3db", "#fee8b2",
                                      "#fedf8c", "#fed66c", "#fdcf45", "#fac631"))
      palette <- palette(nBins)
    }

    if (color_pal == 3){palette <- 
        grDevices::colorRampPalette(c("#5c3209", "#96560e", "#b27028", "#d1a759",
                                      "#dfc07a", "#f5e5bf", "#fefefe","#b0dfda", 
                                      "#6fc0b8", "#389c94", "#078470", "#045f5a", 
                                      "#0f3c33"))
      palette <- palette(nBins)
    }
  
    binCol <- function (Temp){ 
      index.col <- floor((Temp - minT)/binWidth)+1
      if(index.col > 10){index.col <- 10}
      palette[index.col] 
    }
  
    diff_time <- abs(date_info[2] - date_info[1])

    rectPlot <-  function(df.row){
      y <- df.row["date_info"]
      lineCol <- binCol(df.row["dum_dat"])
      graphics::rect(y-(diff_time/2), minT, y+(diff_time/2), maxT, col=lineCol, border=NA, lwd=0)
    }
  
    apply(dataT, 1, rectPlot)
  
    title(title, col.main = "white", cex.main = 2.0, line = 0.2, 
        font.main = 2)
    graphics::mtext(xlabel, side = 1, line = -0.5, at = date_info_1, 
        cex = 1.5, col = "white")
  
    if (pointsTF) graphics::points(dataT,pch=21,bg="white")
    if (lineTF) graphics::abline(stats::line(dataT),col="white")
  }
  
  grDevices::dev.off()
  
  # calc monitor climate parameters
  tmp_climate_dir <- file.path(tempdir(), "tmp_climate_dir")
  # remove if it exists
  if (dir.exists(tmp_climate_dir)) {
    unlink(tmp_climate_dir, recursive = TRUE)
  }
  # create new temp dic
  if (!dir.exists(tmp_climate_dir)) {
    dir.create(tmp_climate_dir)
  }
  
  # Clim mean value
  tmp_clim_mean_value <- file.path(tmp_climate_dir, paste0("tmp_clim_mean_value.nc"))
  cmsafops::fldmean(var = variable, infile = climatology_file, outfile = tmp_clim_mean_value, overwrite = TRUE)
  nc_in <- ncdf4::nc_open(tmp_clim_mean_value)
  dum_dat_mean <- ncdf4::ncvar_get(nc_in, variable, collapse_degen = FALSE)
  ncdf4::nc_close(nc_in)
  
  years_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$years
  months_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$months
  days_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$days
  
  ranking <- data.frame(years_all, months_all, days_all, as.vector(dum_dat))
  names(ranking) <- c("Year", "Month", "Day","Value")
  
  titles <- c("Analyzed years", "Climatology Mean Value", "Maximum", "Minimum")
  
  ordered_index_dataT <- order(dataT['dum_dat'])
  ordered_dataT <- dataT[ordered_index_dataT, ]
  row.names(ordered_dataT) <- NULL
  
  standout_years <- c(paste0(climate_year_start, " - " ,format(end_date, format = "%Y")),
                      paste(climate_year_start, climate_year_end, sep = " - "),
                      toString(ordered_dataT[nrow(ordered_dataT),1]),
                      toString(ordered_dataT[1,1]))
  
  standout_values <- c(toString(mean(dataT$dum_dat)), mean(dum_dat_mean), toString(ordered_dataT[nrow(ordered_dataT),2]), toString(ordered_dataT[1,2]))
  
  final_values <- data.frame(title = titles, years = standout_years, value = standout_values)
  calc.parameters.monitor.climate(final_values, ranking[order(ranking$Value),])
  
  # remove if it exists
  if (dir.exists(tmp_climate_dir)) {
    unlink(tmp_climate_dir, recursive = TRUE)
  }
}
