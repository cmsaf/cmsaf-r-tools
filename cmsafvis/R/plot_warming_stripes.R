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
  
  resfactor <- 1
  grDevices::png(filename=paste0(out_dir, "/", outfile_name), 
      res = 72*resfactor, height=640*resfactor, width=1140*resfactor)
  
  #plot(x=date_info_1, y= dum_dat, ylab=variable, xlab="time", main = title, type = "n")
  graphics::plot(x=date_info_1, y = dum_dat, yaxt='n', ylab = '', xlab='', main = title, type = "n")
  
  rangeT=maxT-minT
  binWidth=rangeT/nBins
  palette <- rev(RColorBrewer::brewer.pal(nBins,"RdBu"))
  
  #binCol <- function (Temp){ palette[floor((Temp - minT)/binWidth)+1] }
  binCol <- function (Temp){ 
    index.col <- floor((Temp - minT)/binWidth)+1
    if(index.col > 10){index.col <- 10}
    palette[index.col] 
  }
  
  diff_time <- abs(date_info[2] - date_info[1])

  rectPlot <-  function(df.row){
    y<-df.row["date_info"]
    lineCol<-binCol(df.row["dum_dat"])
    graphics::rect(y-(diff_time/2),minT,y+(diff_time/2),maxT,col=lineCol,border=NA,lwd=0)
    #rect(y-0.5,minT,y+0.5,maxT,col=lineCol,border=NA,lwd=0)
  }
  
  apply(dataT,1,rectPlot)
  if (pointsTF) graphics::points(dataT,pch=21,bg="white")
  if (lineTF) graphics::abline(stats::line(dataT),col="white")
  
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
