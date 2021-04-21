#'Calculates the rmse, mae, bias, correlation over time
#'of a NetCDF file and a dataframe (station data).
#'Designed for the CM SAF R Toolbox.
#'
#'@param var Name of NetCDF variable of NetCDF file (character).
#'@param infile Filename of input NetCDF file. This may include 
#'  the directory (character).
#'@param data_station Dataframe of RData or csv file (station data); 
#'  Designed for the CM SAF R Toolbox.
#'@param outfile Filename of output csv file. This may include the directory
#'  (character).
#'@param overwrite logical; should existing output file be overwritten?; Default: FALSE
#'
#'@return A csv file including the rmse, mae, bias and correlation over time 
#'  is written. 
#'@export
#'
#'@family metrics

cmsaf.stats.station.data <- function(var, infile, data_station, outfile, overwrite = FALSE) {
  gc()
  calc_time_start <- Sys.time()
  
  if(overwrite){
    if(file.exists(outfile)){
      unlink(outfile)
    }
  }
  
  check_variable(var)
  check_infile(infile)
  
  nc_in <- nc_open(infile)
  file_data <- cmsafops::read_file(infile, var)
  lon <- file_data$dimension_data$x
  lat <- file_data$dimension_data$y
  
  id <- ncdf4::nc_open(infile)
  data_nc <- ncdf4::ncvar_get(id, var, collapse_degen = FALSE)
  
  date <- ncdf4::ncvar_get(id, "time")
  t_unit <- ncdf4::ncatt_get(id, "time", "units")$value
  date.time <- as.character(cmsafops::get_time(t_unit, date))
  data2 <- data_station
  
  list_data_station <- list()
  
  station_all <- NULL
  for (i in seq_along(data2$lon)) {
    dummy <- paste0("[", round(data2$lon[i], digits = 1), ";", round(data2$lat[i], digits = 1), "]")
    station_all <- append(station_all, dummy)
  }
  station_all_seq <- unique(station_all)
  
  for(index_time in 1:length(date.time)){
    a <- data2
    min_lon <- min(lon, na.rm = TRUE)
    max_lon <- max(lon, na.rm = TRUE)
    min_lat <- min(lat, na.rm = T)
    max_lat <- max(lat, na.rm = T)
    
    # lon
    slider1 <- c(max(round(as.numeric(min_lon)), -180), min(round(as.numeric(max_lon)), 180))
    
    # lat
    slider2 <- c(max(round(as.numeric(min_lat)), -90), min(round(as.numeric(max_lat)), 90))
    
    lo_dummy <- c("lon", "longitude", "laenge", "x", "lon_rep")
    la_dummy <- c("lat", "latitude", "breite", "y", "lat_rep")
    ti_dummy <- c("time", "date", "zeit", "t", "get_time.file_data.time_info.units..file_data.dimension_data.t.")
    da_dummy <- c("data", "daten", "z", "element", "result")
    
    dn <- attr(a, "element_name")
    if (!is.null(dn)) {
      da_dummy <- append(da_dummy, dn)
    } else {
      dn <- attr(a, "data_name")
      if (!is.null(dn)) {
        da_dummy <- append(da_dummy, dn)
      }
    }
    
    instat_names <- names(a)
    
    lo_n <- 0
    la_n <- 0
    ti_n <- 0
    da_n <- 0
    
    for (i in seq_along(instat_names)) {
      if (toupper(instat_names[i]) %in% toupper(lo_dummy)) (lo_n <- i)
      if (toupper(instat_names[i]) %in% toupper(la_dummy)) (la_n <- i)
      if (toupper(instat_names[i]) %in% toupper(ti_dummy)) (ti_n <- i)
      if (toupper(instat_names[i]) %in% toupper(da_dummy)) (da_n <- i)
    }
    
    if (lo_n > 0 & la_n > 0 & ti_n > 0 & da_n > 0) {
      # check monthly or daily
      # station
      time_station <- a[, ti_n]
      if (length(time_station) > 500) (time_station <- time_station[1:500])
      mon_station  <- format(as.Date(time_station), "%m")
      year_station <- format(as.Date(time_station), "%Y")
      day_station  <- format(as.Date(time_station), "%d")
      
      dummy <- which(mon_station == mon_station[1] & year_station == year_station[1])
      mmdm <- "d"
      
      if (length(unique(day_station[dummy])) == 1) {
        mmdm <- "m"
      }
      
      # satellite
      time_sat <- date.time
      if (length(time_sat) > 40) (time_sat <- time_sat[1:40])
      mon_sat  <- format(as.Date(time_sat), "%m")
      year_sat <- format(as.Date(time_sat), "%Y")
      day_sat  <- format(as.Date(time_sat), "%d")
      dummy <- which(mon_sat == mon_sat[1] & year_sat == year_sat[1])
      mmdm_sat <- "d"
      if (length(unique(day_sat[dummy])) == 1) {
        mmdm_sat <- "m"
      }
      
      # extract data for chosen time step
      if (mmdm == "m" & mmdm_sat == "m") {
        match_time   <- which(format(as.Date(a[, ti_n]), "%Y-%m") == format(as.Date(date.time[index_time]), "%Y-%m"), arr.ind = TRUE)
      } else {
        match_time   <- which(a[, ti_n] == date.time[index_time], arr.ind = TRUE)
      }
      
      lon_station  <- a[, lo_n][match_time]
      lat_station  <- a[, la_n][match_time]
      data_station <- a[, da_n][match_time]
      
      # delete NAs
      dummy <- !is.na(data_station)
      data_station <- data_station[dummy]
      data_station <- data_station
      lon_station  <- lon_station[dummy]
      lat_station  <- lat_station[dummy]
      # Extract corresponding data points
      
      data_sat <- c(seq_along(data_station))
      
      result_x <- c()
      result_y <- c()
      
      result_x <- rep(lon, length(lat))
      
      for(j in seq_along(lat)){
        result_y <- append(result_y, rep(lat[j], length(lon)))
      }
      
      coor_sat <- cbind(x=result_x, y=result_y)
      A <- sp::SpatialPoints(coor_sat)
      
      for (istation in seq_along(data_station)) {
        B <- sp::SpatialPoints(cbind(x=c(lon_station[istation]), y=c(lat_station[istation])))
        tree <- SearchTrees::createTree(sp::coordinates(A))
        inds <- SearchTrees::knnLookup(tree, newdat=sp::coordinates(B), k=1)
        
        lon_coor <- coor_sat[inds,1]
        lat_coor <- coor_sat[inds,2]
        
        data_sat[istation] <- data_nc[which(lon == lon_coor),which(lat == lat_coor), which(date.time == date.time[index_time])]
      }
      cd <- data.frame(data_sat, data_station, lon_station, lat_station)
    }
    
    labs <- NULL
    for (i in seq_along(cd$lon_station)) {
      dummy <- paste0("[", round(cd$lon_station[i], digits = 1), ";", round(cd$lat_station[i], digits = 1), "]")
      labs <- append(labs, dummy)
    }
    row.names(cd) <- labs
    
    for(i in 1:length(station_all_seq)){
      value <- cd[station_all_seq[i],]
      
      if(index_time==1){
        list_data_station <- append(list_data_station, list(cd[station_all_seq[i],]))
        row.names(list_data_station[[i]]) <- NULL
      } else {
        list_data_station[[i]] <- rbind(list_data_station[[i]],cd[station_all_seq[i],])
        row.names(list_data_station[[i]]) <- NULL
      }
    }
  }

  rmse <- c()   # RMSE (root mean squared error)
  mae <- c()    # MAE (mean absolute error)
  bias <- c()   # BIAS
  correlation <- c()    # Correlation over time
  
  for(i in 1:length(station_all_seq)) {
    diff_nc_station_data <- list_data_station[[i]]$data_sat - list_data_station[[i]]$data_station
    rmse <- append(rmse, sqrt(mean((c(diff_nc_station_data))^2, na.rm = TRUE)))
    mae <- append(mae, mean(abs(c(diff_nc_station_data)), na.rm = TRUE))
    bias <- append(bias, sqrt(  mean((c(diff_nc_station_data))^2, na.rm = TRUE) - stats::var(c(diff_nc_station_data), na.rm = TRUE)))
    correlation <- append(correlation, stats::cor(list_data_station[[i]]$data_sat, list_data_station[[i]]$data_station, use = "na.or.complete"))
  }
  
  location <- station_all_seq
  result_stats <- cbind(location, rmse, mae, bias, correlation)
  result_stats_df <- as.data.frame(result_stats)
  utils::write.csv(x = result_stats_df, file = outfile, row.names = FALSE)
}