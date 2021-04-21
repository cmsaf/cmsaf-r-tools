#'Function to combine ACSAF NetCDF files and simultaneously cut a region.
#'
#'This function selects a region (and optionally a level) from a bunch of AC SAF
#'NetCDF files that match the same pattern of the filename, and writes the
#'output to a new file.
#'
#'@param path The directory of input NetCDF files without / at the end
#'  (character).
#'@param pattern A part of the filename, which is the same for all desired input
#'  files (character). The pattern has to be a character string containing a
#'  regular expression.
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param lon1 Longitude of lower left corner (numeric).
#'@param lon2 Longitude of upper right left corner (numeric).
#'@param lat1 Latitude of lower left corner (numeric).
#'@param lat2 Latitude of upper right corner (numeric).  Longitude of upper
#'  right corner (numeric).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'
#'@return A NetCDF file including the merged time series of the selected region
#'  is written.
#'@export
#'
#'@family data manipulation functions

acsaf_box_mergetime <-
function(path, pattern, outfile, lon1=-180, lon2=180, lat1=-90, lat2=90, nc34=3){
  start.time <- Sys.time()

  # define standard names of variables and dimensions
  t_name <- "time"
  t_standard_name = "time"
  t_calendar = "standard"
  ref_date = "1983-01-01"
  t_units = paste("days since ",ref_date," 00:00:00",sep="")
  
  nb2_units = "1"
  
  lat_name = "latitude"
  lat_standard_name = "latitude"
  lat_long_name = "latitude"
  lat_units = "degrees_north"
  lat_axis = "Y"
  
  lon_name = "longitude"
  lon_standard_name = "longitude"
  lon_long_name = "longitude"
  lon_units = "degrees_east"
  lon_axis = "X"
  
  v_standard_name = "undefined"
  v_long_name = "undefined"
  v_units = "undefined"
  v__FillValue = "undefined"
  v_missing_value = "undefined"
  
  info = "Created with the CM SAF R Toolbox." 
  var_prec="float"
  
  att_list <- c("standard_name","long_name","units","_FillValue","missing_value","calendar")
  v_att_list <- c("v_standard_name","v_long_name","v_units","v__FillValue","v_missing_value","v_calendar")

  fileslist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames

  result.fileslist <- NULL
  for(i in 1:length(fileslist)){
    regex <- regmatches(fileslist[i], regexpr(pattern, fileslist[i]))
    if(!is.null(regex))
      result.fileslist <- append(result.fileslist, regex)
    regex <- NULL
  }
  
  filelist <- sort(result.fileslist)
  fdim <- length(filelist)
  file=filelist[1]
  file <- paste(path,"/",file,sep="")
  
  id <- nc_open(file)

  # get information about dimensions and attributes
  dimnames   <- names(id$dim)
  global_att <- ncatt_get(id,0)

  # check standard_names of dimensions
  for (i in 1:length(dimnames)){
    sn <- ncatt_get(id,dimnames[i],"standard_name")
    ln <- ncatt_get(id,dimnames[i],"long_name")
    if (!is.null(sn$hasatt)){
      if (sn$hasatt){
        sn <- sn$value
        if (sn %in% c("longitude","Longitude","Lon","lon"))(lon_name <- dimnames[i])
        if (sn %in% c("latitude","Latitude","Lat","lat"))(lat_name <- dimnames[i])
        if (sn=="time"|sn=="Time")(t_name <- dimnames[i])
      } else {
          if (ln$hasatt){
            ln <- ln$value
            if (ln %in% c("longitude","Longitude","Lon","lon"))(lon_name <- dimnames[i])
            if (ln %in% c("latitude","Latitude","Lat","lat"))(lat_name <- dimnames[i])
            if (ln=="time"|ln=="Time")(t_name <- dimnames[i])
          }
       }
    }
  }
  for (i in 1:length(dimnames)){
    if (t_name %in% dimnames){
      attnames <- names(id$dim[[i]])
      if ("units" %in% attnames){
	      t_units <- ncatt_get(id,t_name,"units")$value}
      if ("calendar" %in% attnames){
	      t_calendar <- ncatt_get(id,t_name,"calendar")$value}
    }
  }
  
  # adapt varname to AC SAF standard
  variables.names <- names(id$var)
  pattern <- "^PRODUCT\\/\\w+" # regular exp. to extract variables
  var <- as.vector(regmatches(variables.names, regexpr(pattern, variables.names)))
  
  var_tmp <- NULL
  for(i in 1:length(var)){
    var_tmp  <- append(var_tmp, substring(var[i], 9))
  }
  varn <- var_tmp

  # get information about variables
  varnames <- names(id$var)
  var_default <- subset(varnames, !(varnames %in% c("lat","lon","time_bnds","nb2","time")))
  
  var_tmp <- NULL
  for(i in 1:length(var)){
    if (toupper(var[i]) %in% toupper(var_default)){
      var_tmp <- append(var_tmp, var_default[which(toupper(var[i])==toupper(var_default))])
    }
  }
  var <- var_tmp
  
  # set variable precision
  varind   <- which(varnames==var[1])

  varprec  <- NULL
  varprec  <- id$var[[varind]]$prec
 
  if (!is.null(varprec)){
    if (varprec %in% c("short", "float", "double", "integer", "char", "byte")){
      (var_prec <- varprec)
    }
  }

target.list <- list()

 if (var[1] %in% varnames){
  for (i in 1:6){
    att_dum <- ncatt_get(id,var[1],att_list[i])
    if (att_dum$hasatt){
      assign(v_att_list[i],att_dum$value)}
  }

  # get data of first file and cut desired region
  lon <- ncvar_get(id,lon_name)
  lat <- ncvar_get(id,lat_name)
  
  # AC SAF files do not contain time
  # time information will be extracted from file name
  dummy <- strsplit(basename(file),"_")[[1]][4]
  dummy <- paste(dummy,"01",sep="")
  dummy <- as.Date(dummy,format="%Y%m%d")
  time1 <- as.integer(as.Date(dummy,format="%Y%m%d")-as.Date(ref_date))
  time_len <- length(time1)

  lon_limit <- which(lon>=lon1&lon<=lon2)  
  lat_limit <- which(lat>=lat1&lat<=lat2) 

  lon <- lon[lon_limit]
  lat <- lat[lat_limit]

  # check for empty lon_limit or lat_limit
  if (length(lon_limit)==0|length(lat_limit)==0){
    nc_close(id)
    stop("Selected region is outside target area!")
  }

	startx <- min(lon_limit)
	starty <- min(lat_limit)
	countx <- length(lon_limit)
	county <- length(lat_limit)
  
  for(i in 1:length(var)){
    target <- ncvar_get(id,var[i],start=c(startx,starty),count=c(countx,county))
    target.list[[i]] <- target
  }

  } else {
     nc_close(id)
  }

  if (v__FillValue == "undefined"){ 
    v__FillValue = v_missing_value}
  if (v_missing_value == "undefined"){ 
    v_missing_value = v__FillValue}

  nc_close(id)

  # store data in target field
  if ("time_bnds" %in% varnames){
    time_bnds <- array(NA,dim=c(2,length(time1)))
    #time_bnds[,1:length(time1)] <- tbnds1
  } 

  # get time reference
  dt_ref <- get_time(t_units,0)
  unit_ref <- unlist(strsplit(t_units,split=" "))[1]

  # check reference time unit
  if (unit_ref=="minutes"|unit_ref=="Minutes"|unit_ref=="Mins"|unit_ref=="Min"|unit_ref=="min")(unit_ref <- "mins")
  if (unit_ref=="seconds"|unit_ref=="Seconds"|unit_ref=="Secs"|unit_ref=="Sec"|unit_ref=="sec")(unit_ref <- "secs")
  if (unit_ref=="Hours"|unit_ref=="Hour"|unit_ref=="hour")(unit_ref <- "hours")
  if (unit_ref=="Days"|unit_ref=="Day"|unit_ref=="day")(unit_ref <- "days")
  if (unit_ref=="Months"|unit_ref=="Month"|unit_ref=="month")(unit_ref <- "months")
  if (unit_ref!="mins"&unit_ref!="secs"&unit_ref!="hours"&unit_ref!="days"&unit_ref!="weeks"&unit_ref!="months")(unit_ref <- "auto")

  # create netcdf
  # NetCDF format 3 or 4
  if (nc34==4){
    nc_format <- as.logical(1)
    compression = 4
  } else {
    nc_format <- as.logical(0)
    compression = NA
  }

  cmsaf_info <- paste0("cmsaf::acsaf_box_mergetime for variables ")
  
  for(i in 1:length(varn)){
    target.list[i][is.na(target.list[i])] <- v_missing_value
  }

  nb2 <- c(0,1)
    
  # prepare global attributes
  global_att_default <- c("institution","title","summary","id","creator_name",
                          "creator_email","creator_url","creator_type","publisher_name",
                          "publisher_email","publisher_url","publisher_type",
                          "references","keywords_vocabulary","keywords","project",
                          "standard_name_vocabulary","geospatial_lat_units",
                          "geospatial_lon_units","geospatial_lat_resolution",
                          "geospatial_lon_resolution","platform_vocabulary","platform",
                          "instrument_vocabulary","instrument","date_created","product_version",
                          "producer","version","dataset_version","source")
  
  global_att_list <- names(global_att)
  
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(global_att_default)]
  global_att <- global_att[global_att_list]

  x <- ncdim_def(name="lon",units=lon_units,vals=lon)
  y <- ncdim_def(name="lat",units=lat_units,vals=lat)
  t <- ncdim_def(name="time",units=t_units,vals=time1,unlim=TRUE)
  
  if ("time_bnds" %in% varnames){
    tb <- ncdim_def(name="nb2",units="1",vals=nb2)
  }
  
  var.list <- list()
  for(i in 1:length(varn)){
    var.list[[i]] <- ncvar_def(name=varn[i],units=v_units,dim=list(x,y,t),missval=v_missing_value,
              prec=var_prec,compression=compression)
  }

  if ("time_bnds" %in% varnames){
    var2 <- ncvar_def(name="time_bnds",units="1",dim=list(tb,t),prec="double")
    
    vars <- list(var2)
    for(i in 1:length(varn)){
      vars[[i+1]] <- var.list[[i]]
    }
    
    ncnew <- nc_create(outfile,vars,force_v4=nc_format)
    ncvar_put(ncnew,var2,time_bnds)
    
    for(i in 1:length(varn)){
      ncvar_put(ncnew,var.list[i],target.list[i])
    }
    

    for(i in 1:length(varn)){
      ncatt_put(ncnew,varn[i],"standard_name",v_standard_name,prec="text")
      ncatt_put(ncnew,varn[i],"long_name",v_long_name,prec="text")
      ncatt_put(ncnew,varn[i],"cmsaf_info",cmsaf_info,prec="text")
    }
    
    ncatt_put(ncnew,"time","standard_name",t_standard_name,prec="text")
    ncatt_put(ncnew,"time","calendar",t_calendar,prec="text")
    ncatt_put(ncnew,"time","bounds","time_bnds",prec="text")

    ncatt_put(ncnew,"lon","standard_name",lon_standard_name,prec="text")
    ncatt_put(ncnew,"lon","long_name",lon_long_name,prec="text")
    ncatt_put(ncnew,"lon","axis",lon_axis,prec="text")

    ncatt_put(ncnew,"lat","standard_name",lat_standard_name,prec="text")
    ncatt_put(ncnew,"lat","long_name",lat_long_name,prec="text")
    ncatt_put(ncnew,"lat","axis",lat_axis,prec="text")

    ncatt_put(ncnew,0,"Info",info,prec="text")
    
    if (length(global_att_list)>0){
      for (iglob in 1:length(global_att_list)){
        ncatt_put(ncnew,0,global_att_list[iglob],global_att[iglob][[1]],prec="text")
      }
    }
    
  } else {
    vars <- list()
    for(i in 1:length(varn)){
      vars[[i]] <- var.list[[i]]
    }

    ncnew <- nc_create(outfile,vars,force_v4=nc_format)

    for(i in 1:length(varn)){
      ncvar_put(ncnew,var.list[[i]],target.list[[i]])
    }

    for(i in 1:length(varn)){
      ncatt_put(ncnew,varn[i],"standard_name",v_standard_name,prec="text")
      ncatt_put(ncnew,varn[i],"long_name",v_long_name,prec="text")
      ncatt_put(ncnew,varn[i],"cmsaf_info",cmsaf_info,prec="text")
    }

    ncatt_put(ncnew,"time","standard_name",t_standard_name,prec="text")
    ncatt_put(ncnew,"time","calendar",t_calendar,prec="text")

    ncatt_put(ncnew,"lon","standard_name",lon_standard_name,prec="text")
    ncatt_put(ncnew,"lon","long_name",lon_long_name,prec="text")
    ncatt_put(ncnew,"lon","axis",lon_axis,prec="text")

    ncatt_put(ncnew,"lat","standard_name",lat_standard_name,prec="text")
    ncatt_put(ncnew,"lat","long_name",lat_long_name,prec="text")
    ncatt_put(ncnew,"lat","axis",lat_axis,prec="text")

    ncatt_put(ncnew,0,"Info",info,prec="text")
    
    if (length(global_att_list)>0){
      for (iglob in 1:length(global_att_list)){
        ncatt_put(ncnew,0,global_att_list[iglob],global_att[iglob][[1]],prec="text")
      }
    }
  }
  
  # check timestep sorting
  time_sorting <- time1
  file_num <- rep(1,length(time_sorting))  
      
  if (fdim>=2){
    for (i in 2:fdim){
      file=filelist[i]
      file <- file.path(path,file)
      id <- nc_open(file)
        dummy <- strsplit(basename(file),"_")[[1]][4]
        dummy <- paste(dummy,"01",sep="")
        dum_time <- as.integer(as.Date(dummy,format="%Y%m%d")-as.Date(ref_date))
      nc_close(id)
      time_sorting <- append(time_sorting,dum_time)
      file_num <- append(file_num,rep(i,length(dum_time)))
    }
        
    file_num <- file_num[order(time_sorting)]
    filelist <- filelist[unique(file_num)]
  }
    
  # get data and cut desired region  
  time_len <- length(time1)
  if (fdim>=2){
    for (i in 2:fdim){
      #cat("\r","loading file ",i," of ",fdim,sep="")
      file=filelist[i]
      file <- file.path(path,file)
      id <- nc_open(file)
      
      dum_dat.list <- list()
      for(i in 1:length(varn)){
        dum_dat.list[[i]] <- ncvar_get(id,var[i],start=c(startx,starty),count=c(countx,county))
      }
      
      dummy <- strsplit(basename(file),"_")[[1]][4]
      dummy <- paste(dummy,"01",sep="")
      dum_time <- as.integer(as.Date(dummy,format="%Y%m%d")-as.Date(ref_date))
      time_len <- time_len+length(dum_time)
  
      if ("time_bnds" %in% varnames){
	      dum_tb <- ncvar_get(id,"time_bnds")
      }

      nc_close(id)

      for(i in 1:length(varn)){
        dum_dat.list[i][is.na(dum_dat.list[i])] <- v_missing_value
      }
      
      startt2 <- time_len-length(dum_time)+1
      countt2 <- length(dum_time)
      
      if ("time_bnds" %in% varnames){
        for(i in 1:length(varn)){
          ncvar_put(ncnew,var.list[[i]],dum_dat.list[[i]],start=c(1,1,startt2),count=c(-1,-1,countt2))
        }
        
        ncvar_put(ncnew,var2,dum_tb,start=c(1,startt2),count=c(-1,countt2))
        ncvar_put(ncnew,t,dum_time, start=startt2, count=countt2 )
        nc_sync(ncnew)

      } else {
        for(i in 1:length(varn)){
          ncvar_put(ncnew,var.list[[i]],dum_dat.list[[i]],start=c(1,1,startt2),count=c(-1,-1,countt2))
        }
        
        ncvar_put(ncnew,t,dum_time, start=startt2, count=countt2 )
        nc_sync(ncnew)
      }
    }
  } else if (fdim==1) {
    #cat("Just one file matches this pattern.")
  }
  nc_close(ncnew)

end.time <- Sys.time()
}