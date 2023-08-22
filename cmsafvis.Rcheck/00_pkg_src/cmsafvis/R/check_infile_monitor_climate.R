# This function checks whether monthly or daily data is available
check_infile_monitor_climate <- function(infile,
                                         accumulate,
                                         nc = NULL)
{
  if (!is.null(nc)) opennc <- nc
  else opennc <- ncdf4::nc_open(infile)
  time <- ncdf4::ncvar_get(opennc, "time")
  if (is.null(nc)) ncdf4::nc_close(opennc)
  
  # check monthly data
  diff_time <- time[2] - time[1]
  if(!is.na(diff_time)){
    if(diff_time > 1) # monthly file
    {
      if(accumulate){
        stop("Please select a daily file for accumulated data!")
      }
    }
  }
}