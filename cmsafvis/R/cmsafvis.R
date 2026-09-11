#'cmsafvis: A 'cmsaf' package extension for visualization of CM SAF NetCDF data.
#'
#'The 'cmsafvis' plotting routines are designed to analyze climate files
#'by generating graphics or videos. The functions (Fieldmean, Fieldmean and anomaly plots) were designed
#'and tested for daily valued CM SAF NetCDF data. The functions (Absolute Map, Anomaly Map, Climatology, 
#'Warming Stripes Plot, Time Series Plot, Trend Plot) were designed
#'and tested for daily or monthly valued CM SAF NetCDF data. As interface to NetCDF 
#'data the \link[ncdf4:ncdf4-package]{ncdf4 package} is used.
"_PACKAGE"
#'
#'@section Absolute:
#'  \code{\link{absolute_map}}
#'
#'@section Anomaly:
#'  \code{\link{anomaly_map}}
#'
#'@section Climatology:
#'  \code{\link{climatology_map}}
#'
#'@section Fieldmean:
#'  \code{\link{fieldmean_plot}}
#'
#'@section Fieldmean and anomaly plots:
#'  \code{\link{fieldmean_and_anomaly_map}}
#'  
#'@section Warming Stripes Plot:
#'  \code{\link{warming_stripes_plot}}
#'  
#'@section Time Series Plot:
#'  \code{\link{time_series_plot}}
#'  
#'@section Trend Plot:
#'  \code{\link{trend_plot}}
#'
#'
#'@author Maintainer: Steffen Kothe \email{Steffen.Kothe@dwd.de}
#'
#'  Contact: CM SAF Team \email{contact.cmsaf@dwd.de}
#'
#'@references \url{http://www.cmsaf.eu/R_toolbox}
#'
#'  Kothe, S.; Hollmann, R.; Pfeifroth, U.; Träger-Chatterjee, C.; Trentmann, J.
#'  The CM SAF R Toolbox—A Tool for the Easy Usage of Satellite-Based Climate Data
#'  in NetCDF Format. ISPRS Int. J. Geo-Inf. 2019, 8, 109.
#'  \doi{10.3390/ijgi8030109}
#'
#'@keywords datagen manip package spatial ts univar
NULL
