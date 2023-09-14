#'cmsaf: A user-friendly Toolbox for preparing, manipulating, analyzing and visualizing 
#'satellite-based CM SAF NetCDF formatted data.
#'
#'The 'cmsafops' functions are manipulating NetCDF input files and write the result
#'in a separate output file. The functions were designed and tested for CM SAF
#'NetCDF data, but most of the functions can be applied to other NetCDF data,
#'which use the CF convention and time, latitude, longitude dimensions. As interface 
#to NetCDF data the '\link[ncdf4:ncdf4-package]{ncdf4 package} is used.
#'This package uses functionalities of the \link[cmsafops:cmsafops]{cmsafops} and
#'\link[cmsafvis:cmsafvis]{cmsafvis} packages.
"_PACKAGE"
#'
#'@section Get started: Run \code{run_toolbox()} to open the interactive toolbox.
#'
#'
#'@author Maintainer: Steffen Kothe \email{Steffen.Kothe@dwd.de}
#'
#'  Contact: CM SAF Team \email{contact.cmsaf@dwd.de}
#'
#'@references \url{http://www.cmsaf.eu/R_toolbox}
#'
#'  Kothe, S.; Hollmann, R.; Pfeifroth, U.; Träger-Chatterjee, C.; Trentmann, J.
#'  The CM SAF R Toolbox—A Tool for the Easy Usage of Satellite-Based Climate
#'  Data in NetCDF Format. ISPRS Int. J. Geo-Inf. 2019, 8, 109.
#'  \doi{10.3390/ijgi8030109}
#'
#'@keywords datagen manip package spatial ts univar
NULL
