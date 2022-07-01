# Changelog for cmsafvis package

## 1.1.12

- Adapt y-limit for fieldmean
- Add option for x and y label of 1d plots
- Unused packages removed from Imports: rworldxtra, rgdal, maptools

## 1.1.11

- Minor adaptations to fieldmean plots
- New color options for anomaly plots
- Option to plot relative anomaly plots

## 1.1.10

- new config parameter for quicklook: 'unit' - define variable unit
  - e.g., Percent or '%'
- new color scales 'albedo' and 'albedo2' added 
- minor adaptations to quicklook
- GeoTiff output changed from RGB to raster values
- Circular Stripe plots (contribution by Emanuele Bevacqua; see emanuele.bevacqua.eu)

## 1.1.9

- plotKML package is back on CRAN and export of KML is possible again
- fieldmean plot includes now all available years and not just years included
  in the climatology period
- new color scale 'larry' added
- get_basename_vis added to helper functions to avoid clash with   
  cmsafops::get_basename

## 1.1.8

- Duplication of get_basename helper function in cmsafvis and cmsafops led to
  a warning in cmsaf

## 1.1.7

- Adaptations to work with files via URL
- KML export is temporarily removed due to missing plotKML package 

## 1.1.6
- Bug fixes in quicklook: wrong logo position, wrong color bar in polar plots

## 1.1.5
- Several adaptations in quicklook operator
- New config parameters for quicklook

## 1.1.0

- Add new funtionalities to monitor_climate operator (warming stripes, trend plot, time series plot)
- Update required package dependencies
- Add new operators to compare data
- Add option to plot DWD logo
- Source label changed from creator_name to publisher_name

## 1.0.0

- Split up old cmsaf package into cmsaf (containing only the toolbox), cmsafops (containing operators for NetCDF data) 
  and cmsafvis (containing plotting functionality)
- New operator set to monitor and analyze the climate (monitor_climate)
- Quicklook operator to create a visual preview of data obtained from the CM SAF (quicklook)