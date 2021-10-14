# Changelog for cmsafvis package

## 1.1.7

- Adaptations to work with files via URL

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