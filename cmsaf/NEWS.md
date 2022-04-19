# Release notes for cmsaf

## 3.4.2
- Bug fix: wrong assignment of time step
- lon / lat default for visualization changed from integer to double

## 3.4.1

- Minor adaptation to stripe plots
- New color options for anomaly plots
- Visualizer shows time bounds if included
- Visualizer lon / lat sliders replaced by shinyWidgets::numericRangeInput

## 3.4.0

- Added new color schemes 'Rocket', 'Mako', 'Hawaii', 'Batlow',  
  'albedo', 'albedo2'
- Fix issue when user directory is changed
- Additional color options for stripe plots
- Circular stripe plots inspired by Emanuele Bevacqua (see emanuele.bevacqua.eu)
- New option to (just) merge nc files in Prepare by using box_mergetime,
  which is faster, but has no checks or adaptations

## 3.3.1

- Bug fix in monitor_climate: lon and lat were handed over with wrong name
- For each selected 'Group of Operators' a small description text is shown
- Added choice for border color
- Added new color scheme 'larry'
- Added warning if user directory does not exist

## 3.3.0

- Add new option in Prepare panel to open data via URL

## 3.2.0

- Add new operators timavg, num_above, num_below, num_equal,
  mon_num_above, mon_num_below, mon_num_equal
- Add operators cmsaf.mul and cmsaf.div to Toolbox
- New slider in visualizer to change image ratio
- Adaptation to source information for figures

## 3.1.1

- Minor update of dates and mail address
- Bug fix in box_mergetime and get_time due to unit 'seconds since'

## 3.1.0

- Extend functionalities of Climate Analysis
- Add new operators (for details see [cmsafops package](https://cran.r-project.org/package=cmsafops))
- Add option to use NetCDF files as input in Prepare step
- Add functionalities to compare data
- Update required package dependencies
- Bug fixes

## 3.0.0

- Add new functionalities for Climate Analysis
- Add option to export data in KML, GeoTiff or CSV
- Add option to export figures as JPEG or PDF
- Update required package dependencies
- Add new operator timcumsum
- For older versions of the package see [cmsaf package](https://cran.r-project.org/package=cmsaf).

## Older versions

For older versions, check the changelog of the [cmsafops package](https://cran.r-project.org/package=cmsafops). Note that the toolbox used to be separate from any package before 2.0.1.
