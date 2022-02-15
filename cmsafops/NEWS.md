# Changelog for cmsafops package

## 1.2.4
- Adaptation of time sorting in box_mergetime 
- Minor bug fix for timcumsum, which might occur in rare cases

## 1.2.3
- Bug fix in mon.anomaly: wrong definition of time unit
- Bug fix in selpoint.multi: time is now double, not integer
- Add time sorting of filelist to box_mergetime

## 1.2.2
- Correction of cmsaf_info attribute

## 1.2.1
- Modification of operators to work with data via URL

## 1.2.0
- Add new operators: timavg, num_above, num_below, num_equal,
  mon_num_above, mon_num_below, mon_num_equal
- Bug fix for wrong time stamp in selpoint.multi
- Bug fix for box_mergetime when time unit is seconds

## 1.1.1
- Fix a bug, which led to an error using cmsafvis anomaly_map

## 1.1.0
- Add new operators: cmsaf.abs, cmsaf.detrend, cmsaf.mk.test, cmsaf.regres, dayavg,
  daymax, daymin, daypctl, daysd, dayvar, fldcor, fldcovar, fldrange, fldsd, 
  gridboxmax, gridboxmean, gridboxmin, gridboxrange, gridboxsd, gridboxsum,
  gridboxvar, hourmean, hoursum, mermean, monavg, mondaymean, monvar, runmax,
  runmean, runmin, runrange, runsd, runsum, seasvar, timcor, timcovar, timselmean,
  timselsum, trend_advanced, ydaymax, ydaymin, ydayrange, ydaysd, ydrunmean,
  ydrunsum, yearrange, yearvar, ymonsd, yseassd, zonmean, zonsum
- Increase efficiency of some operators


## 1.0.0

- Split up old cmsaf package into cmsaf (containing only the toolbox) and cmsafops (containing base operators)
- Add new operator for accumulating data over time (timcumsum)
- Improve stability for large data for frequently used operators
- Fix several bugs
- Depend on R >= 3.6
- Update all package dependencies to recent version

## 2.0.1 (originally in cmsaf package)

Published 2019-08-26

- remove output to stdout
- functions show a warning if input variable is not found
- add consistent input parameter checks
- existing output files are not overwritten by default any more (use argument 'overwrite = TRUE' for old functionality
- remove function 'checkfile'
- add check for correct input of nc_version
- add function 'add_grid_info' for adding grid info to non-standard projections
- replace function 'remapbil' by the new function 'remap' for mapping non-standard grid data to standard grid data
- improve error handling and error messages
- add argument 'level' to function 'box_mergetime'
- use 'Imports' instead of 'Depends' for package dependencies (meaning that dependencies are not automatically attached)
- require R version 3.4
- add argument 'verbose' to all operators which leads to progress and timing information being shown
- deal with missing attributes in a more consistent way
- fix capitalization issue with 'info' attribute
- integrate toolbox into the package (run function 'run_toolbox' to open it)
- all temporal operators ('timmax', 'timmin', 'timmean', 'timpctl', 'timsum' and 'timsd') can now deal with large input files
- Change default netcdf version of output files from 3 to 4

## 1.9.5 (originally in cmsaf package)

timsum: bug fix - zero instead of NA
        new na.rm option of treatment of NA values
box_mergetime, cmsaf.cat, levbox_mergetime: bug fix - now merging of files with more than one time step is allowed
timmean: new na.rm option of treatment of NA values
seassum: new function for seasonal sums
seas.anomaly, seasmean, seassum: bug fix - only monthly input was working

Apply.Function.R: bug fix - timpctl default was not working
                  seassum operator added
CMSAF-R-TOOLBOX.R: browsing from last file's position
CMSAF_Visualizer.R: bug fix - caption of colorbar was not saved properly

## 1.9.4 (originally in cmsaf package)

ydaymean: bug fix because function caused an error on two of ten test platforms
Prep.Data.R: bug fix for no plot in case of longitude instead of lon (same for lat)
shinyApp_toolbox.R: bug fix for problem with file selection after update to shinyFiles 0.7.0
minor bug fixes

## 1.9.3 (originally in cmsaf package)

ydaymean: function caused an error on two of ten test platforms;
          first bug fix unfortunately did not work

## 1.9.2 (originally in cmsaf package)

read_ncvar: renamed because of conflicts with other packages
            output as list including time dimension (if applicable)
all functions with file output: export of global attributes to output file
fldmean, fldmin, fldmax, wfldmean: lon and lat are set to the mid-point of the applied field
timsum: new function for all-time sums
ydaymean: new function for multi-year daily sums
multimonmean, multimonsum, seasmean: bug fix if used with daily or even higher resolved data
cmsaf.addc, cmsaf.mulc, cmsaf.divc, cmsaf.subc: added info about used function in meta data
multimonmean: bug fix in cmsaf.info attribute
CMSAF_Visualizer: additional color palette; error check for ppt; exit button to stop app;
                  bug fix for saving analyse.timeseries plot; clean up at end;
                  Analyse.TimeSeriesTrend.R is now included in Analyse.TimeSeries;
                  option to plot data from R-Instat RData files; plot own locations
Prep.Data.R: preview of available and chosen region; clean up at end; show standard name
Apply.Function.R: clean up at end; more functions of cmsaf packages added; option for nc4
CMSAF-R-TOOLBOX: add script that controls all main toolbox scripts by shinyApp_toolbox.R
                 - this includes minor modifications on Prep.Data.R, Apply.Function.R,
                   CMSAF_Visualizer.R
                 as file.choose works only in interactive sessions, additional way to choose
                 files with shinyFiles; remember last selected file
Adapted folder structure in R-scripts
Minor performance improvements in R-scripts

## 1.9.1 (originally in cmsaf package)

all operators with var input: no case sensitivity for var and if var not found default var is used; operator name is included in output NetCDF
selpoint: file extension is checked; if problems occur, nc is used as default
selpoint.multi: file extension is checked; if problems occur, nc is used as default
checkfile: nc file extension of outfile is checked; if problems occur, nc is used as default
Prep.Data.R & Apply.Function.R: bug fix if path contains identical entries; path length under
                                Windows is restricted to < 260
                                added check for startdate and enddate input
                                variable SATID will be no default variable
Apply.Function.R: operator cmsaf.mulc added
                  option for operator trend added
                  when applying another function, user can choose to which one
Apply.Function.R & CMSAF_Visualizer: add options to improve the visualization
                                     add tabs for additional information
                                     bug fix for wrong imagesize of analyze.timeseries plot
                                     additional projection (orthographic) added

## 1.9.0 (originally in cmsaf package)

box_mergetime: bug fix in time_sorting if applied to just one file
box_mergetime: if given variable is not included in file, the first included one will be used
box_mergetime: bug fix if time_bnds are included
levbox_mergetime: include time_sorting
levbox_mergetime: if given variable is not included in file, the first included one will be used
levbox_mergetime: bug fix if time_bnds are included
selpoint.multi: bug fix: tbnds was not correctly defined in case of a given infile
multimonmean: bug fix: Error in seas[j, i] <- dum : replacement has length zero
fldmin: bug fix if applied to just one time step
fldmax: bug fix if applied to just one time step
trend: bug fix: Error for NA values
trend: bug fix for maxval warning and increased sample number (3 to 5)
trend: include option keyword for option 1 (faster trend calculation, but whole data field
       is read in) and option 2 (same calculation, but sequential, if data file is too big
       for memory; needs much more time)
checkfile: bug fix: in some cases a wrong status was given
timpctl: new function to determine a given percentile
cmsaf.cat: new function to concatenate datasets of several input files
Apply.Function.R and Prep.Data.R: put sys.frame intp try-function to avoid error, when scripts are
                                  started from terminal

## 1.8.3 (originally in cmsaf package)

multimonsum: bug fix: NA values were converted into zero
multimonsum: bug fix: Error in seas[j, i] <- dum : replacement has length zero

## 1.8.2 (originally in cmsaf package)

selpoint.multi: bug fix if only one location is selected
selpoint.multi: bug fix remove 'any' command
Apply.Function.R and CMSAF_Visualizer.R: improved plotting of 1D-timeseries;
                     additional plotting options;
                     bug fix for wrong latitude in HLW and HSH data;
      requires additional package 'colourpicker'
Prep.Data.R:        no need for path information; path and file will be chosen by file.chose and
                    depending on current file directory;
            if NetCDF contains only one variable no need for input;
            option for internal tar function if problems under Windows occur;
            error message if sinusoidal projection is used; fix dums warning;
            bug fix because of undefined slash;
            no case sensitive input anymore;
            additional checks for correct input otherwise defaults are used;
            start- and enddate are not needed - push ENTER is enough;
Apply.Function.R:   no need for path information; path and file will be chosen by file.chose and
                    depending on current file directory;
            if NetCDF contains only one variable no need for input;
            close all graphic devices in the end;
            bug fix because of undefined slash;
            no case sensitive input anymore;
            additional checks for correct input otherwise defaults are used;
            bug fix: HOAPS figures could be upside down because of latitude sorting
            data file can be in any folder (not only data or output)
CMSAF_Visualizer.R: no need for path information; path and file will be chosen by file.chose and
                    depending on current file directory;
            if NetCDF contains only one variable no need for input;
            longname in title as default;
            proper display of HOAPS data;
            fix warning for RB-colorbar;
            close all graphic devices in the end;
            include AnalyzeTimeseries;
            dynamic width and height;
            bug fix because of undefined slash
            include new CM SAF R Toolbox logo;
            interactive Analyse.Timeseries;
            bug fix: error if trend line for daily data

## 1.8.1 (originally in cmsaf package)

all functions with file output: bug fix for wrong reading of dimensional variables
yseasmean: bug fix - now daily data can be used
yseasmax:  bug fix - now daily data can be used
yseasmin:  bug fix - now daily data can be used
yseassd:   bug fix - now daily data can be used

## 1.8.0 (originally in cmsaf package)

box_mergetime: bug fix if only one file corresponds to pattern
box_mergetime: order of timesteps will be checked
all functions with file output: bug fix for use with CRU data due to missing standard_name of longitude
all functions with file output: bug fix for input files with dimension nb2
cmsaf.sub: bug fix variable precision and setting of fill value; will be done for other functions, too
selpoint.multi: new function to select multiple locations from a single file or a bunch of files
all functions with file output: new option for NetCDFv4 output
trend: additional output field for trend without multiplication with length of time series
trend: bug fix in trend calculation, which could lead to wrong trend values
trend: faster trend calculation
multimonmean: bug fix wrong if statement could lead to error of month not included
multimonsum: bug fix wrong if statement could lead to error of month not included
Prep.Data.R: all tar-files with same ORD number are handled in one step

## 1.7.2 (originally in cmsaf package)

Prep.Data.R: remove functionality to avoid overwriting; now capable to handle daily and hourly data;
         HSH data can be used; extracted files can be deleted
Apply.Function.R: bug fix for choice 26; add option for CM SAF R Visualizer; more info messages added
Plot.TimeSeries.R: bug fix for HLW and HSH products
Analyse.TimeSeries.R: bug fix for HLW and HSH products
Install.Packages4Toolbox.R: new script for easier installation of necessary packages
timmean: bug fix if lon x lat x time is too large
selmon: bug fix matching months where not found
yearsum: bug fix sum of NA values is now NA instead of 0
box_mergetime: bug fix wrong time bounds
levbox_mergetime: bug fix wrong time bounds
extract.levels: new function to extract one or all levels from 4D NetCDF files

## 1.7.1 (originally in cmsaf package)

Bug fix: Precision info in netcdf, which is not conform to standard, could lead to error
Prep.Data.R: Adaptations to netcdf4 were done

## 1.7.0 (originally in cmsaf package)

Plot.Region.R no RNetCDF dependency anymore
Plot.Region.Multi.R no RNetCDF dependency anymore
Analyse.TimeSeries.R no RNetCDF dependency anymore
Analyse.TimeSeriesTrend.R no RNetCDF dependency anymore
Plot.Region.R round thick labels to be shorter
Plot.Region.Multi.R round thick labels to be shorter
Export.TimeSeries.R deleted from toolbox, as selpoint function can export to netcdf or csv
ncinfo: info="l" returns info and list
Precision of time bounds: bug fix set precision to double
The default precision was set from "double" to "float" to reduce file size
Functions, which do not change the original values, check for variable precision
monmax: new function to get monthly maxima
monmin: new function to get monthly minima
monsd: new function to get monthly standard deviations
ymonmax: new function to get multi-year monthly maxima
ymonmin: new function to get multi-year monthly minima
ymonsd: new function to get multi-year monthly standard deviations
yseasmax: new function to get multi-year seasonal maxima
yseasmin: new function to get multi-year seasonal minima
yseassd: new function to get multi-year seasonal standard deviations
timmax: new function to get all-time maxima
timmin: new function to get all-time minima
timsd: new function to get all-time seasonal standard deviations
remapbil: grids with coordinates 0-360 can be used now as infile1
fldmean: bug fix if infile has only one timestep
mon.anomaly: bug fix adaptation of fill values (e.g.-1 could lead to errors)
seas.anomaly: bug fix adaptation of fill values (e.g.-1 could lead to errors)
monsum: bug fix sum of NA values is now NA instead of 0

## 1.6.1 (originally in cmsaf package)

selpoint: bug fix in strsplit "\\." instead of "."
Apply.Function.R: bug fix l.57 go_on="n" has to be set to go_on="y"

## 1.6

get_time: get_time can now handle Julian date
get_time: higher precision for time unit 'months'
get_time: time units without 'since' give an error message
box_mergetime: error message if coordinates are outside the target area
levbox_mergetime: error message if coordinates are outside the target area
cmsaf.addc: new function to add a constant
cmsaf.divc: new function to divide by a constant
cmsaf.mulc: new function to multiply with a constant
cmsaf.subc: new function to subtract a constant
selpoint: add option for output of a csv-file
selpoint: add check for correct file extension
read_nc: new function to read data into R

## 1.5

Due to some problems with the installation of the 'RNetCDF' package, the 'RNetCDF'
function 'utcal.nc' was replaced by an own, quite simple, function to extract the
time information. The function 'get_time' converts the time step and time unit
information into POSIXct times. Thus, the 'RNetCDF' package is no more required
for the 'cmsaf' package.

## 1.4

levbox_mergetime: change back from as.Date to as.POSIXct to keep hourly timesteps
levbox_mergetime: difftime can only handle units up to "weeks", now "months" are possible
box_mergetime: change back from as.Date to as.POSIXct to keep hourly timesteps
box_mergetime: difftime can only handle units up to "weeks", now "months" are possible
Prep.Data.light.R: same as Prep.Data.R but without untar and unzip
ymonsum: new function to calculate multi-year monthly sums
multimonmean: new function to calculate multi-monthly means
multimonsum: new function to calculate multi-monthly sums
year.anomaly: new function to calculate annual anomalies
seas.anomaly: new function to calculate seasonal anomalies
mon.anomaly: ymonsub was renamed to mon.anomaly
all functions: standard precision is set to double, the previous automatically check
           could lead to imprecisions
selperiod: new function to select periods of dates from a timeseries
extract.period: new function to remove periods of dates from a timeseries
trend: wrong line break to show progress was corrected
Prep.Data.R: if files are already unzipped, they will be overwritten, deleting is not
             necessary anymore
Prep.Data.R: default for the functions loop is set to n
Prep.Data.R: HLW data are recognized and levbox_mergetime is used in this case
Prep.Data.R: ymonsub is renamed to mon.anomaly
Apply.Function.R: new cmsaf_1.4 functions added
Apply.Function.R: default for the functions loop is set to n
Apply.Function.R: HLW data are recognized and levbox_mergetime is used in this case
Apply.Function.R: ymonsub is renamed to mon.anomaly
Plot.Region.R: add option to plot user defined locations

## 1.3.4 (originally in cmsaf package)

Final version for the 2015 CM SAF Training Workshop in Pretoria, South Africa.
