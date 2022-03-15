# the CM SAF R Toolbox.
#
# You should not use this R-script on its own!
#
# Have fun with the CM SAF R TOOLBOX!
#                                              (Steffen Kothe / CM SAF 2022-03-15)
#__________________________________________________________________________________

# Use home directory for storing config file (e.g. C:\Users\<user>\Documents
# on Windows or /home/<user> on Linux)
config_directory <- file.path(path.expand("~"), "CMSAF-Toolbox")
config_filepath <- file.path(config_directory, "config.conf")
grid_filepath <- file.path(config_directory, "myGrid.txt")

# ignore warnings
options(warn = -1)

# Is a local or remote session?
isRunningLocally <- Sys.getenv("SHINY_PORT") == ""
# Test Remote
# isRunningLocally <- FALSE
remoteVolume <- c(data = "/srv/shiny-server/toolbox/data")

# Prepare stuff
operatorGroupsPrepare <- c("Diurnal means" = "daymean",
                           "Diurnal averages" = "dayavg",
                           "Diurnal sums" = "daysum",
                           "Monthly means" = "monmean",
                           "Monthly averages" = "monavg",
                           "Monthly sums" = "monsum",
                           "Time selection mean" = "timselmean",
                           "Time selection sum" = "timselsum")

# Analyze stuff
operatorGroups <- c("Hourly statistics",
                    "Daily statistics",
                    "Monthly statistics",
                    "Seasonal statistics",
                    "Annual statistics",
                    "Temporal operators",
                    "Time range statistics",
                    "Running statistics",
                    "Zonal statistics",
                    "Meridional statistics",
                    "Grid boxes statistics",
                    "Spatial operators",
                    "Correlation and covariance",
                    "Mathematical operators",
                    "Selection",
                    "Data manipulation",
                    "Climate Analysis",
                    "Compare Data")

operators <- c()

operators[["Mathematical operators"]] <- c("Compute absolute values" = "cmsaf.abs",
                                           "Add constant to data" = "cmsaf.addc",
                                           "Divide data by constant" = "cmsaf.divc",
                                           "Multiply data with constant" = "cmsaf.mulc",
                                           "Subtract constant from data" = "cmsaf.subc",
                                           "Divide by days per month" = "divdpm",
                                           "Multiply by days per month" = "muldpm",
                                           "Add values from another file" = "cmsaf.add",
                                           "Subtract values from another file" = "cmsaf.sub",
                                           "Multiply values from another file" = "cmsaf.mul",
                                           "Divide by values from another file" = "cmsaf.div")

operators[["Hourly statistics"]] <- c("Hourly mean" = "hourmean",
                                      "Hourly sum" = "hoursum")

operators[["Daily statistics"]] <- c("Diurnal averages" = "dayavg",
                                     "Diurnal maxima" = "daymax", 
                                     "Diurnal means" = "daymean",
                                     "Diurnal minima" = "daymin",
                                     "Diurnal percentiles" = "daypctl",
                                     "Diurnal range" = "dayrange",
                                     "Diurnal standard deviations" = "daysd",
                                     "Diurnal sums" = "daysum",
                                     "Diurnal variances" = "dayvar",
                                     "Multi-year daily maxima" = "ydaymax",
                                     "Multi-year daily means" = "ydaymean",
                                     "Multi-year daily minima" = "ydaymin",
                                     "Multi-year daily range" = "ydayrange",
                                     "Multi-year daily standard deviations" = "ydaysd",
                                     "Multi-year daily sums" = "ydaysum")

operators[["Monthly statistics"]] <- c("Monthly anomalies" = "mon.anomaly",
                                       "Monthly averages" = "monavg",
                                       "Monthly maxima" = "monmax",
                                       "Monthly means" = "monmean",
                                       "Monthly minima" = "monmin",
                                       "Monthly percentiles" = "monpctl",
                                       "Monthly standard deviation" = "monsd",
                                       "Monthly sums" = "monsum",
                                       "Monthly variances" = "monvar",
                                       "Mean monthly daily variation" = "mondaymean",
                                       "Multi-monthly means" = "multimonmean",
                                       "Multi-monthly sums" = "multimonsum",
                                       "Multi-year monthly maxima" = "ymonmax",
                                       "Multi-year monthly means" = "ymonmean",
                                       "Multi-year monthly minima" = "ymonmin",
                                       "Multi-year monthly standard deviations" = "ymonsd",
                                       "Multi-year monthly sums" = "ymonsum",
									   "Monthly number of timesteps above threshold" = "mon_num_above",
									   "Monthly number of timesteps below threshold" = "mon_num_below",
									   "Monthly number of timesteps equal threshold" = "mon_num_equal")

operators[["Seasonal statistics"]] <- c("Seasonal anomalies" = "seas.anomaly",
                                        "Seasonal means" = "seasmean",
                                        "Seasonal standard deviations" = "seassd",
                                        "Seasonal sums" = "seassum",
                                        "Seasonal variances" = "seasvar",
                                        "Multi-year seasonal maxima" = "yseasmax",
                                        "Multi-year seasonal means" = "yseasmean",
                                        "Multi-year seasonal minima" = "yseasmin",
                                        "Multi-year seasonal standard deviations" = "yseassd")

operators[["Annual statistics"]] <- c("Annual anomalies" = "year.anomaly",
                                      "Annual maxima" = "yearmax",
                                      "Annual means" = "yearmean",
                                      "Annual minima" = "yearmin",
                                      "Annual range" = "yearrange",
                                      "Annual standard deviations" = "yearsd",
                                      "Annual sums" = "yearsum",
                                      "Annual variances" = "yearvar")

operators[["Zonal statistics"]] <- c("Zonal means" = "zonmean",
                                     "Zonal sums" = "zonsum")

operators[["Meridional statistics"]] <- c("Meridional means" = "mermean")

operators[["Running statistics"]] <- c("Running maxima" = "runmax",
                                       "Running means" = "runmean",
                                       "Running minima" = "runmin",
                                       "Running range" = "runrange",
                                       "Running standard deviations" = "runsd",
                                       "Running sums" = "runsum",
                                       "Multi-year daily running means" = "ydrunmean",
                                       "Multi-year daily running standard deviations" = "ydrunsd",
                                       "Multi-year daily running sums" = "ydrunsum")

operators[["Grid boxes statistics"]] <- c("Gridbox maxima" = "gridboxmax",
                                          "Gridbox means" = "gridboxmean",
                                          "Gridbox minima" = "gridboxmin",
                                          "Gridbox range" = "gridboxrange",
                                          "Gridbox standard deviations" = "gridboxsd",
                                          "Gridbox sums" = "gridboxsum",
                                          "Gridbox variances" = "gridboxvar")

operators[["Temporal operators"]] <- c("All-time average" = "timavg",
									                     "All-time maxima" = "timmax",
                                       "All-time mean" = "timmean",
                                       "All-time minima" = "timmin",
                                       "All-time percentile" = "timpctl",
                                       "All-time standard deviation" = "timsd",
                                       "All-time sum" = "timsum",
                                       "Detrend" = "cmsaf.detrend",
                                       "Linear trend" = "trend",
                                       "Mann-Kendall Test" = "cmsaf.mk.test",
                                       "Regression" = "cmsaf.regres",
                                       "Multiple linear regression" = "trend_advanced",
									   "Number of timesteps above threshold" = "num_above",
									   "Number of timesteps below threshold" = "num_below",
									   "Number of timesteps equal threshold" = "num_equal"								                     
                                       )

operators[["Time range statistics"]] <- c("Time selection mean" = "timselmean",
                                          "Time selection sum" = "timselsum"
                                       )

operators[["Spatial operators"]] <- c("Spatial maximum" = "fldmax",
                                      "Spatial mean" = "fldmean",
                                      "Spatial minimum" = "fldmin",
                                      "Spatial range" = "fldrange",
                                      "Spatial standard deviations" = "fldsd",
                                      "Spatial sums" = "fldsum",
                                      "Weighted spatial mean" = "wfldmean")

operators[["Correlation and covariance"]] <- c("Correlation in grid space" = "fldcor",
                                               "Covariance in grid space" = "fldcovar",
                                               "Correlation over time" = "timcor",
                                               "Covariance over time" = "timcovar")

operators[["Selection"]] <- c("Select region by longitude and latitude" = "sellonlatbox",
                              "Select data at given point" = "selpoint",
                              "Select data at multiple points" = "selpoint.multi",
                              "Select list of months" = "selmon",
                              "Select time period" = "selperiod",
                              "Select list of years" = "selyear",
                              "Select list of times" = "seltime",
                              "Remove time period" = "extract.period")

operators[["Data manipulation"]] <- c("Grid interpolation" = "remap",
                                      "Match Data" = "cmsaf.adjust.two.files")

operators[["Climate Analysis"]] <- c("Absolute map" = "absolute_map",
                                     "Anomaly map" = "anomaly_map",
                                     "Climatology map" = "climatology_map",
                                     "Fieldmean plot" = "fieldmean_plot",
                                     "Fieldmean and anomaly map" = "fieldmean_and_anomaly_map",
                                     "Stripes Plot" = "warming_stripes_plot",
                                     "Time Series Plot" = "time_series_plot",
                                     "Trend Plot" = "trend_plot"
)

operators[["Compare Data"]] <- c("Difference plot (absolute)" = "cmsaf.diff.absolute",
                                 "Difference plot (relative)" = "cmsaf.diff.relative",
                                 "Scatterplot" = "cmsaf.scatter",
                                 "Histogram" = "cmsaf.hist",
                                 "Side-by-Side plot" = "cmsaf.side.by.side",
                                 "Comparison of time series" = "cmsaf.time.series",
                                 "Hovmöller plot" = "cmsaf.hovmoller",
                                 "Show statistics" = "cmsaf.stats"
)

operatorOptions <- c("constant",
                     "region",
                     "point",
                     "useFastTrend",
                     "dateRange",
                     "percentile",
                     "gridbox",
                     "months",
                     "years",
                     "times",
                     "method",
                     "monitor_climate",
                     "file_select",
                     "file_selection",
                     "running",
                     "timeRange",
                     "compare_data",
                     "threshold")

operatorOptionsDict <- c()
operatorOptionsDict[["constant"]] <- c("cmsaf.addc",
                                       "cmsaf.divc",
                                       "cmsaf.mulc",
                                       "cmsaf.subc")
operatorOptionsDict[["threshold"]] <- c("num_above",
                                        "num_below",
                                        "num_equal",
										                "mon_num_above",
										                "mon_num_below",
										                "mon_num_equal")
operatorOptionsDict[["region"]] <- c("sellonlatbox",
                                     "absolute_map",
                                     "anomaly_map",
                                     "climatology_map",
                                     "fieldmean_plot",
                                     "fieldmean_and_anomaly_map")
operatorOptionsDict[["point"]] <- c("selpoint", "selpoint.multi")
operatorOptionsDict[["useFastTrend"]] <- c("trend")
operatorOptionsDict[["dateRange"]] <- c("selperiod",
                                     "extract.period",
                                     "absolute_map",
                                     "anomaly_map",
                                     "climatology_map",
                                     "fieldmean_plot",
                                     "fieldmean_and_anomaly_map")
operatorOptionsDict[["percentile"]] <- c("timpctl", 
                                         "daypctl", 
                                         "monpctl")
operatorOptionsDict[["gridbox"]] <- c("gridboxmax", 
                                      "gridboxmean", 
                                      "gridboxmin", 
                                      "gridboxrange", 
                                      "gridboxsd", 
                                      "gridboxsum", 
                                      "gridboxvar")
operatorOptionsDict[["running"]] <- c("runmax", 
                                      "runmean", 
                                      "runmin", 
                                      "runrange", 
                                      "runsd", 
                                      "runsum",
                                      "ydrunmean",
                                      "ydrunsd",
                                      "ydrunsum")
operatorOptionsDict[["timeRange"]] <- c("timselmean", 
                                        "timselsum")
operatorOptionsDict[["months"]] <- c("selmon", "multimonmean", "multimonsum")
operatorOptionsDict[["years"]] <- c("selyear")
operatorOptionsDict[["times"]] <- c("seltime")
operatorOptionsDict[["method"]] <- c("remap")
operatorOptionsDict[["monitor_climate"]] <- c("absolute_map",
                                              "anomaly_map",
                                              "climatology_map",
                                              "fieldmean_plot",
                                              "fieldmean_and_anomaly_map",
                                              "warming_stripes_plot",
                                              "time_series_plot",
                                              "trend_plot")
operatorOptionsDict[["file_select"]] <- c("cmsaf.add", 
                                          "cmsaf.sub",
                                          "cmsaf.mul",
                                          "cmsaf.div")
operatorOptionsDict[["file_selection"]] <- c("fldcor", 
                                             "fldcovar", 
                                             "timcor", 
                                             "timcovar", 
                                             "trend_advanced",
                                             "cmsaf.adjust.two.files")
operatorOptionsDict[["compare_data"]] <- c( "cmsaf.diff.absolute", 
                                            "cmsaf.diff.relative", 
                                            "cmsaf.scatter", 
                                            "cmsaf.hist",
                                            "cmsaf.side.by.side", 
                                            "cmsaf.time.series",
                                            "cmsaf.hovmoller",
                                            "cmsaf.stats")

# default plot settings
textsize    <- 1.2
linesize    <- 1.5
bordercolor <- "gray20"
# imagewidth  <- -1         # if -1 image dimensions are taken from data
# imageheight <- -1
na.color    <- "gray80"
image_def <- 800         # default image size
#ihsf      <- 0.1        # default image heigth scale factor (new slider for ihsf in visualizer)
grid_col  <- "cornsilk2" # default color of grid lines
plot_grid <- TRUE        # plot grid lines (TRUE = yes, FALSE = no)

# Load countriesHigh data.
countriesHigh <- numeric(0)
data(countriesHigh, package = "rworldxtra")

# Valid countries  # TODO duplicated code in cmsafvis/data-raw/generate_internal_data.R
# The package needs only three specific columns from the original data.
codes <- countrycode::codelist[, c("iso3c", "country.name.en", "country.name.de")]
# Apparently, in R 4.0.0 this returns a tibble which leads to an error because
# tibble indexing works differently. See #873 and #894.
codes <- as.data.frame(codes)

# Clear out rows where iso3c is NA. Since we always use iso3c as 'origin',
# we don't need these rows.
codes <- codes[!is.na(codes["iso3c"]), ]

# Only allow countries which are represented in countriesHigh
ids <- codes[, "iso3c"] %in% countriesHigh$ISO3.1
codes <- codes[ids, ]

# Add data for special cases.
codes <- rbind(
  codes,
  c("EUR", "Europe", "Europa"),
  c("AFR", "Africa", "Afrika"),
  c("TOT", "Totaldisk", "Gesamtausschnitt"),
  c("S_A", "Selected Area", "Gewähltes Gebiet")
)

source("ColorspacePrivates.R")
