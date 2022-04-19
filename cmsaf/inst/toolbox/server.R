# the CM SAF R Toolbox.
#
# You should not use this R-script on its own!
#
# Have fun with the CM SAF R TOOLBOX!
#                                              (Steffen Kothe / CM SAF 2022-03-17)
#__________________________________________________________________________________

# Function to compute first of month
firstOfMonth <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# Get tarlist sorted by name.
getTarList <- function(path_to_tar, tar_flag = 1, includeClaasAux = FALSE) {
  
  ordname <- cmsafops::get_basename(path_to_tar)
  ordpath <- dirname(path_to_tar)

  if (identical(path_to_tar, character(0))) {
    stop(paste0("No infile!", var))
  }

  flist <- list.files(ordpath, substr(ordname, 1, 8), full.names = TRUE)
 
  tarlist <- NULL
  for (file in flist) {
    if (tar_flag == 1) {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE, tar = "internal"))
    } else if (tar_flag == 2) {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE, tar = Sys.getenv("TAR")))
    } else if (tar_flag == 3) {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE, tar = "C:/Rtools/bin/tar.exe"))
    } else {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE))
    }
  }
  # Exlude claas aux file if not wanted.
  if (!includeClaasAux) {
    tarlist <- tarlist[tarlist != "CM_SAF_CLAAS2_L2_AUX.nc"]
  }

  tarlist <- sort(tarlist)
  
  return(tarlist)
}

# Get CLAAS AUX FLAG
getClaasAuxFlag <- function(tarlist) {
  return("CM_SAF_CLAAS2_L2_AUX.nc" %in% tarlist)
}

# A function to extract ALL dates from a tar file.
extractAllDates <- function(path_to_tar, tar_flag) {
  # List the to be untared  file.
  tarlist <- getTarList(path_to_tar, tar_flag = tar_flag)

  # Timestep HAS TO BE RELEVANT FOR SOMETHING! FIND THAT OUT!!
  timestep <- substr(tarlist[1], 4, 4)

  allDates <- as.character(substr(tarlist, 6, 13))
  return(list(allDates = as.Date(allDates, "%Y%m%d"), timestep = timestep))
}

# A function to extract start and end dates.
extractDateRange <- function(path_to_tar, tar_flag) {

  tmp <- extractAllDates(path_to_tar, tar_flag = tar_flag)

  # Select earliest date from the files.
  date_from <- min(tmp$allDates)
  date_to   <- max(tmp$allDates)

  return(list(date_from = date_from, date_to = date_to, timestep = tmp$timestep))
}

# A function to extract ALL dates from .nc-files.
extractAllDatesNc <- function(path_to_nc, nc_flag) {
  
  ordname <- cmsafops::get_basename(path_to_nc)
  ordpath <- dirname(path_to_nc)
  
  if (identical(path_to_nc, character(0))) {
    stop(paste0("No infile!", var))
  }
  
  fileslist <- list.files(ordpath, full.names = TRUE)
  pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
  
  result.fileslist <- NULL
  for(i in 1:length(fileslist)){
    regex <- regmatches(fileslist[i], regexpr(pattern, fileslist[i]))
    if(!is.null(regex))
      result.fileslist <- append(result.fileslist, regex)
    regex <- NULL
  }
  result.fileslist <- sort(result.fileslist)
  
  timestep <- cmsafops::calc_timestepNc(result.fileslist, ordpath)
  
  # # Timestep HAS TO BE RELEVANT FOR SOMETHING! FIND THAT OUT!!
  # timestep <- substr(result.fileslist[1], 4, 4)
  allDates <- cmsafops::calc_allDatesNc(result.fileslist, ordpath)
  
  # allDates <- as.character(substr(result.fileslist, 6, 13))
  
  return(list(allDates = as.Date(allDates, "%Y%m%d"), timestep = timestep))
}

# A function to extract start and end dates of NetCDF files
extractDateRangeNc <- function(path_to_nc, nc_flag) {
  tmp <- extractAllDatesNc(path_to_nc, nc_flag = nc_flag)
  
  # Select earliest date from the files.
  date_from <- min(tmp$allDates)
  date_to   <- max(tmp$allDates)
  
  return(list(date_from = date_from, date_to = date_to, timestep = tmp$timestep))
}

# Function for OS-Independently choosing a directory
choose_directory <- function(caption = "Select data directory") {
  if (exists("utils::choose.dir") || exists("choose.dir")) {
    utils::choose.dir(caption = caption)
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}

# Validate an .nc URL
# Parameters
# x: a string containing a URL to validate whether it leads to a .nc file
# Value
# -1 if cannot connect to the URL
# TRUE if URL appears to lead to a .nc file and FALSE if not.
# Details
# It is suggested to use this check only if ncdf4::nc_open() fails.
# Some valid .nc URLs do not return the "application/x-netcdf" content-type.
# An additional proxy check requiring a "text" or "application/octet-stream" content-type 
# and ".nc" appearing in the URL is used to ensure these URLs are considered valid.
# However, this may lead to some invalid URLs returning TRUE, 
# which will be detected later in the app.
valid_nc_url <- function(x) {
  con_type <- ""
  try({
    url_head <- httr::HEAD(x)
    con_type <- httr::headers(url_head)[["content-type"]]
    stat_code <- as.character(httr::status_code(url_head))
  })
  # If the content type is blank or the status code begins with 4 e.g. 404 
  # then it's likely the URL is invalid.
  if (con_type == "" || startsWith(stat_code, "4")) return(-1)
  else if (con_type %in% c("application/x-netcdf", "application/x-netcdf4")) return(TRUE)
  else {
    # Many URL which lead to NetCDF file e.g. links from THREDDS servers,
    # do not have the NetCDF content type, hence an additional check is needed.
    # These often have a "text" content type.
    # Some NetCDF files are also coded as "application/octet-stream" which is like "unknown" or "generic"
    return((startsWith(con_type, "text/plain") || startsWith(con_type, "text/html") || con_type == "application/octet-stream") && grepl(".nc", x))
  }
}

# Strips suffix of .nc URL to get base .nc URL
# e.g. by removing "dataset.html" or "dataset.xml" at the end of the URL provided by a user
base_ncss_url <- function(x) {
  if (!endsWith(x, ".nc")) {
    x <- gsub(pattern = "([^/]+$)", replacement = "", x)
    x <- gsub(pattern = "(\\/+$)", replacement = "", x)
    x
  } else x
}

# Gets the data description (XML file) from a NetCDF Subset Service (NCSS) URL.
# NULL is returned for invalid URLs.
# The data description is returned as a list constructed by xml2::as_list()
ncss_data_description <- function(x) {
  x <- base_ncss_url(x)
  # If the URL does not end in .nc then it will not be an ncss URL
  if (!endsWith(x, ".nc")) return(NULL)
  x <- paste0(x, "/dataset.xml")
  res <- try(xml2::read_xml(x))
  # If this fails then URL is not an ncss URL or no connection
  if (class(res) == "try-error") return(NULL)
  res <- xml2::as_list(res)
  res
}

# Constructs a query URL from an NCSS nc file.
# This is used to download a subset defined in panel_prepare_ncss_url_subset
construct_ncss_url <- function(base_url, var, north, west, east, south, h_stride = 1, time_start, time_end, time_stide = 1, accept = "netcdf") {
  if (missing(var)) stop("var must be supplied.")
  n_dir <- sum(!missing(north), !missing(west), !missing(east), !missing(south))
  if (n_dir > 0 & n_dir < 4) stop("All four lat/lon bounds must be supplied, or none for the full range.")
  if (missing(time_start) || missing(time_end)) stop("time_start and time_end must be supplied.")
  
  colon <- "%3A"
  q_url <- base_url
  q_url <- paste0(q_url, "?var=", var, "&")
  if (n_dir == 4) {
    q_url <- paste0(q_url, "north=", north, "&")
    q_url <- paste0(q_url, "west=", west, "&")
    q_url <- paste0(q_url, "east=", east, "&")
    q_url <- paste0(q_url, "south=", south, "&")
  }
  # This assumes time_start and time_end are "Date" types without time components
  # TODO add option for time_start and time_end to be different classes
  time_start <- paste0(time_start, "T", "00", colon, "00", colon, "00", "Z")
  time_end <- paste0(time_end, "T", "23", colon, "59", colon, "59", "Z")
  q_url <- paste0(q_url, "time_start=", time_start, "&")
  q_url <- paste0(q_url, "time_end=", time_end, "&")
  q_url <- paste0(q_url, "timeStride=", time_stide, "&")
  
  q_url <- paste0(q_url, "accept=", accept)
  q_url
}

months_list <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

# Constructs a list of query URLs from an NCSS nc file to extract a range of months for each year.
# This is used to download a subset defined in panel_prepare_ncss_url_subset
construct_ncss_url_month_extract <- function(base_url, var, north, west, east, south, h_stride = 1, month_start, month_end, year_start, year_end, accept = "netcdf") {
  month_min <- match(month_start, months_list)
  month_max <- match(month_end, months_list)
  month_max_next <- ifelse(month_max == 12, 1, month_max + 1)
  years <- year_start:year_end
  urls <- vector("character", length(years))
  for(i in seq_along(years)) {
    y_min <- years[i]
    y_max <- ifelse(month_min <= month_max, y_min, y_min + 1)
    time_start <- as.Date(paste(y_min, month_min, "1"), format = "%Y %m %d")
    # Get the first day of the next month and subtract 1 to get last day of month_max
    if (month_max == 12) time_end <- as.Date(paste(y_max + 1, month_max_next, "1"), format = "%Y %m %d")
    else time_end <- as.Date(paste(y_max, month_max_next, "1"), format = "%Y %m %d")
    time_end <- time_end - 1
    urls[i] <- construct_ncss_url(base_url = base_url, var = var, 
                                  north = north, west = west, east = east, south = south, 
                                  h_stride = h_stride,
                                  time_start, time_end, accept = "netcdf")
  }
  return(urls)
}
function(input, output, session) {
  #### Preparation and session set up ####
  # TODO: Setting the maximum request size. WARNING: NOT SURE WHAT'S A GOOD VALUE FOR THIS
  # FOR NOW SETTING TO 2.5 GB, as this exceeds the largest test data.
  options(shiny.maxRequestSize = 2500 * 1024^2)

  # Check if is running locally or on remote server
  # Variable can be found in global.R
  if (isRunningLocally) {
    shinyjs::show("tarFileLocal")
    shinyjs::show("ncFileLocal")
    shinyjs::show("or_prepare")
    shinyjs::show("ncURL")
    shinyjs::show("or_prepare2")
    shinyjs::show("ncFileLocal_analyze")
    shinyjs::show("ncFileLocal_visualize")
    shinyjs::show("shapefileLocal")

    # Let user be able to modify the output directory.
    shinyjs::show("modify_userDir")
    updateActionButton(inputId = "nc_url_download", label = "Download this file")
    updateActionButton(inputId = "ncss_subset_download", label = "Download this subset")
  } else {
    shinyjs::show("tarFileRemote")
    shinyjs::show("ncFileRemote")
    shinyjs::show("or_prepare")
    shinyjs::show("ncURL")
    shinyjs::show("or_prepare2")
    shinyjs::show("ncFileRemote_analyze")
    shinyjs::show("ncFileRemote_visualize")
    shinyjs::show("shapefileRemote")

    # Let user be able to download the output directory.
    shinyjs::show("downloader")
    
    updateActionButton(inputId = "nc_url_download", label = "Download to server")
    updateActionButton(inputId = "ncss_subset_download", label = "Download subset to server")
  }

  dir.create(config_directory, showWarnings = FALSE)

  if (file.exists(config_filepath)) {
    # Does user dir exist in config file?
    config_lines <- try(readLines(config_filepath))
  } else {
    config_lines <- ""
  }

  # Save user dir as a global variable.
  userDir <<- ""
  outputDir <<- ""
  action_userDir_change <- reactiveVal(0)

  # Number of remote files created by the user.
  # This is used to inform the user of any created files before exiting.
  n_remote_files <- reactiveVal(0)
  
  # 'Unique' session name
  # session$token is the recommended unique identifier
  # timestamps should be avoided in case of multiple users accessing instantaneously.
  sessionName <- session$token

  videoDir <- reactiveVal(file.path(getwd(), "www", "video"))
  observe({
    if (dir.exists(videoDir())) {
      unlink(videoDir(), recursive = TRUE, force = TRUE)
    }
  })

  # Compare data
  list_compare_data <<- list()
  
  # Merging nc-files (filetypes that can be merge)
  nc.merge.filetypes <<- c("AC_SAF", "ECMWF", "ERA5", "H_SAF", "LSA_SAF", "LSASAF_CLIMA", "OSI_SAF")
  
  #### Reactive values ####
  # Reactive value for tar-file-path. (update action if file stays the same on new upload)
  tar_path <- reactiveVal()
  tar_path_action <- reactiveVal(0)
  
  # Reactive value for nc-file-path.
  nc_path <- reactiveVal("")
  nc_path_action <- reactiveVal(0)
  
  # Reactive value for the URL and data description of an NCSS .nc URL
  ncss_url <- reactiveVal(NULL)
  ncss_url_data_desc <- reactiveVal(NULL)
  
  # Reactive value set after untaring files
  untarVals <- reactiveVal()

  # Reactive value for date range
  dateRange_prep <- reactiveVal()
  
  # Reactive value for date range .nc-files selection
  dateRange_prep_nc <- reactiveVal()

  # Reactive values for biggest possible spatial range
  spatialRange <- reactiveValues()
  
  # Similar to spatialRange but for NCSS URL subset.
  spatialRange_ncss <- reactiveValues()

  # Reactive value for timestep
  timestep <- reactiveVal()
  timestep_c <- reactiveVal()

  #Reactive value for maximum level
  max_level <- reactiveVal()

  # Reactive value for dimensions
  dimensions <- reactiveVal()

  # Reactive value for a path to the aux file.
  auxFilePath <- reactiveVal()
  globalAuxFilePath <- reactiveVal()

  # Reactive value for using level or not.
  usingLevel <- reactiveVal(FALSE)

  # Reactive value for output file path (nc file)
  outputFilepath <- reactiveVal()

  # Reactive value for output file nc object
  # An alternative to outputFilepath(), particularly for URL paths
  # to increase efficiency by avoiding repeated calls ncdf4::nc_open() on the URL
  outputFilenc <- reactiveVal(NULL)
  
  # Reactive value for the nc file to be used in analyze. (update action if file stays the same on new upload)
  nc_path_analyze <- reactiveVal()
  nc_path_analyze_action <- reactiveVal(0)

  # Similar to outputFilenc(), reactive value for the nc object to be used in analyze
  nc_object_analyze <- reactiveVal(NULL)
  
  # Reactive value for which variable is used
  var_used <- reactiveVal()

  # Reactive value for current Operator Option
  currentOperatorOption <- reactiveVal()
  currentOperatorValue <- reactiveVal()

  chosen_latPoints <- reactiveVal(c())
  chosen_lonPoints <- reactiveVal(c())

  # Reactive value for chosen operators + a simulated reactive value.
  operatorDataFrame <<- data.frame()
  operatorDataFrameAction <- reactiveVal(0)

  # Repeat message on file loss (remote case only)
  repeatWarning <- reactiveVal(TRUE)

  # Second nc file for operators
  second_infile <- reactiveVal()
  
  # Operator Input
  operatorInput_value <- reactiveVal("")
  
  # global variable for plot type 1d
  plot_type_1d <<- ""
  
  # global variable for plot type 2d
  plot_type_2d <<- ""
  
  # compare data: date range
  start_date_reac <- reactiveVal()
  end_date_reac <- reactiveVal()
  
  # Compare data: save the clicked plots
  checkboxCompareData <- reactiveVal(0)   # Analyze step
  checkboxCompareData_dropdown <- reactiveVal(0)   # Save clicked plot in dropdown in visualize step
  two_files_compare_data_vis <- reactiveVal(0) # Save the number of files to plot (O: one file or 1: two files)

  # Reactive value for the nc file to be used in analyze. (update action if file stays the same on new upload)
  nc_path_visualize <- reactiveVal()
  nc_path_visualize_2 <- reactiveVal("")
  actionVisualize <- reactiveVal(0)
  
  # Similar to outputFilenc(), reactive value for the nc object to be used in visualize
  nc_object_visualize <- reactiveVal(NULL)
  
  # Reactive value for the csv / RData data to be used in visualize (compare data)
  station_data_compare <- reactiveVal(0)
  
  # save cmsaf.stats mode
  cmsaf_stats_enable <- reactiveVal(0)

  # Image path for monitor climate
  image_path_visualize <- reactiveVal()
  actionVisualizeMonitorClimate <- reactiveVal(0)

  # Shape file path
  shapeFile_path <- reactiveVal()
  shapeFile_path_action <- reactiveVal(0)

  # Storing the polygons.
  region_data <- reactiveVal()

  # Instat file path
  instat_path <- reactiveVal()
  instat_path_action <- reactiveVal(0)
  
  # Reactive Value for all visualize variables
  visualizeVariables <- reactiveVal()
  visualizeDataTimestep <- reactiveVal()
  visualizeDataMin <- reactiveVal()
  visualizeDataMax <- reactiveVal()
  reversedDimensions <- reactiveValues(transpose = FALSE, latReverse = FALSE, lonReverse = FALSE)

  # Variable to be visualized chosen in modal
  variable_visualize_modal <- reactiveVal()
  action_visualize_post_modal <- reactiveVal(0)
  
  second_variable_analyze <- reactiveVal(0)
  infile2_analyze_value <- reactiveVal("")
  
  # save file names for plotting in compare data step
  analyze_file1_plot <- reactiveVal("")
  analyze_file2_plot <- reactiveVal("")
  
  # prepare options
  option_prepare_ui <- reactiveVal(0)

  # Reactive values for coordinate bounds in visualize
  lon_bounds <- reactiveVal()
  lat_bounds <- reactiveVal()
  ihsf       <- reactiveVal()

  # Longitute, latitute location vectors (of own locations added in options)
  lon_loc_vec  <- reactiveVal()
  lat_loc_vec  <- reactiveVal()
  name_loc_vec <- reactiveVal()

  # Png file location
  png_path <- reactiveVal()

  # Reactive values for imagewidth and imageheight
  imageheight <- reactiveVal(round(image_def * (2 / 3)))
  imagewidth  <- reactiveVal(image_def)
  lat_lon_trigger <- reactiveVal(0)

  # Reactive value to determine whether this was the first plot
  readyToPlot <- reactiveVal(FALSE)

  # Set these values to one as soon as valid.
  userDirValid <- reactiveVal(0)
  resolutionValid <- reactiveVal(0)

  # isRunningLocally can be found in global.R
  # Find/Create user directory.
  if (isRunningLocally) {
    # Case of local host!

    # Read lines of file.
    for (line in config_lines) {
      if (startsWith(line, "USRWDIR=")) {
        if (nchar(line) > 8) {
          userDir <<- substring(line, 9)
          userDir <<- file.path(userDir, "output")
          outputDir <<- userDir
          # Create the output directory.
          if (!dir.exists(userDir)) {
            cat("WARNING!","\n")
            cat(paste(userDir," does not exist and will be created!", sep = ""),"\n")
            cat("You might check the user directory in ~/CMSAF-Toolbox/config.conf","\n")
            dir.create(userDir)
          }
          setwd(userDir)
        }
      }
    }

    # Set flags and counts for girdInfo and userDir
    gridSupportFile <- file.path(config_directory, "myGrid.txt")
    noGridInfoFlag <<- !file.exists(gridSupportFile)
    noUserDirFlag <<- (nchar(userDir) == 0 || !dir.exists(userDir))

    checkSum <<- 0
    if (noUserDirFlag || noGridInfoFlag) {
      # Check sum = 1 unless both files are missing
      checkSum <<- 1
      if (noUserDirFlag && noGridInfoFlag) {
        checkSum <<- 2

        showModal(modalDialog(
          h4("Please select a directory where created output will be saved."),
          br(),
          actionButton("userDirButton", "Choose a user directory."),
          verbatimTextOutput("selectedUserDirectory"),
          hr(),
          h4("Please specify a grid resolution."),
          h5("The chosen value must be between 0.01 and 89.99.
              Depending on your browser a comma or a point will be accepted as decimal seperator."),
          numericInput("gridResolution", "Resolution", value = 0.05, step = 0.01, min = 0.01, max = 89.99),
          title = "We need your help.",
          size = "l",
          footer = shinyjs::disabled(actionButton("Submit", "Submit"))
        ))
      } else if (noUserDirFlag) {
        showModal(modalDialog(
          h4("Please select a directory where we can save your created output."),
          br(),
          actionButton("userDirButton", "Choose a user directory."),
          verbatimTextOutput("selectedUserDirectory"),
          title = "We need your help.",
          size = "l",
          footer = shinyjs::disabled(actionButton("Submit", "Submit"))
        ))
      } else {
        showModal(modalDialog(
          h4("Please specify a grid resolution."),
          h5("The chosen value must be between 0.01 and 89.99.
              Depending on your browser a comma or a point will be accepted as decimal seperator."),
          numericInput("gridResolution", "Resolution", value = 0.05, step = 0.01, min = 0.01, max = 89.99),
          title = "We need your help.",
          size = "l",
          footer = shinyjs::disabled(actionButton("Submit", "Submit"))
        ))
      }
    }
  } else {
    # Case of remote host!
	dataDir <- file.path("output")
    dataDir <- file.path(dirname(dataDir), "output")

    # Remember the user directory and tear it down in destructor.
    if (!dir.exists("output")) {
      warning("Creating user directory at '", getwd(), "/output'.")
      dir.create("output")
    }
    userDir   <<- file.path("output", sessionName)
    outputDir <<- userDir

    dirFlag <- FALSE
    # If by any chance this directory has existed give warning message.
    # Note: This should now never happen through use of session$token
    # but sensible to keep as a check.
    if (dir.exists(userDir)) {
      dirFlag <- TRUE
      warning(paste0("User directory: ", userDir, " already exists. APP WILL STOP!"))
    } else {
      dir.create(userDir)
    }
  }

  # Observing when user submits initial values for config and/or myGrid.txt file.
  observeEvent(input$Submit, {
    if (noUserDirFlag) {
      # Overwrite config file (config filepath is found in global.R)
      if (file.exists(config_filepath)) {
        file.remove(config_filepath)
      }
      file.create(config_filepath)

      # Save  for next session.
      writeLines(paste0("USRWDIR=", userDir), config_filepath)

      userDir   <<- file.path(userDir, "output")
      outputDir <<- userDir
      action_userDir_change(action_userDir_change() + 1)

      if (!dir.exists(userDir)) {
        dir.create(userDir)
      }
    }

    if (noGridInfoFlag) {
      file.create(gridSupportFile)

      gridXsize <- 360 / input$gridResolution + 1
      gridYsize <- 180 / input$gridResolution + 1

      gridLines <- c("gridtype = lonlat",
                     paste0("xsize = ", gridXsize),
                     paste0("ysize = ", gridYsize),
                     "xfirst = -180",
                     paste0("xinc = ", input$gridResolution),
                     "yfirst = -90",
                     paste0("yinc = ", input$gridResolution))

      writeLines(gridLines, gridSupportFile)
    }

    removeModal()
  })

  # Observe when user gives output directory.
  observeEvent(input$userDirButton, {
    userDirValid(0)
    shinyjs::disable("userDirButton")
    try(userDir <<- choose_directory())
    if (dir.exists(userDir)) {
      output$selectedUserDirectory <- renderText({
        userDir
      })
      userDirValid(1)
      shinyjs::show("selectedUserDirectory")
      setwd(userDir)
	    writeLines(paste0("USRWDIR=", userDir), config_filepath)
    } else {
      shinyjs::hide("selectedUserDirectory")
    }
    shinyjs::enable("userDirButton")
  })

  # Observe when user gives grid resolution.
  observeEvent(input$gridResolution, {
    resolutionValid(0)
    req(input$gridResolution)
    if (input$gridResolution >= 0.01 && input$gridResolution <= 89.99) {
      resolutionValid(1)
    }
  })

  # Allow submitting the initial modal page, as soon as check sum fullfilled, i.e. all required inputs are valid.
  observe({
    req(isRunningLocally)
    if (resolutionValid() + userDirValid() == checkSum) {
      shinyjs::enable("Submit")
    } else {
      shinyjs::disable("Submit")
    }
  })

  observeEvent(input$modify_userDir, {
    showModal(modalDialog(
      h4("Your current user directory is located at:"),
      tags$strong(dirname(userDir)),
      h5("A changed user directory will be visible with the next start of the CM SAF R Toolbox."),
      h5("You can check and change the user directory in ~/CMSAF-Toolbox/config.conf."),
      br(),
      actionButton("userDirButton", "Change user directory."),
      title = "User directory information.",
      size = "l"
    ))
  })

  observe({
    # Trigger every time this changes
    c(action_userDir_change())
    output$prepareString <- renderUI({
      # Can be found in global.R
      if (isRunningLocally) {
        # Local host prepare string dependent on userDir
        tags$div(h2("Prepare"),
                 tags$p("Please select a TAR file", tags$strong("(.tar)"), ", a NetCDF file", tags$strong("(.nc)"), " or a NetCDF URL to start the preparation"),
                 tags$p("of your data. This is the first step after you downloaded your ordered tar-file(s) or NetCDF files or obtained a direct URL."),
                 tags$p("For NetCDF files, you only need to select the first file and the others are selected automatically."),
                 br(),
                 tags$p("This application will help you to extract and merge the data."),
                 tags$p("In addition, you can select a time range and region from your data."),
                 br(),
                 tags$p("If you choose the option to 'just merge' the data files, make sure that grid"),
                 tags$p("and variables are matching. Files are automatically chosen by the pattern of"),
                 tags$p("the first four characters of the file names."),
                 br(),
                 tags$p("Finally, a NetCDF file will be created for you. You can find it in the output directory"),
                 tags$p("located at ", tags$strong(dirname(userDir))),
                 br(),
                 tags$p("The app guides through all steps."))
      } else {
        tags$div(h2("Prepare"),
                 tags$p("Please select a TAR file", tags$strong("(.tar)"), "or a NetCDF file", tags$strong("(.nc)"), " to start the preparation."),
                 tags$p("of your data. This is the first step after you downloaded your ordered tar-file(s)."),
                 tags$p("For NetCDF files, you only need to select the first file and the others are selected automatically."),
                 br(),
                 tags$p("This application will help you to extract and merge the data."),
                 tags$p("In addition, you can select a time range and region from your data."),
                 br(),
                 tags$p("If you choose the option to 'just merge' the data files, make sure that grid"),
                 tags$p("and variables are matching. Files are automatically chosen by the pattern of"),
                 tags$p("the first four characters of the file names."),
                 br(),
                 tags$p("Finally, a NetCDF file will be created for you."),
                 tags$p(tags$strong("Make sure to download your session files before closing this application.")),
                 br(),
                 tags$p("The app guides through all steps."))
      }
    })
  })

  # Going to home screen.
  observeEvent(input$homeimg, {
    shinyjs::hide("panel_content")
    shinyjs::show("panel_home", anim = TRUE, animType = "fade")
  })

  observeEvent(c(input$prepare, input$analyze, input$visualize), {
    shinyjs::hide("panel_home")
    shinyjs::show("panel_content", anim = TRUE, animType = "fade")
  }, ignoreInit = TRUE)

  #### Observing Uploading Events ####
  #### Updating paths ####
  # Modal for wrong file format called with type argument (.tar, .nc, .shp, .RData).
  wrong_file_modal <- function(type) {
    showModal(modalDialog(
      h4(paste("Wrong file format. Please choose",
               type,
               "file to continue.")),
      title = "Warning!",
      size = "m"
    ))
  }

  # Updating the path to the tar file. (local)
  observeEvent(input$tarFileLocal, {
    shinyjs::disable("tarFileLocal")
    res <- try(tar_path(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      #for (path in tar_path()) {
      if (!endsWith(tar_path(), ".tar")) {
        isolate(tar_path(""))
        wrong_file_modal(".tar")
      } else {
        tar_path_action(tar_path_action() + 1)
      }
      #}
    }
    shinyjs::enable("tarFileLocal")
  }, ignoreInit = TRUE)
  
  # prepare choose .nc-files button
  observeEvent(input$ncFileLocal, {
    shinyjs::disable("ncFileLocal")
    res <- try(nc_path(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      if (!endsWith(nc_path(), ".nc")) {
        isolate(nc_path(""))
        wrong_file_modal(".nc")
      } else {
        nc_path_action(nc_path_action() + 1)
      }
    }
    shinyjs::enable("ncFileLocal")
  }, ignoreInit = TRUE)

  # Prepare enter .nc file from URL button. (local and remote)
  observeEvent(input$ncURL, {
    shinyjs::hide(id = "panel_prepareGo")
    shinyjs::show(id = "panel_prepare_nc_url", anim = TRUE, animType = "fade")
  }, ignoreInit = TRUE)
  
  # Prepare check .nc URL button
  observeEvent(input$nc_url_text, {
    shinyjs::toggleState("nc_url_connect", input$nc_url_text != "")
    output$nc_url_valid_message <- renderText({""})
    shinyjs::hide(id = "nc_url_file_info")
    shinyjs::hide(id = "nc_url_download")
    shinyjs::hide(id = "nc_url_subset")
    shinyjs::hide(id = "or_prepare3")
    shinyjs::hide(id = "nc_url_analyze")
    shinyjs::hide(id = "or_prepare4")
    shinyjs::hide(id = "nc_url_visualize")
    shinyjs::hide(id = "nc_url_download_analyze_or_visualise")
  }, ignoreInit = TRUE)
  
  # Set .nc URL validation and connection text
  observeEvent(input$nc_url_connect, {
    shinyjs::disable("panel_prepare_nc_url")
    shinyjs::show("spinner_prepare_nc_url_connect", anim = TRUE, animType = "fade")
    user_url <- input$nc_url_text
    if (endsWith(user_url, "dataset.html") || endsWith(user_url, "dataset.xml")) {
      ncss_url(base_ncss_url(user_url))
      ncss_url_data_desc(ncss_data_description(ncss_url()))
      if (!is.null(ncss_url_data_desc())) {
        shinyjs::show(id = "nc_url_subset")
        output$nc_url_valid_message <- renderText({
          "<span style='color:green'>Valid NetCDF (.nc) URL for subsetting selection.</span>"
        })
      } else {
        output$nc_url_valid_message <- renderText({
          "<span style='color:red'>URL does not appear to lead to a NetCDF (.nc) file or cannot access URL. Please check the URL or your connection and try again.</span>"
        })
        shinyjs::hide(id = "nc_url_subset")
      }
    } else {
      nc <- tryCatch(ncdf4::nc_open(user_url),
                     error = function(e) return(NULL))
      if (!is.null(nc)) valid_url <- TRUE
      else valid_url <- valid_nc_url(user_url)
      output$nc_url_valid_message <- renderText({
        if (valid_url == -1) {
          mes <- "<span style='color:red'>Cannot access URL. Check the URL is correct"
          if (isRunningLocally) {
            mes <- paste(mes, "and your internet connection")
          }
          return(paste0(mes, " and try again.</span>"))
        } else if (valid_url) {
          return("<span style='color:green'>Valid NetCDF (.nc) URL</span>")
        } else {
          return("<span style='color:red'>URL does not appear to lead to a NetCDF (.nc) file. Please check the URL and try again.</span>")
        }
      })
      if (isTRUE(valid_url)) {
        shinyjs::show(id = "nc_url_download")
        if (!is.null(nc)) {
          output$ncurlShortInfo <- renderPrint({
            cmsafops::ncinfo(nc = nc)
          })
          outputFilepath(user_url)
          outputFilenc(nc)
          shinyjs::show(id = "nc_url_file_info")
          shinyjs::show(id = "or_prepare3")
          shinyjs::show(id = "nc_url_analyze")
          shinyjs::show(id = "or_prepare4")
          shinyjs::show(id = "nc_url_visualize")
        } else {
          outputFilenc(NULL)
          shinyjs::hide(id = "nc_url_file_info")
          shinyjs::hide(id = "or_prepare3")
          shinyjs::hide(id = "nc_url_analyze")
          shinyjs::hide(id = "or_prepare4")
          shinyjs::hide(id = "nc_url_visualize")
        }
      } else {
        shinyjs::hide(id = "nc_url_download")
      }
    }
    shinyjs::hide("spinner_prepare_nc_url_connect")
    shinyjs::enable("panel_prepare_nc_url")
  }, ignoreInit = TRUE)
  
  # Download .nc URL
  observeEvent(input$nc_url_download, {
    outputFilepath(NULL)
    outputFilenc(NULL)
    shinyjs::disable("panel_prepare_nc_url")
    shinyjs::show("spinner_prepare_nc_url_download", anim = TRUE, animType = "fade")
    # Try to get the base name from the URL
    b <- try(cmsafops::get_basename(input$nc_url_text))
    # However, some URLs are too long e.g. from THREDDS servers, and don't return a
    # base name. Hence, a default name "url_download" is used instead.
    # This could be replaced by something else derived from the URL
    # e.g. first and last few characters?
    if (class(b) == "try-error") b <- "url_download"
    # The timestamp is appended to the file name to ensure the file is unique.
    url_file <- file.path(userDir, paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"), b))
    # The base name may not finish with ".nc" so this needs to be added.
    if (!endsWith(url_file, ".nc")) url_file <- paste0(url_file, ".nc")
    res <- try(utils::download.file(input$nc_url_text, url_file, method = "auto", mode = "wb"))
    
    shinyjs::hide("spinner_prepare_nc_url_download")
    shinyjs::enable("panel_prepare_nc_url")
    if (class(res) != "try-error" && res == 0) {
      shinyjs::hide(id = "nc_url_download")
      outputFilepath(url_file)
      output$ncurlShortInfo <- renderPrint({
        cmsafops::ncinfo(url_file)
      })
      shinyjs::show(id = "nc_url_file_info")
      shinyjs::show(id = "nc_url_download_analyze_or_visualise")
    } else {
      showModal(modalDialog(
        h4("Something went wrong while downloading the file. Please try again or choose another URL."),
        title = "Error.",
        size = "l"
      ))
    }
  },
  ignoreInit = TRUE
  )
  
  # Set up panel_prepare_ncss_url_subset when nc_url_subset clicked
  observeEvent(input$nc_url_subset, {
    shinyjs::hide("panel_prepare_nc_url")
    shinyjs::show("panel_prepare_ncss_url_subset")
    
    grid_west <- as.numeric(unlist(ncss_url_data_desc()$gridDataset$LatLonBox$west))
    grid_east <- as.numeric(unlist(ncss_url_data_desc()$gridDataset$LatLonBox$east))
    grid_north <- as.numeric(unlist(ncss_url_data_desc()$gridDataset$LatLonBox$north))
    grid_south <- as.numeric(unlist(ncss_url_data_desc()$gridDataset$LatLonBox$south))
    
    # Set spatial range for dynamic plot
    spatialRange_ncss$lat_range <- c(grid_south, grid_north)
    spatialRange_ncss$lon_range  <- c(grid_west, grid_east)
    
    #TODO Determine this from data description
    lon_step <- 0.05
    lat_step <- 0.05
    
    time_min <- as.Date(unlist(ncss_url_data_desc()$gridDataset$TimeSpan$begin))
    time_max <- as.Date(unlist(ncss_url_data_desc()$gridDataset$TimeSpan$end))
    year_min <- as.numeric(format(time_min, "%Y"))
    year_max <- as.numeric(format(time_max, "%Y"))
    
    vars <- c()
    grid_set <- ncss_url_data_desc()$gridDataset$gridSet
    for (i in seq_along(grid_set)) {
      if (names(grid_set)[i] == "grid") {
        vars <- c(vars, attributes(grid_set[[i]])$name)
      }
    }
    vars <- vars[!vars %in% "crs"]
    output$ncss_url_print <- renderText({paste(tags$b(ncss_url()))})
    output$ncss_var_list_ui <- renderUI({
      selectInput(inputId = "ncss_var_list",
                  label = "Select a variable",
                  vars)
    })
    output$ncss_select_region_ui <- renderUI({
      tags$div(id = "ncss_region",
               fluidRow(
                 column(width = 5,
                        numericInput("ncss_lon_min",
                                     label = "Longitude min",
                                     min = grid_west,
                                     max = grid_east,
                                     value = grid_west)
                 ),
                 column(width = 5,
                        numericInput("ncss_lon_max",
                                     label = "Longitude max",
                                     min = grid_west,
                                     max = grid_east,
                                     value = grid_east
                        ))),
               fluidRow(
                 column(width = 5,
                        numericInput("ncss_lat_min",
                                     label = "Latitude min",
                                     min = grid_south,
                                     max = grid_north,
                                     value = grid_south)
                        ),
                 column(width = 5,
                        numericInput("ncss_lat_max",
                                     label = "Latitude max",
                                     min = grid_south,
                                     max = grid_north,
                                     value = grid_north)
                        )
                 
               )
      )
    })
    output$ncss_date_range_ui <- renderUI({
      dateRangeInput(inputId = "ncss_date_range",
                  label = "Please select a date range.",
                  start = time_min,
                  end = time_max,
                  min = time_min,
                  max = time_max)
    })
    output$ncss_year_range_ui <- renderUI({
      fluidRow(
        column(width = 5,
               numericInput("ncss_year_min",
                            label = "Year min",
                            min = year_min,
                            max = year_max,
                            value = year_min)
        ),
        column(width = 5,
               numericInput("ncss_year_max",
                            label = "Year max",
                            min = year_min,
                            max = year_max,
                            value = year_max
               )))
      })
  },
  ignoreInit = TRUE
  )
  
  # Creating a preview plot for the NCSS URL subset.
  output$preview_ncss_subset <- renderPlot({
    req(input$ncss_lon_min)
    req(input$ncss_lon_max)
    req(input$ncss_lat_min)
    req(input$ncss_lat_max)
    
    cmsafvis::render_preview_plot(spatial_lon_range = isolate(spatialRange_ncss$lon_range),
                                  spatial_lat_range = isolate(spatialRange_ncss$lat_range),
                                  lonRange = c(input$ncss_lon_min, input$ncss_lon_max),
                                  latRange = c(input$ncss_lat_min, input$ncss_lat_max))
  })
  
  # Radio buttons for time selection type for subsetting
  observeEvent(input$ncss_time_type, {
    if (input$ncss_time_type == "date_range") {
      shinyjs::show("ncss_date_range_ui")
      shinyjs::hide("ncss_year_range_ui")
      shinyjs::hide("ncss_month_range")
    } else {
      shinyjs::hide("ncss_date_range_ui")
      shinyjs::show("ncss_year_range_ui")
      shinyjs::show("ncss_month_range")
    }
  })
  
  # Downloading a NCSS subset
  observeEvent(input$ncss_subset_download, {
    if (input$ncss_time_type == "date_range") {
      subset_url <- construct_ncss_url(base_url = ncss_url(), var = input$ncss_var_list, 
                                       north = input$ncss_lat_max, west = input$ncss_lon_min, 
                                       east = input$ncss_lon_max, south = input$ncss_lat_min, 
                                       time_start = input$ncss_date_range[1], time_end = input$ncss_date_range[2])
    } else {
      subset_url <- construct_ncss_url_month_extract(base_url = ncss_url(), var = input$ncss_var_list,
                                                     north = input$ncss_lat_max, west = input$ncss_lon_min,
                                                     east = input$ncss_lon_max, south = input$ncss_lat_min,
                                                     month_start = input$ncss_month_from, month_end = input$ncss_month_to, 
                                                     year_start = input$ncss_year_min, year_end = input$ncss_year_max)
    }
    
    # Try to get the base name from the URL
    b <- try(cmsafops::get_basename(ncss_url()))
    if (class(b) == "try-error") b <- "url_download"
    if (endsWith(b, ".nc")) b <- substr(b, 1, nchar(b) - 3)
    b <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"), b)
    shinyjs::disable("panel_prepare_ncss_url_subset")
    shinyjs::show("spinner_prepare_ncss_download", anim = TRUE, animType = "fade")
    url_files <- vector("character", length(subset_url))
    for (i in seq_along(subset_url)) {
      # The timestamp is appended to the file name to ensure the file is unique.
      url_files[i] <- file.path(userDir, paste0(b, "_", i))
      # The base name may not finish with ".nc" so this needs to be added.
      if (!endsWith(url_files[i], ".nc")) url_files[i] <- paste0(url_files[i], ".nc")
      
      res <- try(utils::download.file(subset_url[i], url_files[i], method = "auto", mode = "wb"))
      if (class(res) == "try-error" || res != 0) {
        showModal(modalDialog(
          h4("Something went wrong while downloading the file. Please try again or choose another URL."),
          title = "Error.",
          size = "l"
        ))
        break
      }
    }
    if (length(url_files) > 1) {
      url_file_final <- file.path(userDir, paste0(b, "_", "merge.nc"))
      res <- try({
        cmsafops::box_mergetime(var = input$ncss_var_list, path = userDir, pattern = b,
                                outfile = url_file_final, overwrite = TRUE)
      })
      if (!is.null(res)) {
        showModal(modalDialog(
          h4("Something went wrong merging downloaded files. Please try again or choose another URL."),
          title = "Error.",
          size = "l"
        ))
        url_file_final <- NULL
      }
    } else url_file_final <- url_files[1]
    
    shinyjs::hide("spinner_prepare_ncss_download")
    shinyjs::enable("panel_prepare_ncss_url_subset")
    if ((length(url_files) > 1 && is.null(res)) || (class(res) != "try-error" && res == 0)) {
      shinyjs::hide(id = "nc_url_download")
      outputFilepath(url_file_final)
      output$ncssShortInfo <- renderPrint({
        cmsafops::ncinfo(url_file_final)
      })
      shinyjs::hide(id = "panel_prepare_ncss_url_subset")
      shinyjs::show(id = "panel_prepare_ncss_download_info")
    } else {
      showModal(modalDialog(
        h4("Something went wrong while downloading the file. Please try again or choose another URL."),
        title = "Error.",
        size = "l"
      ))
    }
  },
  ignoreInit = TRUE
  )
  
  # Prepare .nc URL for analyze
  observeEvent(input$nc_url_analyze, {
    nc_path_analyze(outputFilepath())
    nc_object_analyze(outputFilenc())
    if (!endsWith(nc_path_analyze(), ".nc") && is.null(nc_object_analyze())) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      shinyjs::hide("panel_prepare_nc_url")
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  },
  ignoreInit = TRUE
  )
  
  # Prepare .nc URL for visualize
  observeEvent(input$nc_url_visualize, {
    nc_path_visualize(outputFilepath())
    nc_object_visualize(outputFilenc())
    if (!endsWith(nc_path_visualize(), ".nc") && is.null(nc_object_visualize())) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      shinyjs::hide("panel_prepare_nc_url")
      actionVisualize(actionVisualize() + 1)
    }
  },
  ignoreInit = TRUE
  )
  
  # Prepare downloaded .nc URL for analyze
  observeEvent(input$nc_url_download_analyze, {
    nc_path_analyze(outputFilepath())
    isolate(nc_object_analyze(NULL))
    if (!endsWith(nc_path_analyze(), ".nc")) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      shinyjs::hide("panel_prepare_nc_url")
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  },
  ignoreInit = TRUE
  )
  
  # Prepare downloaded .nc URL for visualize
  observeEvent(input$nc_url_download_visualize, {
    nc_path_visualize(outputFilepath())
    nc_object_visualize(NULL)
    if (!endsWith(nc_path_visualize(), ".nc") && is.null(nc_object_visualize())) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      shinyjs::hide("panel_prepare_nc_url")
      actionVisualize(actionVisualize() + 1)
    }
  },
  ignoreInit = TRUE
  )
  
  # Prepare downloaded .nc URL for analyze
  observeEvent(input$ncss_download_analyze, {
    nc_path_analyze(outputFilepath())
    isolate(nc_object_analyze(NULL))
    if (!endsWith(nc_path_analyze(), ".nc")) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      shinyjs::hide("panel_prepare_ncss_download_info")
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  },
  ignoreInit = TRUE
  )
  
  # Prepare downloaded .nc URL for visualize
  observeEvent(input$ncss_download_visualize, {
    nc_path_visualize(outputFilepath())
    nc_object_visualize(NULL)
    if (!endsWith(nc_path_visualize(), ".nc") && is.null(nc_object_visualize())) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      shinyjs::hide("panel_prepare_ncss_download_info")
      actionVisualize(actionVisualize() + 1)
    }
  },
  ignoreInit = TRUE
  )
  
  # Prepare downloaded .nc URL for visualize
  observeEvent(input$ncss_info_back, {
    shinyjs::show(id = "panel_prepare_ncss_url_subset")
    shinyjs::hide(id = "panel_prepare_ncss_download_info")
  },
  ignoreInit = TRUE
  )
  
  # Handling remote tar selection.
  shinyFiles::shinyFileChoose(input, 'tarFileRemote', session = session, roots = remoteVolume, filetypes=c('tar'))
    
  observeEvent(input$tarFileRemote, {
    pth <- shinyFiles::parseFilePaths(remoteVolume,input$tarFileRemote)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    req(endsWith(pth$datapath, ".tar"))
    tar_path( pth$datapath )
    tar_path_action(tar_path_action() + 1)
  })
  
  # Handling remote nc selection.
  shinyFiles::shinyFileChoose(input, 'ncFileRemote', session = session, roots = remoteVolume, filetypes=c('nc'))
  
  observeEvent(input$ncFileRemote, {
    pth <- shinyFiles::parseFilePaths(remoteVolume,input$ncFileRemote)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    req(endsWith(pth$datapath, ".nc"))
    nc_path( pth$datapath )
    nc_path_action(nc_path_action() + 1)
  })

  # Updating the path to the nc file analyze. (local)
  observeEvent(input$ncFileLocal_analyze, {
    shinyjs::disable("ncFileLocal_analyze")
    shinyjs::disable("useOutputFile_analyze")
    res <- try(nc_path_analyze(file.choose(new = TRUE)))
    isolate(nc_object_analyze(NULL))
    if (class(res) != "try-error") {
      if (!endsWith(nc_path_analyze(), ".nc")) {
        isolate(nc_path_analyze(""))
        wrong_file_modal(".nc")
      } else {
        nc_path_analyze_action(nc_path_analyze_action() + 1)
      }
    }
    shinyjs::enable("ncFileLocal_analyze")
    shinyjs::enable("useOutputFile_analyze")
  }, ignoreInit = TRUE)

  # Use shiny files to handle remote uploading from user directory.
  volumes_output = c("Output" = outputDir, remoteVolume)

  shinyFiles::shinyFileChoose(input, 'ncFileRemote_analyze', session = session, roots = volumes_output, filetypes=c('nc'))

  observeEvent(input$ncFileRemote_analyze, {
    pth <- shinyFiles::parseFilePaths(volumes_output,input$ncFileRemote_analyze)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    isolate(nc_object_analyze(NULL))
    if (!endsWith(pth$datapath, ".nc")) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      nc_path_analyze( pth$datapath )
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  })

  # If user chooses to take generated nc file update nc_path_analyze. (output file)
  observeEvent(input$useOutputFile_analyze, {
    nc_path_analyze(outputFilepath())
    isolate(nc_object_analyze(NULL))
    if (!endsWith(nc_path_analyze(), ".nc")) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  }, ignoreInit = TRUE)

  # Updating the path to the nc file visualize. (local)
  observeEvent(input$ncFileLocal_visualize, {
    shinyjs::disable("ncFileLocal_visualize")
    shinyjs::disable("useOutputFile_visualize")
    res <- try(nc_path_visualize(file.choose(new = TRUE)))
    isolate(nc_object_visualize(NULL))
    if (class(res) != "try-error") {
      if (!endsWith(nc_path_visualize(), ".nc")) {
        isolate(nc_path_visualize(""))
        wrong_file_modal(".nc")
      } else {
        actionVisualize(actionVisualize() + 1)
      }
    }
    shinyjs::enable("ncFileLocal_visualize")
    shinyjs::enable("useOutputFile_visualize")
  }, ignoreInit = TRUE)

  # Observing changes in selected nc file visualize. (remote)
  shinyFiles::shinyFileChoose(input, 'ncFileRemote_visualize', session = session, roots = volumes_output, filetypes=c('nc'))

  observeEvent(input$ncFileRemote_visualize, {
    pth <- shinyFiles::parseFilePaths(volumes_output,input$ncFileRemote_visualize)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    isolate(nc_object_visualize(NULL))
    if (!endsWith(pth$datapath, ".nc")) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      nc_path_visualize( pth$datapath )
      actionVisualize(actionVisualize() + 1)
    }
  })

  # If user chooses to take generated nc file update nc_path_visualize. (output file)
  observeEvent(input$useOutputFile_visualize, {
    nc_path_visualize(outputFilepath())
    if (!endsWith(nc_path_visualize(), ".nc")) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      actionVisualize(actionVisualize() + 1)
    }
  }, ignoreInit = TRUE)

  # Updating path to shape file. (local)
  observeEvent(input$shapefileLocal, {
    shinyjs::disable("shapefileLocal")
    res <- try(shapeFile_path(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      if (!endsWith(shapeFile_path(), ".shp")) {
        isolate(shapeFile_path(""))
        wrong_file_modal(".shp")
      } else {
        shapeFile_path_action(shapeFile_path_action() + 1)
      }
    }
    shinyjs::enable("shapefileLocal")
  }, ignoreInit = TRUE)

  # Observing changes in selected shape file. (remote)
  shinyFiles::shinyFileChoose(input, 'shapefileRemote', session = session, roots = remoteVolume, filetypes=c('shp'))

  observeEvent(input$shapefileRemote, {
    pth <- shinyFiles::parseFilePaths(remoteVolume,input$shapefileRemote)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    if (!endsWith(pth$datapath, ".shp")) {
      isolate(shapeFile_path(""))
      wrong_file_modal(".shp")
    } else {
      shapeFile_path( pth$datapath )
      shapeFile_path_action(shapeFile_path_action() + 1)
    }
  })

  # Deleting instat file path when removing plot r-instat check box selection
  observeEvent(input$plot_rinstat, {
    if (!input$plot_rinstat) {
      instat_path("")
    }
  })

  # Updating the path to the instat file. (local)
  observeEvent(input$instat_file_local, {
    shinyjs::disable("instat_file_local")
    res <- try(instat_path(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      if (!endsWith(instat_path(), ".RData") && !endsWith(instat_path(), ".csv")) {
        isolate(instat_path(""))
        wrong_file_modal(".RData")
      } else {
        instat_path_action(instat_path_action() + 1)
      }
    }
    shinyjs::enable("instat_file_local")
  }, ignoreInit = TRUE)

  # Observing changes in selected instat file. (remote)
  shinyFiles::shinyFileChoose(input, 'instat_file_remote', session = session, roots = remoteVolume, filetypes=c('RData', 'csv'))

  observeEvent(input$instat_file_remote, {
    pth <- shinyFiles::parseFilePaths(remoteVolume,input$instat_file_remote)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    if (!endsWith(pth$datapath, ".RData") && !endsWith(pth$datapath, ".csv")) {
      isolate(instat_path(""))
      wrong_file_modal(".RData")
    } else {
      instat_path( pth$datapath )
      instat_path_action(instat_path_action() + 1)
    }
  })

  resetToPreparePanel <- function() {
    # Clear mem
    rm(list = ls())
    gc()

    # First toggle states of main output panel.
    shinyjs::hide("panel_analyzeGo")
    shinyjs::hide("panel_visualizeGo")
    shinyjs::hide("panel_prepareInput1")
    shinyjs::hide("panel_prepare_nc_url")
    shinyjs::hide("panel_prepare_ncss_url_subset")
    shinyjs::hide("panel_prepare_ncss_download_info")
    shinyjs::hide("panel_prepareInput1Nc")
    shinyjs::hide("panel_prepareInput2")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::reset("panel_prepare_nc_url")
    shinyjs::reset("panel_prepare_ncss_url_subset")
    shinyjs::reset("panel_prepare_ncss_download_info")
    shinyjs::reset("panel_prepareInput1Nc")
    shinyjs::reset("panel_prepareInput2")
    shinyjs::hide("spinner_prepare1")
    shinyjs::hide("spinner_prepare2")
    shinyjs::hide("spinner_prepare3")
    shinyjs::hide("spinner_prepare4")
    shinyjs::hide("spinner_analyze")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("panel_analyze")
    shinyjs::reset("panel_analyze")

    untarVals(list())

    # Resetting data frame
    operatorDataFrame <<- data.frame()
    operatorDataFrameAction(operatorDataFrameAction() + 1)
    shinyjs::show("panel_prepareGo", anim = TRUE, animType = "fade")
  }

  resetToAnalyzePanel <- function() {
    # Clear mem
    rm(list = ls())
    gc()

    # First toggle states of main output panel.
    shinyjs::hide("panel_prepareGo")
    shinyjs::hide("panel_visualizeGo")
    shinyjs::hide("panel_prepareInput1")
    shinyjs::hide("panel_prepare_nc_url")
    shinyjs::hide("panel_prepare_ncss_url_subset")
    shinyjs::hide("panel_prepare_ncss_download_info")
    shinyjs::hide("panel_prepareInput1Nc")
    shinyjs::hide("panel_prepareInput2")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::reset("panel_prepare_nc_url")
    shinyjs::reset("panel_prepare_ncss_url_subset")
    shinyjs::reset("panel_prepare_ncss_download_info")
    shinyjs::reset("panel_prepareInput1Nc")
    shinyjs::reset("panel_prepareInput2")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("panel_analyze")
    shinyjs::reset("panel_analyze")
    shinyjs::hide("ncFile_analyze_second_file")
    shinyjs::reset("ncFile_analyze_second_file")
    
    # Resetting path of second file (compare data)
    nc_path_visualize_2("")
    infile2_analyze_value("")
    switchButtonCompare <<- FALSE   # switch button operator dropdown (compare data)
    
    # Resetting data frame
    operatorDataFrame <<- data.frame()
    operatorDataFrameAction(operatorDataFrameAction() + 1)
    shinyjs::show("panel_analyzeGo", anim = TRUE, animType = "fade")
  }

  resetToVisualizePanel <- function() {
    # Clear mem
    rm(list = ls())
    gc()

    # First toggle states of main output panel.
    shinyjs::hide("panel_prepareGo")
    shinyjs::hide("panel_analyzeGo")
    shinyjs::hide("panel_prepareInput1")
    shinyjs::hide("panel_prepare_nc_url")
    shinyjs::hide("panel_prepare_ncss_url_subset")
    shinyjs::hide("panel_prepare_ncss_download_info")
    shinyjs::hide("panel_prepareInput1Nc")
    shinyjs::hide("panel_prepareInput2")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::reset("panel_prepare_nc_url")
    shinyjs::reset("panel_prepare_ncss_url_subset")
    shinyjs::reset("panel_prepare_ncss_download_info")
    shinyjs::reset("panel_prepareInput1Nc")
    shinyjs::reset("panel_prepareInput2")
    shinyjs::hide("panel_analyze")
    shinyjs::reset("panel_analyze")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("spinner_visualize_compare_data")
    shinyjs::hide("date_range_compare_data")
    shinyjs::hide("ncFile_analyze_second_file")
    shinyjs::hide("visualizePage")
    shinyjs::reset("visualizePage")
    shinyjs::reset("backToSetup")
    shinyjs::reset("myImage_monitorClimate")
    shinyjs::reset("monitorClimate_PNG")
    shinyjs::reset("monitorClimate_MP4")
    shinyjs::hide("dropdown_station_number")
    shinyjs::hide("title_text2")
    shinyjs::hide("subtitle_text2")
    
    readyToPlot(FALSE)
    
    applyOperatorActiv <<- FALSE   # because something went wrong, when apply compare data
    
    # delete temp folder (compare data)
    compare_temp_dir <- file.path(tempdir(), "compare_temp")
    # remove if it exists
    if (dir.exists(compare_temp_dir)) {
      unlink(compare_temp_dir, recursive = TRUE)
    }
    
    # Resetting path of second file (compare data)
    nc_path_visualize_2("")
    switchButtonCompare <<- FALSE   # switch button operator dropdown (compare data)
    shinyjs::hide("operatorInput_dropdown")
    shinyjs::reset("operatorInput_dropdown")
    
    cmsaf_stats_enable(0)
    
    # Resetting reactive variables
    operatorInput_value("")
    two_files_compare_data_vis(0)
    checkboxCompareData_dropdown(0)
    checkboxCompareData(0)
    
    analyze_file1_plot("")
    analyze_file2_plot("")
    
    # reset global variables
    plot_type_1d <<- ""
    plot_type_2d <<- ""

    # Resetting data frame
    operatorDataFrame <<- data.frame()
    operatorDataFrameAction(operatorDataFrameAction() + 1)
    shinyjs::show("panel_visualizeGo")
    shinyjs::show("setupPage", anim = TRUE, animType = "fade")

  }

  # THIS HAD FLAG napp() == 1.
  observeEvent(input$prepare, {
    resetToPreparePanel()
  })

  # THIS HAD FLAG napp() == 2.
  observeEvent(input$analyze, {
    resetToAnalyzePanel()
  })

  # THIS HAD FLAG napp() == 3.
  observeEvent(input$visualize, {
    resetToVisualizePanel()
  })

  #### Handling the scripts ####
  #### PREPARATION ####
  observeEvent({
    tar_path()
    tar_path_action()
  }, {
    # Requirements
    req(tar_path())
    req(tar_path_action())

    # If wrong format alert and stop.
    #for(path in tar_path()) {
    if (!endsWith(tar_path(), ".tar")) {
      isolate(tar_path(""))
      showModal(modalDialog(
        h4("Wrong file format. Please choose .tar file to continue."),
        title = "Warning!",
        size = "m"
      ))
    } else {
      # Show Spinner
      shinyjs::hide(id = "panel_prepareGo")
      shinyjs::show("spinner_prepare1", anim = TRUE, animType = "fade")

      # Extracting date range and lat/lon range from functions
      extractedDates <- extractDateRange(tar_path(), tar_flag = 1)
      timestep(extractedDates$timestep)

      # Rendering date range.
      output$dateRange_ui <- renderUI({
        dateRangeInput(inputId = "dateRange_prepare",
                       label = "Please select a date range to prepare.",
                       start = extractedDates$date_from,
                       end = extractedDates$date_to,
                       min = extractedDates$date_from,
                       max = extractedDates$date_to,
                       startview = "year"
        )
      })
      shinyjs::hide("spinner_prepare1")
      shinyjs::show(id = "panel_prepareInput1", anim = TRUE, animType = "fade")
    }
  })
  
  # Handling .nc-files prepare
  observeEvent({
    nc_path()
    nc_path_action()
  }, {
    # Requirements
    req(nc_path())
    req(nc_path_action())
    
    # If wrong format alert and stop.
    if (!endsWith(nc_path(), ".nc")) {
      isolate(nc_path(""))
      showModal(modalDialog(
        h4("Wrong file format. Please choose .nc file to continue."),
        title = "Warning!",
        size = "m"
      ))
    } else {
      # Show Spinner
      shinyjs::hide(id = "panel_prepareGo")
      shinyjs::show("spinner_prepare1", anim = TRUE, animType = "fade")
      
      # check filetype 
      # c("GOME", "_gri", "z_ca", "era", "h141", "HDF5", "NETCDF4", "ice")
      # filetype.list <- list(ac_saf)
      # check.boolean <- check.ncfiletype()
      # 
      # if(check.boolean){
      #   showModal(modalDialog(
      #     h4("Wrong file format. Please choose .nc file to continue."),
      #     title = "Warning!",
      #     size = "m"
      #   ))
      # }
      
      filname <- cmsafops::get_basename(nc_path())
      filetype <- substr(filname, 1, 4)
      pattern <- substr(filname, 1, 6)
      if(filetype == "GOME"){
        cmsafops::acsaf_box_mergetime(dirname(nc_path()), pattern,file.path(tempdir(),"ac_saf_mergetime.nc"))
        nc_path(file.path(tempdir(),"ac_saf_mergetime.nc"))
      }
      
      # choose variable 
      userOptions <- getUserOptions(nc_path(), claas_flag = 0)
      output$variable_ui_nc <- renderUI({
        vars <- subset(userOptions$variables, !(userOptions$variables %in% c("lat", "lon", "latitude", "longitude","time_bnds", "nb2", "time", "crs")))
        selectInput("variableInput360",
                    "Please choose a variable.",
                    choices = vars)
      })
      
      # Extracting date range and lat/lon range from functions
      extractedDates <- extractDateRangeNc(nc_path(), nc_flag = 1)
      timestep(extractedDates$timestep)

      # Rendering date range.
      output$dateRange_ui_nc <- renderUI({
        dateRangeInput(inputId = "dateRange_prepare_nc",
                       label = "Please select a date range to prepare.",
                       start = extractedDates$date_from,
                       end = extractedDates$date_to,
                       min = extractedDates$date_from,
                       max = extractedDates$date_to,
                       startview = "year"
        )
      })
      shinyjs::hide("spinner_prepare1")
      shinyjs::show(id = "panel_prepareInput1Nc", anim = TRUE, animType = "fade")
    }
  })
  
  # Observing time aggregation checkbox
  observeEvent(input$checkboxInput_aggregate, {
    if(input$checkboxInput_aggregate == TRUE){
        shinyjs::show(id = "operatorGroupsPrepare")
    } else {
      shinyjs::hide(id = "operatorGroupsPrepare")
    }
  })

  observeEvent(input$dateRange_prepare, {
    req(input$dateRange_prepare[1])
    req(input$dateRange_prepare[2])
    shinyjs::enable("untarAndUnzip")
  })
  
  # Observing dateRange_prepare_nc
  observeEvent(input$dateRange_prepare_nc, {
    req(input$dateRange_prepare_nc[1])
    req(input$dateRange_prepare_nc[2])
    shinyjs::enable("applyDateRange")
  })
  
  observeEvent(input$justmerge, {
    req(input$justmerge)
    shinyjs::enable("applyDateRange")
  })

  # Observing upload of aux file.
  observeEvent(input$aux_upload, {
    res <- try(globalAuxFilePath(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      shiny::removeModal()
    } else {
      showModal(modalDialog(
        h4("Something went wrong while uploading the auxiliary file. Please try again or choose another .tar file."),
        title = "Error.",
        size = "l"
      ))
    }
  })

  # Observing
  observeEvent(input$aux_download, {
    auxfile <- file.path(userDir, "claas2_level2_aux_data.nc")
    res <- tryCatch(utils::download.file("https://public.cmsaf.dwd.de/data/perm/auxilliary_data/claas2_level2_aux_data.nc", auxfile, "auto", mode = "wb"))

    # Download file returns 0 on success.
    if (class(res) != "try-error" && res == 0) {
      globalAuxFilePath(file.path(auxfile))
      shiny::removeModal()
    } else {
      showModal(modalDialog(
        h4("Something went wrong while downloading the auxiliary file. Please try again or choose another .tar file."),
        title = "Error.",
        size = "l"
      ))
    }
  })

  # Observing canceling of aux file choice. Return to prepare
  observeEvent(input$aux_cancel, {
    shiny::removeModal()
    shinyjs::hide("spinner_prepare2")
    resetToPreparePanel()
  })

  # get list of NetCDF files
  getNcList <- function(path_to_nc, nc_flag)
  {
    ordname <- cmsafops::get_basename(path_to_nc)
    ordpath <- dirname(path_to_nc)
    
    if (identical(path_to_nc, character(0))) {
      stop(paste0("No infile!", var))
    }
    
    pattern <- substr(ordname, 1, 4)
    fileslist <- list.files(path = ordpath, pattern = pattern, full.names = TRUE)
    #fileslist <- list.files(ordpath, full.names = TRUE)
    pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
    
    result.fileslist <- NULL
    for(i in 1:length(fileslist)){
      regex <- regmatches(fileslist[i], regexpr(pattern, fileslist[i]))
      if(!is.null(regex))
        result.fileslist <- append(result.fileslist, regex)
      regex <- NULL
    }
    result.fileslist <- sort(result.fileslist)
    return(result.fileslist)
  }
  
  #### Function to calculate date range of .nc-files and give variable names. ####
  ncFilelist_dateRange <- function(path_to_nc, startDate, endDate, timestep, nc_flag = 1) {
    # Get important values.
    ncfilelist_all <- getNcList(path_to_nc = path_to_nc, nc_flag = nc_flag)
    # WARNING: DATES ARE IN SAME FORMAT (INDEPENDENT OF TIMESTEP ==/!= 'm')
    # IN FACT: WE ARE OMITTING TIMESTEP VALUE CAUSE WE DON'T APPEAR TO NEED IT.
    dates <- extractAllDatesNc(path_to_nc, nc_flag = nc_flag)
    dates <- dates$allDates
    
    # So filelist just equals tarlist here?
    filelist <- NULL
    
    dums <- NA
    dume <- NA
    
    if (startDate %in% dates) {
      dums <- which(dates == startDate)
    } else {
      if (startDate < dates[1] & endDate >= dates[1])
        (dums <- 1)
    }
    
    if (endDate %in% dates) {
      dume <- which(dates == endDate)
    } else {
      if (endDate > dates[length(dates)])
        (dume <- length(dates))
    }
    
    dums <- dums[1]
    dume <- dume[length(dume)]
    
    if (!is.na(dums)) {
      start <- dums
      end <- dume
      
      if (length(start) > 1) {
        start <- start[1]
        dums <- start
      }
      if (length(end) > 1) {
        end <- end[length(end)]
        dume <- end
      }
      
      if (dums > dume) {
        start <- dume
        end <- dums
      }
      tarlist <- ncfilelist_all
      tarlist <- sort(tarlist)
      tarlist <- tarlist[which(tarlist %in% ncfilelist_all[start:end])]
      
      filelist <- append(filelist, tarlist)
    }
    
    infile <- filelist[1]
    ordpath_nc <- dirname(path_to_nc)
    
    nc_temp_dir <- file.path(tempdir(), "nc_temp")
    # remove if it exists
    if (dir.exists(nc_temp_dir)) {
      unlink(nc_temp_dir, recursive = TRUE)
    }
    
    # create new nc_temp
    if (!dir.exists(nc_temp_dir)) {
      dir.create(nc_temp_dir)
    }
    
    ordpath <- ordpath_nc
    cmsafops::check.coordinate.system(paste0(ordpath,"/",filelist[1]), nc_temp_dir, input$variableInput360, filelist)
    
    # remove temp-file of ac_saf_mergetime
    if(file.exists(file.path(tempdir(),"ac_saf_mergetime.nc"))){
      file.remove(file.path(tempdir(),"ac_saf_mergetime.nc"))
    }
    
    pattern <- substr(filelist[1], 1, 5)
    
    flistdim <- length(filelist)
    
    if(flistdim > 1)
    {
      cmsafops::box_mergetime(input$variableInput360,
                              path = nc_temp_dir,
                              pattern = pattern,
                              outfile = paste0(nc_temp_dir,"/","merge_",filelist[1]),
                              lon1 = -180,
                              lon2 = 180,
                              lat1 = -90,
                              lat2 = 90,
                              nc34 = 4,
                              overwrite = TRUE)
      if(startDate != endDate){
        cmsafops::selperiod(input$variableInput360,
                            start = startDate,
                            end = endDate,
                            infile = paste0(nc_temp_dir,"/","merge_",filelist[1]),
                            outfile = paste0(nc_temp_dir,"/","final_",filelist[1]),
                            overwrite = TRUE)
        file.remove(paste0(nc_temp_dir,"/","merge_",filelist[1]))
      }else{
        file.rename(paste0(nc_temp_dir,"/","merge_",filelist[1]), paste0(nc_temp_dir,"/","final_",filelist[1]))
      }
    }
    else{
      cmsafops::selperiod(input$variableInput360,
                          start = startDate,
                          end = endDate,
                          infile = paste0(nc_temp_dir,"/", filelist[1]),
                          outfile = paste0(nc_temp_dir,"/","final_",filelist[1]),
                          overwrite = TRUE)
    }
    
    nc_path(paste0(nc_temp_dir,"/",paste0("final_",filelist[1])))
    
    infile <- paste0("final_",filelist[1])
    
    # remove temp-files
    for(i in 1:length(filelist))
    {
      file.remove(paste0(nc_temp_dir,"/",filelist[i]))
    }
    filelist <- c(paste0("final_",filelist[1]))
    
    return(
      list(infile =  file.path(nc_temp_dir, infile),
           claas_flag = 0,
           filelist = filelist)
    )
  }
  
  #### Function to untar and unzip files, and give variable names. ####
  untarFiles <- function(path_to_tar, startDate, endDate, timestep, tar_flag = 1) {
    # Get important values.

    tarlist_all <- getTarList(path_to_tar, tar_flag = tar_flag, includeClaasAux = TRUE)
  
    # WARNING: DATES ARE IN SAME FORMAT (INDEPENDENT OF TIMESTEP ==/!= 'm')
    # IN FACT: WE ARE OMITTING TIMESTEP VALUE CAUSE WE DON'T APPEAR TO NEED IT.
    dates <- extractAllDates(path_to_tar, tar_flag = tar_flag)
    dates <- dates$allDates

    # Get claas_flag and strop from tarlist if needed.
    claas_flag <- getClaasAuxFlag(tarlist_all)
    if (claas_flag) {
      tarlist_all <- tarlist_all[tarlist_all != "CM_SAF_CLAAS2_L2_AUX.nc"]
    }

    if (tar_flag == 0) {
      tar_chunk_size <- 100
    }else{
      tar_chunk_size <- 1000
    }

    # So filelist just equals tarlist here?
    filelist <- NULL

    ordname <- cmsafops::get_basename(path_to_tar)
    ordpath <- dirname(path_to_tar)

    flist <- list.files(ordpath, substr(ordname, 1, 8), full.names = TRUE)


    for (file in flist) {
      if (tar_flag == 1) {
        tarlist <- utils::untar(file, list = TRUE, tar = "internal")
      } else {
        tarlist <- utils::untar(file, list = TRUE)
      }
      
      if ("CM_SAF_CLAAS2_L2_AUX.nc" %in% tarlist) {
        tarlist <- subset(tarlist, !(tarlist %in% c("CM_SAF_CLAAS2_L2_AUX.nc")))
        claas_flag <- 1
      }
      tarlist <- sort(tarlist)
      
      dums <- NA
      dume <- NA

      if (startDate %in% dates) {
        dums <- which(dates == startDate)
      } else {
        if (startDate < dates[1] & endDate >= dates[1])
          (dums <- 1)
      }

      if (endDate %in% dates) {
        dume <- which(dates == endDate)
      } else {
        if (endDate > dates[length(dates)])
          (dume <- length(dates))
      }

      dums <- dums[1]
      dume <- dume[length(dume)]

      if (!is.na(dums)) {
        start <- dums
        end <- dume

        if (length(start) > 1) {
          start <- start[1]
          dums <- start
        }
        if (length(end) > 1) {
          end <- end[length(end)]
          dume <- end
        }

        if (dums > dume) {
          start <- dume
          end <- dums
        }

        tarlist <- tarlist[which(tarlist %in% tarlist_all[start:end])]
        if (length(tarlist) == 0) next

        filelist <- append(filelist, tarlist)

        if (length(tarlist) > tar_chunk_size) {
          dum <- seq(0, length(tarlist), tar_chunk_size)

          for (j in 1:(length(dum) - 1)) {
            tarlist_alt <- tarlist[(dum[j] + 1):dum[j + 1]]
            if (tar_flag == 1) {
              utils::untar(
                file,
                files = tarlist_alt,
                exdir = ordpath,
                tar = "internal"
              )
            } else {
              utils::untar(
                file,
                files = tarlist_alt,
                exdir = ordpath
              )
            }
          }
          tarlist_alt <- tarlist[(dum[length(dum)] + 1):length(tarlist)]
          if (tar_flag == 1) {
            utils::untar(
              file,
              files = tarlist_alt,
              exdir = ordpath,
              tar = "internal"
            )
          } else {
            utils::untar(
              file,
              files = tarlist_alt,
              exdir = ordpath
            )
          }
        } else {

          if (tar_flag == 1) {
            utils::untar(
              file,
              files = tarlist,
              exdir = ordpath,
              tar = "internal"
            )
          } else {
            utils::untar(
              file,
              files = tarlist,
              exdir = ordpath
            )
          }
        }
      }
    } # end if !is.na(dums)

    # Untar auxiliary data if included
    if (claas_flag == TRUE) {
      if (tar_flag == 1) {
        utils::untar(
          path_to_tar,
          files = "CM_SAF_CLAAS2_L2_AUX.nc",
          exdir = ordpath,
          tar = "internal"
        )
      } else {
        utils::untar(path_to_tar,
                     files = "CM_SAF_CLAAS2_L2_AUX.nc",
                     exdir = ordpath)
      }
    }

    # Unzip data in case of NetCDF3
    gzcheck <- length(unlist(strsplit(tarlist[1], ".nc")))

    if (gzcheck == 2) {
      for (i in seq_along(filelist)) {
        zipfile <- file.path(ordpath, filelist[i])
        R.utils::gunzip(zipfile, overwrite = TRUE)
      }

      # Get information of first file

      infile <- unlist(strsplit(filelist[1], ".gz"))
    } else {
      infile <- filelist[1]
    }
    return(
      list(infile =  file.path(ordpath, infile),
           claas_flag = claas_flag,
           filelist = filelist)
    )
  }

  # Create modal for

  getUserOptions <- function(infile, claas_flag) {
    id <- ncdf4::nc_open(infile)
    vn <- names(id$var)
    dn <- names(id$dim)
    lon_var <- "lon"
    lat_var <- "lat"

    '%ni%' <- Negate('%in%')

    if ("lon" %ni% dn) {
      if ("longitude" %in% dn) {
        lon_var <- "longitude"
      }
    }

    if ("lat" %ni% dn) {
      if ("latitude" %in% dn) {
        lat_var <- "latitude"
      }
    }

    if (lon_var %in% c(dn, vn)) {
      lon_range <- range(ncdf4::ncvar_get(id, lon_var), na.rm = TRUE)
    }
    if (lat_var %in% c(dn, vn)) {
      lat_range <- range(ncdf4::ncvar_get(id, lat_var), na.rm = TRUE)
    }
    ncdf4::nc_close(id)

    var_default <-
      subset(vn, !(
        vn %in% c(
          "lat",
          "lon",
          "time_bnds",
          "nb2",
          "time",
          "SATID",
          "latitude",
          "longitude",
          "crs"
        )
      ))

    # Get max level.
    max_level <- NA
    for (variable in vn) {
      if (startsWith(variable, "HLW") || startsWith(variable, "HSH")) {
        id <- ncdf4::nc_open(infile)
        max_level <- length(ncdf4::ncvar_get(id, "lev"))
        ncdf4::nc_close(id)
      }
    }

    # Stop if data are in sinusoidal projection
    if ("sinusoidal" %in% vn) {
      showModal(modalDialog(
        h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
            Please use the 'change projection' option during the order process.
            Your NetCDF data have to be on a regular lon-lat grid."),
        title = "Sorry!",
        size = "l"
      ))

      req(FALSE)
      resetToPreparePanel()
    }

    return(list(
      variables = vn,
      dimensions = dn,
      lat_range = lat_range,
      lon_range = lon_range,
      max_level = max_level
    ))
  }

  # When untar button is pressed.
  observeEvent(input$untarAndUnzip, {
    # Require positve click value.
    req(input$untarAndUnzip > 0)

    # Get first of month if timestep == m
    if (timestep() == "m") {
      dateRange_prep(firstOfMonth(input$dateRange_prepare))
    } else {
      dateRange_prep(input$dateRange_prepare)
    }

    # Show spinner
    shinyjs::hide("panel_prepareInput1")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::show("spinner_prepare2", anim = TRUE, animType = "fade")

    untarVals(untarFiles(tar_path(), dateRange_prep()[1], dateRange_prep()[2], timestep(), tar_flag = 1))
  }, ignoreInit = TRUE)
  
  # # When apply date range (nc prepare) button is pressed.
  # observeEvent(input$applyDateRange, {
    # # Require positve click value.
    # req(input$applyDateRange > 0)

    # # Get first of month if timestep == m
    # if (timestep() == "m") {
      # dateRange_prep(firstOfMonth(input$dateRange_prepare_nc))
    # } else {
      # dateRange_prep(input$dateRange_prepare_nc)
    # }
    
    # # Show spinner
    # shinyjs::hide("panel_prepareInput1Nc")
    # shinyjs::reset("panel_prepareInput1Nc")
    # shinyjs::show("spinner_prepare4", anim = TRUE, animType = "fade")
    
    # # From this point on nc and tar files are processed immediately
    # untarVals(ncFilelist_dateRange(nc_path(), dateRange_prep()[1], dateRange_prep()[2], timestep(), nc_flag = 1))
  # }, ignoreInit = TRUE)
  
  
  # Function to create the output
    creatingOutputfile2 <- function(path_to_tar, var) {

    file1   <- cmsafops::get_basename(path_to_tar)
    ordDir  <- dirname(path_to_tar)
	  outfile <- file.path(outputDir, paste0(var, "_mergetime.nc"))

    # box_mergetime
      pattern <- substr(file1, 1, 4)

      cmsafops::box_mergetime(var,
                           path = ordDir,
                           pattern = pattern,
                           outfile = outfile,
                           overwrite = TRUE)
      return(outfile)
  }
  
  
  # When apply date range (nc prepare) button is pressed.
  observeEvent(input$applyDateRange, {
    # Require positve click value.
    req(input$applyDateRange > 0)

	# If just merge checkbox = TRUE
	if (input$justmerge) {
		# Show / hide spinner
		shinyjs::hide("panel_prepareInput1Nc")
		shinyjs::show("spinner_prepare3", anim = TRUE, animType = "fade")

		res <- try(outputFilepath(creatingOutputfile2(nc_path(),
                                                   input$variableInput360)))
    
		if (class(res) == "try-error") {
			showModal(modalDialog(
				h4("An error occured while creating an output file."),
				tags$p(paste0("Message: ", res)),
				title = "Error!",
				size = "m"
			))
		}

		# Variable 'isRunningLocally' can be found in global.R
		if (repeatWarning() && !isRunningLocally) {
			showModal(modalDialog(
				tags$p("A .nc file has been created for you. ",
					"Its contents are temporarily stored in the session folder.",
					"Please downlaod the session folder if you want to save it.",
					"All files will be removed after closing this app!"),
				checkboxInput("noRepeat", "Do not show this message again."),
			title = "Warning to prevent data loss!",
			size = "l"
		))
		}
	
		shinyjs::hide("spinner_prepare3")
		resetToAnalyzePanel()
	} else {
	
		# Get first of month if timestep == m
		if (timestep() == "m") {
			dateRange_prep(firstOfMonth(input$dateRange_prepare_nc))
		} else {
			dateRange_prep(input$dateRange_prepare_nc)
		}
    
		# Show spinner
		shinyjs::hide("panel_prepareInput1Nc")
		shinyjs::reset("panel_prepareInput1Nc")
		shinyjs::show("spinner_prepare4", anim = TRUE, animType = "fade")
    
		# From this point on nc and tar files are processed immediately
		untarVals(ncFilelist_dateRange(nc_path(), dateRange_prep()[1], dateRange_prep()[2], timestep(), nc_flag = 1))
	
	  }
    }, ignoreInit = TRUE)
  

  # Set all input values for next stage.
  observe({
    # First do aux check
    req(untarVals()$infile)
    req(!is.null(untarVals()$claas_flag))

    shinyjs::hide("panel_prepareInput1Nc")
    shinyjs::reset("panel_prepareInput1Nc")
    
    ordPath <- dirname(untarVals()$infile)

    infile <- untarVals()$infile
    claas_flag <- untarVals()$claas_flag
    id <- ncdf4::nc_open(infile)
    file_info <- cmsafops:::check_dims(id)
    ncdf4::nc_close(id)

    if (!file_info$has_lon_lat) {
      if (claas_flag) {
        auxFilePath(file.path(ordPath, "CM_SAF_CLAAS2_L2_AUX.nc"))
        cmsafops::add_grid_info(infile, auxFilePath(), outfile = NULL, overwrite = TRUE)
      } else {
        grid_info <- get_grid(infile)
        if (grid_info == 5 && (is.null(globalAuxFilePath()) || !file.exists(globalAuxFilePath()))) {
          showModal(modalDialog(
            h4("Your data seems to require an auxiliar file. You can either upload a local auxiliar file or download it from the public CM SAF website."),
            br(),
            fluidRow(column(6, actionButton(inputId = "aux_upload",
                                            label = "Upload aux file.",
                                            class = "btn btn-success btn-lg btn-block")),
                     column(6, actionButton(inputId = "aux_download",
                                            label = "Download aux file.",
                                            class = "btn btn-success btn-lg btn-block"))),
            title = "Auxiliary data needed.",
            footer = actionButton(inputId = "aux_cancel",
                                  label = "Cancel"),
            size = "l"
          ))

          # Leave silently and come back when auxFilePath is updated.
          req(FALSE)
          cmsafops::add_grid_info(infile, globalAuxFilePath(), outfile = NULL, overwrite = TRUE)
        } else if (grid_info == 2 || grid_info == 7) {
          showModal(modalDialog(
            h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
               Please use the 'change projection' option during the order process.
               Your NetCDF data have to be on a regular lon-lat grid."),
            title = "Error!",
            size = "l"))

          resetToPreparePanel()
          req(FALSE)
        }
      }

    }

    userOptions <- getUserOptions(infile, claas_flag)

    # Set max_level
    if (is.numeric(userOptions$max_level)) {
      max_level(userOptions$max_level)
      shinyjs::show("level_ui")
    } else {
      max_level(0)
      shinyjs::hide("level_ui")
    }

    # Remembering the largest possible range.
    spatialRange$lat_range <- userOptions$lat_range
    spatialRange$lon_range  <- userOptions$lon_range

    # Remembering dimensions
    dimensions(userOptions$dimensions)

    # Distinction between .tar-mode and .nc-mode
    if(nc_path() != "") {
      output$variable_ui <- renderUI({
        vars <- subset(userOptions$variables, !(userOptions$variables %in% c("lat", "lon", "latitude", "longitude", "time_bnds", "nb2", "time", "crs")))
        selectInput("variableInput",
                    "You have selected the following variable",
                    choices = vars)
      })
    }
    else{
      output$variable_ui <- renderUI({
        vars <- subset(userOptions$variables, !(userOptions$variables %in% c("lat", "lon", "latitude", "longitude", "time_bnds", "nb2", "time", "crs")))
        selectInput("variableInput",
                    "We found the following variables",
                    choices = vars)
      })
    }
    
    # Rendering lon range.
    output$lonRange_ui <- renderUI({
      sliderInput(inputId = "lonRange",
                  label = "Please select a longitude range.",
                  min = trunc(20 * userOptions$lon_range[1]) / 20,
                  max = trunc(20 * userOptions$lon_range[2]) / 20,
                  value = c(trunc(20 * userOptions$lon_range[1]) / 20, trunc(20 * userOptions$lon_range[2]) / 20),
                  step = 0.05
      )
    })

    # Rendering lat range.
    output$latRange_ui <- renderUI({
      sliderInput(inputId = "latRange",
                  label = "Please select a latitude range.",
                  min = trunc(20 * userOptions$lat_range[1]) / 20,
                  max = trunc(20 * userOptions$lat_range[2]) / 20,
                  value = c(trunc(20 * userOptions$lat_range[1]) / 20, trunc(20 * userOptions$lat_range[2]) / 20),
                  step = 0.05
      )
    })

    # Reset auxFilePath and untarVals
    untarVals()
    auxFilePath()

    # Show second part of prepare and hide spinner
    shinyjs::hide("spinner_prepare2")
    shinyjs::hide("spinner_prepare4")
    shinyjs::show(id = "panel_prepareInput2", anim = TRUE, animType = "fade")
    if(nc_path() != ""){
      shinyjs::hide("deleteExtracted")
    }
    else{
      shinyjs::show("deleteExtracted")
    }
  })

  # Creating a preview plot.
  output$previewSpatialCoveragePlot <- renderPlot({
    req(input$lonRange)
    req(input$latRange)

    cmsafvis::render_preview_plot(spatial_lon_range = isolate(spatialRange$lon_range),
                                  spatial_lat_range = isolate(spatialRange$lat_range),
                                  lonRange = input$lonRange,
                                  latRange = input$latRange)
  })

  observe({
    req(input$variableInput)
    req(max_level())

    if (startsWith(input$variableInput, "HLW") | startsWith(input$variableInput, "HSH")) {
      output$level_ui <- renderUI({
        numericInput("level",
                     paste0("Select level (1 - ", max_level(), ")"),
                     min = 1,
                     max = max_level(),
                     value = 1,
                     step = 1)
      })
      shinyjs::show("level_ui")
      usingLevel(TRUE)
    } else {
      shinyjs::hide("level")
      usingLevel(FALSE)
    }
  })

  observeEvent(input$level, {
    shinyjs::disable("createOutput")
    req(is.numeric(input$level))
    if (input$level < 1 || input$level > max_level()) {
      shinyjs::disable("createOutput")
    } else {
      shinyjs::enable("createOutput")
    }
  })

  # Function to create the output
  creatingOutputfile <- function(path_to_tar, startDate, endDate, lon_var, lat_var, var, level, dn, outputFormat, claas_flag, deleteExtracted) {

    filelist <- untarVals()$filelist
    ordDir <- dirname(path_to_tar)

    # Regrid AOD or CLAAS data
    if (claas_flag) {
      cat("Data will be regridded to rectangular grid...", "\n")
      # Regrid first file
      infile  <- filelist[1]
      infile  <- file.path(ordDir, infile)
      auxfile <- file.path(ordDir, "CM_SAF_CLAAS2_L2_AUX.nc")
      dxy <- 0.05     # Target grid resolution

      id <- ncdf4::nc_open(infile)
      rdata <- ncdf4::ncvar_get(id, var)
      ncdf4::nc_close(id)

      id <- ncdf4::nc_open(auxfile)
      lon_reg <- ncdf4::ncvar_get(id, lon_var)
      lat_reg <- ncdf4::ncvar_get(id, lat_var)
      ncdf4::nc_close(id)

      lon_org <- seq(-90.0, 90.0, dxy)
      lat_org <- lon_org

      lon <-
        lon_org[which(lon_org > (min(lon_reg, na.rm = TRUE) - dxy) &
                        lon_org < (max(lon_reg, na.rm = TRUE) + dxy))]
      lat <-
        lat_org[which(lat_org > (min(lat_reg, na.rm = TRUE) - dxy) &
                        lat_org < (max(lat_reg, na.rm = TRUE) + dxy))]

      # FNN solution
      lon_reg   <- as.vector(lon_reg)
      lat_reg   <- as.vector(lat_reg)
      lon_reg2   <- lon_reg[!is.na(lon_reg)]
      lat_reg2   <- lat_reg[!is.na(lon_reg)]

      fnn_a <- FNN::get.knnx(lon, lon_reg2, k = 1)
      fnn_b <- FNN::get.knnx(lat, lat_reg2, k = 1)

      filelist_claas <- NULL

      for (i in seq_along(filelist)) {
        infile  <- filelist[i]
        infile  <- file.path(ordDir, infile)

        id <- ncdf4::nc_open(infile)
        rdata <- as.vector(ncdf4::ncvar_get(id, var))
        ncdf4::nc_close(id)

        rdata2 <- rdata[!is.na(lon_reg)]
        arr_new <- array(NA, c(length(lon), length(lat)))
        arr_new[cbind(fnn_a$nn.index, fnn_b$nn.index)] <- rdata2

        # Put new data into file

        t_name <- "time"
        t_standard_name <- "time"
        t_calendar <- "standard"

        lat_name <- "latitude"
        lat_standard_name <- "latitude"
        lat_long_name <- "latitude"
        lat_units <- "degrees_north"
        lat_axis <- "Y"

        lon_name <- "longitude"
        lon_standard_name <- "longitude"
        lon_long_name <- "longitude"
        lon_units <- "degrees_east"
        lon_axis <- "X"

        info <- "Created with the CM SAF R Toolbox."
        var_prec <- "float"

        # Originally called nc34 and set in header as nc34 <- 3
        # After that no change... I'm now passing this as a shiny select option.
        # outputFormat = "NetCDF3"

        if (outputFormat == "NetCDF4") {
          nc_format <- as.logical(1)
          compression <- 4
        } else {
          nc_format <- as.logical(0)
          compression <- NA
        }

        v_missing_value <- -999

        target <- array(NA, c(length(lon), length(lat), 1))
        target[, , 1] <- arr_new
        target[is.na(target)] <- v_missing_value

        id <- ncdf4::nc_open(infile)
        v_units         <- ncdf4::ncatt_get(id, var, "units")$value
        v_standard_name <- ncdf4::ncatt_get(id, var, "standard_name")$value
        v_long_name     <- ncdf4::ncatt_get(id, var, "long_name")$value
        time1           <- ncdf4::ncvar_get(id, "time")
        t_units         <- ncdf4::ncatt_get(id, "time", "units")$value
        ncdf4::nc_close(id)

        outfile <- file.path(userDir, paste0("RG_", filelist[i]))

        x <- ncdf4::ncdim_def(name = "lon",
                              units = lon_units,
                              vals = lon)
        y <- ncdf4::ncdim_def(name = "lat",
                              units = lat_units,
                              vals = lat)
        t <-
          ncdf4::ncdim_def(
            name = "time",
            units = t_units,
            vals = time1,
            unlim = TRUE
          )

        var1 <-
          ncdf4::ncvar_def(
            name = var,
            units = v_units,
            dim = list(x, y, t),
            missval = v_missing_value,
            prec = var_prec,
            compression = compression
          )

        vars <- list(var1)

        ncnew <- ncdf4::nc_create(outfile, vars, force_v4 = nc_format)

        ncdf4::ncvar_put(ncnew, var1, target)

        ncdf4::ncatt_put(ncnew, var, "standard_name", v_standard_name, prec = "text")
        ncdf4::ncatt_put(ncnew, var, "long_name", v_long_name, prec = "text")

        ncdf4::ncatt_put(ncnew, "time", "standard_name", t_standard_name, prec =
                           "text")
        ncdf4::ncatt_put(ncnew, "time", "calendar", t_calendar, prec = "text")

        ncdf4::ncatt_put(ncnew, "lon", "standard_name", lon_standard_name, prec =
                           "text")
        ncdf4::ncatt_put(ncnew, "lon", "long_name", lon_long_name, prec = "text")
        ncdf4::ncatt_put(ncnew, "lon", "axis", lon_axis, prec = "text")

        ncdf4::ncatt_put(ncnew, "lat", "standard_name", lat_standard_name, prec =
                           "text")
        ncdf4::ncatt_put(ncnew, "lat", "long_name", lat_long_name, prec = "text")
        ncdf4::ncatt_put(ncnew, "lat", "axis", lat_axis, prec = "text")

        ncdf4::ncatt_put(ncnew, 0, "Info", info, prec = "text")

        ncdf4::nc_close(ncnew)

        filelist_claas <-
          append(filelist_claas, paste0("RG_", filelist[i]))
      } # end for filelist

      # Remove original files
      file.remove(file.path(ordDir, filelist))
      file.remove(auxfile)
      filelist <- filelist_claas
    } # end if claas flag

    # if time aggregation is enabled
    if(input$checkboxInput_aggregate == TRUE) {
      outfile <- file.path(outputDir, paste0(var, "_", startDate, "-", endDate, "_tmp.nc"))
    } 
    else {
      outfile <- file.path(outputDir, paste0(var, "_", startDate, "-", endDate, ".nc"))
    }
    
    if (!is.null(level)) {
      pattern <- substr(filelist[1], 1, 5)
      
      cmsafops::levbox_mergetime(var,
                              level = level,
                              path = ordDir,
                              pattern = pattern,
                              outfile = outfile,
                              lon1 = lon_var[1],
                              lon2 = lon_var[2],
                              lat1 = lat_var[1],
                              lat2 = lat_var[2],
                              nc34 = outputFormat,
                              overwrite = TRUE)
    } else {
      # box_mergetime
      pattern <- substr(filelist[1], 1, 5)

      cmsafops::box_mergetime(var,
                           path = ordDir,
                           pattern = pattern,
                           outfile = outfile,
                           lon1 = lon_var[1],
                           lon2 = lon_var[2],
                           lat1 = lat_var[1],
                           lat2 = lat_var[2],
                           nc34 = outputFormat,
                           overwrite = TRUE)
    }

    # Clean up
    if (deleteExtracted) {
      gzcheck <- length(unlist(strsplit(filelist[1], ".nc")))
      if (gzcheck == 2) {
        removelist <- unlist(strsplit(filelist, ".gz"))
      } else {
        removelist <- filelist
      }

      file.remove(file.path(ordDir, removelist))
    }

    # Clean up variables
    var_list_default <- c("checkstring", "userDir", "date_from", "date_to", "dates",
                          "dates_all", "delete", "dume", "dummy", "dums", "end", "endDate",
                          "filelist", "flist", "gzcheck", "i", "id", "infile", "infile_name",
                          "n", "orddir", "ordname", "ordpath",
                          "outputDir", "pattern", "slash", "split_path", "start",
                          "startDate", "tar_flag", "tarlist", "tarlist_all", "timestep", "var",
                          "var_default", "vn", "zipfile", "t", "claas_flag", "sn", "outputFormat",
                          "lon_var", "lat_var", "%ni%")
    var_list <- ls()

    var_list <- var_list[var_list %in% var_list_default]
    var_list <- append(var_list, c("var_list_default", "var_list"))

    rm(list = var_list)
    return(outfile)
  }
  
  observeEvent(input$operatorGroupsPrepare, {
    for (option in operatorOptions) {
      if (is.element(input$operatorGroupsPrepare, operatorOptionsDict[[option]])) {
        currentOperatorOption(option)
        break
      } else {
        currentOperatorOption("None")
      }
    }
    
    if(currentOperatorOption() != "None") {
      option_string <- toString(currentOperatorOption())
      option_prepare_ui(paste0(option_string, "_prepare"))
    }
   
    if(currentOperatorOption() == "None"){
      if(option_prepare_ui() != 0)
        shinyjs::hide(option_prepare_ui())
    } else {
      shinyjs::show(option_prepare_ui())
    }
  })

  observeEvent(input$createOutput, {
    # Show spinner
    shinyjs::hide("panel_prepareInput2")
    shinyjs::show("spinner_prepare3", anim = TRUE, animType = "fade")

    # Require positive click value
    req(input$createOutput > 0)
    req(dateRange_prep())

    # Get level or set to NULL.
    if (usingLevel()) {
      level <- input$level
    } else {
      level <- NULL
    }

    # Finally create the output file and remember it's location.
    # Differentiation between .nc-version and .tar-version
    if(nc_path() != ""){
      res <- try(outputFilepath(creatingOutputfile(nc_path(),
                                                   dateRange_prep()[1],
                                                   dateRange_prep()[2],
                                                   input$lonRange,
                                                   input$latRange,
                                                   input$variableInput,
                                                   level,
                                                   dimensions(),
                                                   input$outputFormat,
                                                   FALSE,
                                                   input$deleteExtracted)))
    }
    else{
      res <- try(outputFilepath(creatingOutputfile(tar_path(),
                                                   dateRange_prep()[1],
                                                   dateRange_prep()[2],
                                                   input$lonRange,
                                                   input$latRange,
                                                   input$variableInput,
                                                   level,
                                                   dimensions(),
                                                   input$outputFormat,
                                                   FALSE,
                                                   input$deleteExtracted)))
    }
    
    # time aggregation in prepare step
    if(input$checkboxInput_aggregate == TRUE) {
      agg_func <- get(input$operatorGroupsPrepare, asNamespace("cmsafops"))
      outfile_path <- file.path(outputDir, paste0(input$variableInput, "_", dateRange_prep()[1], "-", dateRange_prep()[2], ".nc"))
      
      for (option in operatorOptions) {
        if (is.element(input$operatorGroupsPrepare, operatorOptionsDict[[option]])) {
          currentOperatorOption(option)
          break
        } else {
          currentOperatorOption("None")
        }
      }
      
      if(currentOperatorOption() == "None") {
        argumentList <- list(var = input$variableInput,
                             infile = outputFilepath(),
                             outfile = outfile_path,
                             nc34 = input$outputFormat,
                             overwrite = TRUE)
      }
      else if (currentOperatorOption() == "timeRange") {
        argumentList <- list(var = input$variableInput,
                             nts = input$timeRange_nts_prepare,
                             infile = outputFilepath(),
                             outfile = outfile_path,
                             nc34 = input$outputFormat,
                             overwrite = TRUE)
      }
      res <- try(do.call(agg_func, argumentList))
      
      currentOperatorOption("None")   # reset option
      
      outputFilepath(file.path(outputDir, paste0(input$variableInput, "_", dateRange_prep()[1], "-", dateRange_prep()[2], ".nc")))
      unlink(file.path(outputDir, paste0(input$variableInput, "_", dateRange_prep()[1], "-", dateRange_prep()[2], "_tmp.nc")))
    }

    if (class(res) == "try-error") {
      showModal(modalDialog(
        h4("An error occured while creating an output file."),
        tags$p(paste0("Message: ", res)),
        title = "Error!",
        size = "m"
      ))
    }

    # Variable 'isRunningLocally' can be found in global.R
    if (repeatWarning() && !isRunningLocally) {
      showModal(modalDialog(
        tags$p("A .nc file has been created for you. ",
               "Its contents are temporarily stored in the session folder.",
               "Please downlaod the session folder if you want to save it.",
               "All files will be removed after closing this app!"),
        checkboxInput("noRepeat", "Do not show this message again."),
        title = "Warning to prevent data loss!",
        size = "l"
      ))
    }

    shinyjs::hide("spinner_prepare3")
    resetToAnalyzePanel()
  }, ignoreInit = TRUE)

  observeEvent(input$noRepeat, {
    repeatWarning(!input$noRepeat)
  })

  # Download the created session directory.
  # session_dir_download_handler is called in two places
  # For output$download (main button) and output$downloader_modal (button in modal)
  session_dir_download_handler <- downloadHandler(
    filename = function() {
      paste0(sessionName, ".tar")
    },
    content = function(con) {
      if (dir.exists(userDir)) {
        tarname <- paste0(sessionName, ".tar")
        utils::tar(tarname, userDir)
        file.copy(tarname, con)
      }
    },
    contentType = "application/x-tar"
  )
  output$download <- session_dir_download_handler

  #### ANALYZING ####
  # If a file has been generated let user decide if they want to continue with
  # this file or select another file.
  observeEvent(outputFilepath(), {
    if (endsWith(outputFilepath(), ".nc") || !is.null(outputFilenc())) {
      output$ncFile_analyze <- renderUI({
        tags$pre("We prepared the following .nc file for you: ",
                 cmsafops::get_basename(outputFilepath(), nc = outputFilenc()))
      })
      output$ncFile_visualize <- renderUI({
        tags$pre("We prepared the following .nc file for you: ",
                 cmsafops::get_basename(outputFilepath(), nc = outputFilenc()))
      })
      shinyjs::show("ncFile_analyze")
      shinyjs::show("useOutputFile_analyze")
      shinyjs::show("or_analyze")
      shinyjs::show("ncFile_visualize")
      shinyjs::show("useOutputFile_visualize")
      shinyjs::show("or_visualize")
    } else {
      shinyjs::hide("ncFile_analyze")
      shinyjs::hide("useOutputFile_analyze")
      shinyjs::hide("or_analyze")
      shinyjs::hide("ncFile_visualize")
      shinyjs::hide("useOutputFile_visualize")
      shinyjs::hide("or_visualize")
    }
  })

  # Starting analyze page.
  observeEvent(nc_path_analyze_action(), {
    # Requirements
    req(nc_path_analyze())

    # If wrong format alert and stop.
    if (!endsWith(nc_path_analyze(), ".nc") && is.null(nc_object_analyze())) {
      isolate(nc_path_analyze(""))
      showModal(modalDialog(
        h4("Wrong file format. Please choose .nc file to continue."),
        title = "Warning!",
        size = "m"
      ))
    } else {
      # Getting variable(s)
      if (!is.null(nc_object_analyze())) id <- nc_object_analyze()
      else id <- ncdf4::nc_open(nc_path_analyze())
      vn <- names(id$var)
      dn <- names(id$dim)
      if (is.null(nc_object_analyze())) ncdf4::nc_close(id)

      if (!("time" %in% dn)) {
        showModal(modalDialog(
          h4("Sorry, the file you chose does not contain a time dimension."),
          title = "Error!",
          size = "l"))

        resetToPreparePanel()
      } else {

      var_default <- subset(vn, !(vn %in% c("lat", "lon", "latitude", "longitude", "time_bnds", "nb2", "time", "crs")))

      # Stop if data are in sinusoidal projection
      if ("sinusoidal" %in% vn) {
        showModal(modalDialog(
          h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
           Please use the 'change projection' option during the order process.
           Your NetCDF data have to be on a regular lon-lat grid."),
          title = "Error!",
          size = "l"))

        resetToPreparePanel()
      } else {

      output$usedVariable <- renderUI({
        selectInput("usedVariable",
                    label = "Please choose a variable",
                    choices = var_default,
                    width = "320px")
      })
      shinyjs::hide("panel_analyzeGo")
      shinyjs::show("panel_analyze")
      }}
    }
  }, ignoreInit = TRUE)

  # Update Variable
  observeEvent(input$variableAnalyze, {
    req(input$variableAnalyze)

    var_used(input$variableAnalyze)
  }, ignoreInit = TRUE)
  
  # Update possible choices for operator.
  observe({
    req(input$operatorGroup)
    req(!applyOperatorActiv)
    
    if(input$operatorGroup == "Compare Data"){
      if((endsWith(infile2_analyze_value(), ".csv")) || endsWith(infile2_analyze_value(), ".RData")) {
        tmp_vec <- c()
        tmp_vec[["Compare Data"]] <- c(
          "Difference plot (absolute)" = "cmsaf.diff.absolute",
          "Difference plot (relative)" = "cmsaf.diff.relative",
          "Scatterplot" = "cmsaf.scatter",
          "Bar chart" = "cmsaf.hist",
          "Side-by-Side plot" = "cmsaf.side.by.side",
          "Comparison of time series" = "cmsaf.time.series",
          "Show statistics" = "cmsaf.stats"
        )

        shinyjs::show("operator")
        output$operator <- renderUI({
          # Operator can be found in global.R
          checkboxGroupInput("operatorInput",
                             label = "Select an operator.",
                             choices = tmp_vec[[input$operatorGroup]],
                             selected = tmp_vec[[input$operatorGroup]][1],
                             width = "320px")
        })
      } else {
        shinyjs::show("operator")
        output$operator <- renderUI({
          # Operator can be found in global.R
          checkboxGroupInput("operatorInput",
                             label = "Select an operator.",
                             choices = operators[[input$operatorGroup]],
                             selected = operators[[input$operatorGroup]][1],
                             width = "320px")
        })
      }
    } else{
      shinyjs::hide("ncFile_analyze_second_file")
      output$operator <- renderUI({
        # Operator can be found in global.R
        selectInput("operatorInput",
                    label = "Select an operator.",
                    choices = operators[[input$operatorGroup]],
                    width = "320px")
      })
    }
  })
  
  observeEvent(input$operatorInput, {
    req(input$operatorGroup)
    req(input$operatorInput)
    
    if(input$operatorGroup == "Compare Data") {
      checkboxCompareData(input$operatorInput)
      checkboxCompareData_dropdown(checkboxCompareData()[1])
    } 
    operatorInput_value(input$operatorInput[1])
  })
  
  observe({
    req(input$operatorGroup)
    req(operatorInput_value())
    
    climate_analysis_ops <- c(
      "absolute_map",
      "anomaly_map",
      "climatology_map",
      "fieldmean_plot",
      "fieldmean_and_anomaly_map",
      "warming_stripes_plot",
      "time_series_plot",
      "trend_plot"
    )

    countries_choosable <- codes[, "iso3c"]
    names(countries_choosable) <- codes[, "country.name.en"]

    if (operatorInput_value() %in% climate_analysis_ops) {
      # If the monitor climate name is chosen, we will visualize right away!
      shinyjs::show("ClimateAnalysisMessage")
      # Update checkboxes to correct value
      updateCheckboxInput(
        session = session,
        inputId = "applyAnother",
        label = "Do you want to apply another operator afterwards?",
        value = FALSE)
      updateCheckboxInput(
        session = session,
        inputId = "instantlyVisualize",
        label = "Do you want to visualize the results right away?",
        value = TRUE)

      # Render countries including EUR, AFR, TOT
      output$select_country <- renderUI({
        selectInput("country",
                  label = "Please select a country",
                  choices = countries_choosable,
                  selected = countries_choosable[which(countries_choosable == "S_A")],
                  width = "320px",
                  multiple = FALSE)
      })
    }

    # Render some UI elements dependent on infile and chosen operator
    if (operatorInput_value() == "selyear") {
      if (!is.null(nc_object_analyze())) nc <- nc_object_analyze()
      else nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      time_info <- cmsafops:::get_date_time(ncdf4::ncvar_get(nc, "time"), ncdf4::ncatt_get(nc, "time", "units")$value)
      years2 <- time_info$years
      if (is.null(nc_object_analyze())) ncdf4::nc_close(nc)

      output$years_to_select <- renderUI({
        selectInput("years",
                    label = "Select years",
                    choices = sort(unique(years2)),
                    width = "320px",
                    multiple = TRUE)
      })
    }

    if (operatorInput_value() %in% c("selperiod", "extract.period", climate_analysis_ops)) {
      if (!is.null(nc_object_analyze())) nc <- nc_object_analyze()
      else nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(nc, "time", "units")$value, ncdf4::ncvar_get(nc, "time")))
      if (is.null(nc_object_analyze())) ncdf4::nc_close(nc)

      if (operatorInput_value() %in% c("selperiod", "extract.period")) {
        output$dateRange_to_select <- renderUI({
          dateRangeInput("dateRange_analyze",
                         label = "Select date range",
                         start = min(date_time),
                         end = max(date_time),
                         min = min(date_time),
                         max = max(date_time),
                         width = "320px")
        })
      } else if(operatorInput_value() %in% c("absolute_map", "anomaly_map", "climatology_map", "fieldmean_plot", "fieldmean_and_anomaly_map")){
        # In monitor climate want to initialize in same year.
        output$dateRange_to_select <- renderUI({
          dateRangeInput("dateRange_analyze",
                         label = "Select date range",
                         start = min(date_time[format(date_time, "%Y") == format(max(date_time), "%Y")]),
                         end = max(date_time),
                         min = min(date_time),
                         max = max(date_time),
                         width = "320px")
        })
      }
    }

    if (operatorInput_value() %in% c("anomaly_map", "climatology_map", "fieldmean_plot", "fieldmean_and_anomaly_map", "warming_stripes_plot", "time_series_plot", "trend_plot")) 
    {
      output$climatology_years <- renderUI({

        years <- unique(format(date_time, format = "%Y"))
        if (length(years) == 1) {
          end_year <- years[1]
        } else {
          end_year <- years[length(years) - 1]
        }

        tags$div(
          fluidRow(
            column(width = 5,
                   selectInput("climate_year_start",
                               label = "Climatol. start year",
                               choices = years,
                               selected = years[1])
                               ),
            column(width = 5,
                   selectInput("climate_year_end",
                               label = "Climatol. end year",
                               choices = years,
                               selected = end_year
                               ))
          )
        )
      })
    }

    if (operatorInput_value() == "seltime") {
      if (!is.null(nc_object_analyze())) nc <- nc_object_analyze()
      else nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      time_info <- cmsafops:::get_date_time(ncdf4::ncvar_get(nc, "time"), ncdf4::ncatt_get(nc, "time", "units")$value)
      times2 <- time_info$times
      if (is.null(nc_object_analyze())) ncdf4::nc_close(nc)

      output$times_to_select <- renderUI({
        selectInput("times",
                    label = "Select times",
                    choices = sort(unique(times2)),
                    width = "320px",
                    multiple = TRUE)
      })
    }

    if (operatorInput_value() %in% c("sellonlatbox", climate_analysis_ops)) {
      # nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      # lon <- ncdf4::ncvar_get(nc, "lon")
      # lat <- ncdf4::ncvar_get(nc, "lat")
      # ncdf4::nc_close(nc)
      
      file_data <- cmsafops::read_file(isolate(nc_path_analyze()), input$usedVariable, nc = nc_object_analyze())
      lon <- file_data$dimension_data$x
      lat <- file_data$dimension_data$y
      
      output$region_to_select <- renderUI({
        # tags$div(id = "region",
        #          sliderInput("lonRegionSlider",
        #                      label = "Select longitude",
        #                      min = ceiling(min(lon, na.rm = T)*100)/100,
        #                      max = floor(max(lon, na.rm = T)*100)/100,
        #                      value = c(ceiling(min(lon, na.rm = T)*100)/100,
        #                                floor(max(lon, na.rm = T)*100)/100),
        #                      width = "320px"),
        #          sliderInput("latRegionSlider",
        #                      label = "Select latitude",
        #                      min = ceiling(min(lat, na.rm = T)*100)/100,
        #                      max = floor(max(lat, na.rm = T)*100)/100,
        #                      value = c(ceiling(min(lat, na.rm = T)*100)/100,
        #                                floor(max(lat, na.rm = T)*100)/100),
        #                      width = "320px"))
        tags$div(id = "region",
          fluidRow(
            column(width = 5,
                   numericInput("lonRegionMin",
                               label = "Longitude min",
                               min = ceiling(min(lon, na.rm = T)*100)/100,
                               max = floor(max(lon, na.rm = T)*100)/100,
                               value = ceiling(min(lon, na.rm = T)*100)/100)
                               ),
            column(width = 5,
                   numericInput("lonRegionMax",
                                label = "Longitude max",
                                min = ceiling(min(lon, na.rm = T)*100)/100,
                                max = floor(max(lon, na.rm = T)*100)/100,
                                value = floor(max(lon, na.rm = T)*100)/100
                   ))),
            fluidRow(
              column(width = 5,
                     numericInput("latRegionMin",
                                  label = "Latitude min",
                                  min = ceiling(min(lat, na.rm = T)*100)/100,
                                  max = floor(max(lat, na.rm = T)*100)/100,
                                  value = ceiling(min(lat, na.rm = T)*100)/100)
              ),
              column(width = 5,
                     numericInput("latRegionMax",
                                  label = "Latitude max",
                                  min = ceiling(min(lat, na.rm = T)*100)/100,
                                  max = floor(max(lat, na.rm = T)*100)/100,
                                  value = floor(max(lat, na.rm = T)*100)/100
                     ))
            
          )
        )
      })
    }

    if (operatorInput_value() == "selpoint.multi") {
      # nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      # lon <- ncdf4::ncvar_get(nc, "lon")
      # lat <- ncdf4::ncvar_get(nc, "lat")
      # ncdf4::nc_close(nc)
      
      file_data <- cmsafops::read_file(isolate(nc_path_analyze()), input$usedVariable, nc = nc_object_analyze())
      lon <- file_data$dimension_data$x
      lat <- file_data$dimension_data$y

      updateCheckboxInput(session, "instantlyVisualize", value = FALSE)

      updateSelectInput(session,
                        "format",
                        "Select output format",
                        choices = c("NetCDF4" = 4, "NetCDF3" = 3, "CSV" = 5))

      output$multi_warning <- renderUI({
        tags$div(
          br(),
          p("This function creates multiple output files.
                                    These will be saved in your output directory.
                                    Instant visualization is not possible!", style = "color:red"),
          br())
        })

    } else {
      updateSelectInput(session,
                        "format",
                        "Select output format",
                        choices = c("NetCDF4" = 4, "NetCDF3" = 3))
    }

    if(operatorInput_value() == "cmsaf.adjust.two.files") {
      updateCheckboxInput(session, "instantlyVisualize", value = FALSE)
      
      output$multi_warning_adjust_two_files <- renderUI({
        tags$div(
          br(),
          p("This function creates multiple output files.
                                    These will be saved in your output directory.
                                    Instant visualization is not possible!", style = "color:red"),
          br())
      })
    }
    
    # Disable the correct checkboxes
    disable_apply_another_ops <- climate_analysis_ops
    disable_instant_vis_ops <- c(climate_analysis_ops, "selpoint.multi", "cmsaf.adjust.two.files")

    if (operatorInput_value() %in% disable_apply_another_ops) {
      shinyjs::disable("applyAnother")
      shinyjs::hide("applyAnother")
    } else {
      shinyjs::enable("applyAnother")
    }

    if (operatorInput_value() %in% disable_instant_vis_ops) {
      shinyjs::disable("instantlyVisualize")
      shinyjs::hide("instantlyVisualize")
    } else {
      shinyjs::enable("instantlyVisualize")
    }
  })

  observeEvent(input$add_point, {
    chosen_lonPoints(c(chosen_lonPoints(), input$lonPoint))
    chosen_latPoints(c(chosen_latPoints(), input$latPoint))
    updateNumericInput(session, "lonPoint", label = "Select longitude point", value = 0)
    updateNumericInput(session, "latPoint", label = "Select latitude point", value = 0)
    output$chosen_points <- renderTable({
      df <- data.frame(cbind(chosen_lonPoints(), chosen_latPoints()))
      names(df) <- c("lon", "lat")
      df
    })
  })

  ## Remove file choosing
  volumes_output <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "ncSecondFileRemote", roots = volumes_output, session = session)

  output$secondFile <- renderPrint({
    if (is.integer(input$ncSecondFileRemote)) {
      cat("No file has been selected.")
    } else {
      file <- shinyFiles::parseFilePaths(volumes_output, input$ncSecondFileRemote)
      second_infile( file$datapath )
      file$datapath
    }
  })

  # Observing checkboxes
  observeEvent(input$applyAnother, {
    if (!input$applyAnother) {
      shinyjs::show("instantlyVisualize")
    } else {
      shinyjs::hide("instantlyVisualize")
    }
  }, ignoreInit = TRUE)

  # Toggle states of input ui element options for the different operators.
  observeEvent(operatorInput_value(), {
    req(operatorInput_value())

    # Operator options can be found in global.R
    currentOperatorOption("None")

    for (option in operatorOptions) {
      if (is.element(operatorInput_value(), operatorOptionsDict[[option]])) {
        currentOperatorOption(option)
      } else {
        shinyjs::hide(option)
        shinyjs::hide("dateRange_to_select")
        shinyjs::hide("climatology_years")
        shinyjs::hide("accumulateInfile")
        shinyjs::hide("attachToExisting")
        shinyjs::hide("years_to_select")
        shinyjs::hide("region_to_select")
        shinyjs::hide("times_to_select")
        shinyjs::hide("points_to_select")
        shinyjs::hide("select_country")
        shinyjs::hide("plot_format")
        shinyjs::hide("monitorClimateAnalyzeMethod")
        shinyjs::hide("analyzeTimeSize")
		    shinyjs::hide("stripecol")
		    shinyjs::hide("circular")
		    shinyjs::hide("absrel")
      }
    }

    if (currentOperatorOption() != "None") {
      if (currentOperatorOption() == "dateRange") {
        shinyjs::show("dateRange_to_select")
      } else if (currentOperatorOption() == "years") {
        shinyjs::show("years_to_select")
      } else if (currentOperatorOption() == "times") {
        shinyjs::show("times_to_select")
      } else if (currentOperatorOption() == "region") {
        shinyjs::show("region_to_select")
      } else if (currentOperatorOption() == "point.multi") {
        shinyjs::show("points_to_select")
      } else if (currentOperatorOption() == "monitor_climate") {
        shiny::hideTab(inputId = "mainVisualizeTabset", target = "Metrics")
        shinyjs::show("accumulateInfile")
        shinyjs::show("attachToExisting")
        shinyjs::show("dateRange_to_select")
        if (operatorInput_value() %in% c("warming_stripes_plot", "time_series_plot", "trend_plot")) {
          shinyjs::hide("dateRange_to_select")
        }
        if (!(operatorInput_value() %in% c("absolute_map"))) {
          shinyjs::show("climatology_years")
        }
		
		    if ((operatorInput_value() %in% c("anomaly_map"))) {
          shinyjs::show("stripecol")
		      shinyjs::show("absrel")
        }

        shinyjs::show("region_to_select")
        shinyjs::show("plot_format")
        shinyjs::show("select_country")
        if(operatorInput_value() %in% c("time_series_plot", "trend_plot")){
          shinyjs::show("monitorClimateAnalyzeMethod")
          shinyjs::show("analyzeTimeSize")
          shinyjs::hide("plot_format")
          #shinyjs::hide("region_to_select")
          shinyjs::hide("accumulateInfile")
          #shinyjs::hide("attachToExisting")
        } 
        if (operatorInput_value() %in% c("warming_stripes_plot")) {
          shinyjs::show("monitorClimateAnalyzeMethod")
          shinyjs::show("analyzeTimeSize")
          shinyjs::show("stripecol")
          shinyjs::show("circular")
          shinyjs::hide("plot_format")
          shinyjs::hide("accumulateInfile")
        }
      } else if(currentOperatorOption() == "compare_data") {
          shinyjs::show("file_selection")
      } else {
        shinyjs::show(currentOperatorOption())
      }
    }

    if (operatorInput_value() == "selpoint.multi") {
      shinyjs::show("add_point")
      shinyjs::show("multi_warning")
      shinyjs::show("chosen_points")
    } else {
      shinyjs::hide("add_point")
      shinyjs::hide("multi_warning")
      shinyjs::hide("chosen_points")
    }
    
    if(operatorInput_value() == "cmsaf.adjust.two.files") {
      shinyjs::show("multi_warning_adjust_two_files")
    } else {
      shinyjs::hide("multi_warning_adjust_two_files")
    }

    if (!isRunningLocally && (operatorInput_value() %in% c("remap", 
                                                         "cmsaf.add", 
                                                         "cmsaf.sub", 
                                                         "fldcor", 
                                                         "fldcovar", 
                                                         "timcor", 
                                                         "timcovar") || input$attachToExisting)) {
      shinyjs::show("twofiles")
    } else {
      shinyjs::hide("twofiles")
    }

  }, ignoreInit = TRUE)

  # Updating the operator table and toggle panel state.
  observeEvent(operatorDataFrameAction(), {
    if (nrow(operatorDataFrame) > 0) {
      shinyjs::hide("spinner_analyze")
      shinyjs::show("listOfOperators")
    }

    output$appliedOperators <- renderTable({
      operatorDataFrame
    })

    output$ncShortInfo <- renderPrint({
      req(nc_path_analyze())
      cmsafops::ncinfo(nc_path_analyze(), nc = nc_object_analyze())
    })
	
  # Operator Group Infos
	output$ogInfo <- renderPrint({
    req(input$operatorGroup)    
	  if(input$operatorGroup == "Hourly statistics") {
		  cat("Hourly statistics\n")
	    cat("Calculate hourly statistics from data with a temporal resolution\n")
	    cat("below one hour (e.g., 1/2 h or 15 min). Thus, it makes no sense to\n")
	    cat("apply these operators for daily or monthly data.\n")
	  }
	  if(input$operatorGroup == "Daily statistics") {
	    cat("Daily statistics\n")
	    cat("Calculate daily statistics from data with a temporal resolution\n")
	    cat("below one day (e.g., instantaneous). Thus, it makes no sense to\n")
	    cat("apply these operators for monthly data.\n")
	    cat("Multi-year operators will give you values for each day of the year.\n")
	    cat("The date information in the output file is the date of the first\n")
      cat("contributing input field.\n")
	  }
	  if(input$operatorGroup == "Monthly statistics") {
	    cat("Monthly statistics\n")
	    cat("Calculate monthly statistics from all timesteps of the same month.\n")
	    cat("Multi-year operators will give you values for each month of the year.\n")
	    cat("The date information in the output file is the date of the first\n")
	    cat("contributing input field.\n")
	  }
	  if(input$operatorGroup == "Seasonal statistics") {
	    cat("Seasonal statistics\n")
	    cat("Calculate seasonal statistics from all timesteps of the same\n") 
	    cat("season.Multi-year operators will give you values for each season\n")
	    cat("of the year.The standard seasons are defined as DJF (output field 1),\n")
	    cat("MAM (2), JJA (3) and SON (4).\n") 
	    cat("The date information in the output file is the date of the first\n")
	    cat("contributing input field.\n")
	  }
	  if(input$operatorGroup == "Annual statistics") {
	    cat("Annual statistics\n")
	    cat("Calculate annual statistics from all timesteps of the same year.\n")
	    cat("The date information in the output file is the date of the first\n")
	    cat("contributing input field.\n")
	  }
	  if(input$operatorGroup == "Temporal operators") {
	    cat("Temporal operators\n")
	    cat("Calculate statistics over all timesteps of a file.\n")
	    cat("Besides standard statistics you will also find operators to do\n")
	    cat("trend analysis or to calculate simple indices.\n")
	    cat("The date information in the output file is the date of the first\n")
	    cat("contributing input field.\n")
	  }
	  if(input$operatorGroup == "Running statistics") {
	    cat("Running statistics\n")
	    cat("Calculate statistics over a selected number of timesteps.\n")
	    cat("You will have to give the number of time steps or number of days\n")
	    cat("(for multi-year statistics).\n")
	    cat("The date information in the output file is the date of the first\n")
	    cat("contributing input field.\n")
	  }
	  if(input$operatorGroup == "Zonal statistics") {
	    cat("Zonal statistics\n")
	    cat("Calculate zonal (along latitude) statistics for\n")
	    cat("each timestep of a file.\n")
	  }
	  if(input$operatorGroup == "Meridional statistics") {
	    cat("Meridional statistics\n")
	    cat("Calculate meridional (along longitude) statistics for\n")
	    cat("each timestep of a file.\n")
	  }
	  if(input$operatorGroup == "Grid boxes statistics") {
	    cat("Grid boxes statistics\n")
	    cat("Calculate statistics for a given grid box size.\n")
	  }
	  if(input$operatorGroup == "Spatial operators") {
	    cat("Spatial operators\n")
	    cat("Calculate spatial statistics for each timestep of a file.\n")
	    cat("The resulting file will contain a 1D-timeseries.\n")
	  }
	  if(input$operatorGroup == "Correlation and covariance") {
	    cat("Correlation and covariance\n")
	    cat("Calculate spatial or temporal correlations or covariances.\n")
	    cat("These operators require a second file.\n")
	  }
	  if(input$operatorGroup == "Mathematical operators") {
	    cat("Mathematical operators\n")
	    cat("Add, multiply, subtract or divide your data by a constant\n")
	    cat("or data from a second file.\n")
	    cat("If you use data from a second file, make sure that\n")
	    cat("the dimensions are matching.\n")
	  }
	  if(input$operatorGroup == "Selection") {
	    cat("Selection\n")
	    cat("These operators provide spatial or temporal selection from your data.\n")
	    cat("Please make sure that the selected values are within the\n")
	    cat("available range.\n")
	  }
	  if(input$operatorGroup == "Data manipulation") {
	    cat("Data manipulation\n")
	    cat("Regrid your data using different interpolation methods.\n")
	    cat("Grid information is taken from a second, which has to be provided.\n")
	  }
	  if(input$operatorGroup == "Climate Analysis") {
	    cat("Climate Analysis\n")
	    cat("Here you will find some more complex operators to do climate analysis.\n")
	    cat("Some of these operators require continous time series of daily data,\n")
	    cat("which might cause some time consuming computations. But, the reults\n")
	    cat("are worth waiting. So far, the resulting graphics or animations\n")
	    cat("are not interactive.\n")
	    cat("For animations you will have to install the ffmpeg library.\n")
	  }
	  if(input$operatorGroup == "Compare Data") {
		  cat("Compare Data\n")
	    cat("This operator provides the option to compare data of two input files.\n")
	    cat("You can compare your spatial data to other spatial data or station\n")
	    cat("data. Station data have to follow a specific format (for an example\n")
	    cat("have a look at the output of 'Select data at multiple points' in\n")
	    cat("CSV format). Adaptation options for the output graphics are limited \n")
	    cat("and the calculation might take some time - we try to improve as soon\n")
	    cat("as possible.\n")
	  }
	  if(input$operatorGroup == "Time range statistics") {
	    cat("Time range statistics\n")
	    cat("These operators compute the mean or sum for a given number\n")
	    cat("of timesteps.\n")
	  }
	 })

    if (nrow(operatorDataFrame) == 0) {
      shinyjs::hide("listOfOperators")
    }
  }, ignoreInit = TRUE)


  # multi day accu graphic message
  observe({
    #shinyjs::hide("multiDayNonAccuGraphic")
    req(input$operatorGroup)
    req(input$operatorGroup == "Climate Analysis")
    req(!(is.null(input$accumulateInfile)))
    req(input$dateRange_analyze)
    req(input$plot_format)
    req(!any(is.na(input$dateRange_analyze)))

    # if (!input$accumulateInfile &&
    #     input$plot_format == "graphic" &&
    #     input$dateRange_analyze[1] != input$dateRange_analyze[2]) {
    #   shinyjs::show("multiDayNonAccuGraphic")
    # }
  })

  observeEvent(input$attachToExisting, {
    if (input$attachToExisting) {
      shinyjs::show("attach_warning")
      updateDateRangeInput(session, "dateRange_analyze",
                           label = "Select date range",
                           min = as.Date("1983-01-01"),
                           max = Sys.Date(),
                           )
      updateSelectInput(session, "climate_year_start",
                        label = "Climatology start year",
                        choices = as.character(format(as.Date("1983-01-01"), format = "%Y"):format(Sys.Date(), format = "%Y"))
                        )
      updateSelectInput(session, "climate_year_end",
                        label = "Climatology end year",
                        choices = as.character(format(as.Date("1983-01-01"), format = "%Y"):format(Sys.Date(), format = "%Y"))
                        )
    } else {
      shinyjs::hide("attach_warning")
      # TODO Can the idea of attaching be thought of when an nc object is used?
      nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(nc, "time", "units")$value, ncdf4::ncvar_get(nc, "time")))
      ncdf4::nc_close(nc)
      updateDateRangeInput(session, "dateRange_analyze",
                                          label = "Select date range",
                                          start = min(date_time[format(date_time, "%Y") == format(max(date_time), "%Y")]),
                                          end = max(date_time),
                                          min = min(date_time),
                                          max = max(date_time)
                           )
      years <- unique(format(date_time, format = "%Y"))
      if (length(years) == 1) {
        end_year <- years[1]
      } else {
        end_year <- years[length(years) - 1]
      }

      updateSelectInput(session, "climate_year_start",
                             label = "Climatology start year",
                             choices = years,
                             selected = years[1])
      updateSelectInput(session, "climate_year_end",
                             label = "Climatology end year",
                             choices = years,
                             selected = end_year)

    }
  }, ignoreInit = TRUE)

  applyOperatorActiv <<- FALSE   # because something went wrong, when apply compare data
  # Apply the operator and show details in table.
  observeEvent(input$applyOperator, {
    req(operatorInput_value())
    req(input$usedVariable)
    
    applyOperatorActiv <<- TRUE
    
    # Disable till done.
    shinyjs::disable("applyOperator")
    shinyjs::hide("listOfOperators")
    shinyjs::show("spinner_analyze", anim = TRUE, animType = "fade")

    # Update the current operator value.
    if (currentOperatorOption() == "None") {
      currentOperatorValue(NULL)
    } else {
      if (currentOperatorOption() == "dateRange") {
        currentOperatorValue(input[["dateRange_analyze"]])
      } else {
        currentOperatorValue(input[[currentOperatorOption()]])
      }
    }

    # Create a time stamp
    time <- as.numeric(format(Sys.time(), "%H%M%S"))

    # Update outfile path
    newOutfile <- file.path(outputDir, paste0(input$usedVariable, "_", operatorInput_value(), time, ".nc"))

    if (newOutfile == nc_path_analyze()) {
      # Adding a star tag at end  to prevent equal input and output file name.
      newOutfile <- file.path(outputDir, paste0(input$usedVariable, "_", operatorInput_value(), time, "x.nc"))
    }

    # Here I chose a shorter outfile. Easily revertable.
    # newOutfile <- paste0(outputDir, "/", input$usedVariable,"_Apply.Function_", operatorInput_value(), time, ".nc")

    # Run the operator
    if (currentOperatorOption() == "None") {
      argumentList <- list(var = input$usedVariable,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "threshold") {
      argumentList <- list(var = input$usedVariable,
                           thld = input$threshold,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
	} else if (currentOperatorOption() == "constant") {
      argumentList <- list(var = input$usedVariable,
                           const = input$constant,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "region") {
      # Sellonlatbox
      argumentList <- list(var = input$usedVariable,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           # lon1 = input$lonRegionSlider[1],
                           # lon2 = input$lonRegionSlider[2],
                           # lat1 = input$latRegionSlider[1],
                           # lat2 = input$latRegionSlider[2],
                           lon1 = input$lonRegionMin,
                           lon2 = input$lonRegionMax,
                           lat1 = input$latRegionMin,
                           lat2 = input$latRegionMax,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "point") {
      if (operatorInput_value() == "selpoint") {
        argumentList <- list(var = input$usedVariable,
                             infile = nc_path_analyze(),
                             outfile = newOutfile,
                             lon1 = input$lonPoint,
                             lat1 = input$latPoint,
                             nc34 = input$format,
                             overwrite = TRUE,
                             nc = nc_object_analyze())
      } else {
        newOutfile <- nc_path_analyze()
        format <- "nc"
        nc34 <- input$format
        if (input$format == 5) {
          format <- "csv"
          nc34 <- 3
        }
        argumentList <- list(var = input$usedVariable,
                             infile = nc_path_analyze(),
                             outpath = outputDir,
                             lon1 = as.numeric(chosen_lonPoints()),
                             lat1 = as.numeric(chosen_latPoints()),
                             nc34 = nc34,
                             format = format,
                             nc = nc_object_analyze())
        chosen_lonPoints(c())
        chosen_latPoints(c())
      }
    } else if (currentOperatorOption() == "dateRange") {
      argumentList <- list(var = input$usedVariable,
                           start = input$dateRange_analyze[1],
                           end = input$dateRange_analyze[2],
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "useFastTrend") {
      trendValue <- 1
      if (!input$useFastTrend) {
        trendValue <- 2
      }
      argumentList <- list(var = input$usedVariable,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           option = trendValue,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "percentile") {
      argumentList <- list(var = input$usedVariable,
                           p = input$percentile,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "gridbox") {
      argumentList <- list(var = input$usedVariable,
                           lonGrid = input$gridbox_lon,
                           latGrid = input$gridbox_lat,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "running") {
      argumentList <- list(var = input$usedVariable,
                           nts = input$running_nts,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "timeRange") {
      argumentList <- list(var = input$usedVariable,
                           nts = input$timeRange_nts,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    } else if (currentOperatorOption() == "months") {
      monthList <- which(c("January", "February", "March", "April", "May",
                           "June", "July", "August", "September", "October",
                           "November", "December")
                         %in% input$months)
      argumentList <- list(var = input$usedVariable,
                           month = monthList,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    }else if (currentOperatorOption() == "years") {
      argumentList <- list(var = input$usedVariable,
                           year = input$years,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    }else if (currentOperatorOption() == "times") {
      argumentList <- list(var = input$usedVariable,
                           hour_min = input$times,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc = nc_object_analyze())
    }else if (currentOperatorOption() == "method") {

      # Select second input file depending on local or remote session
      if (!isRunningLocally) {
        infile2 <- second_infile()
      } else {
        infile2 <- try( file.choose(new = TRUE) )
      }

      if (class(infile2) == "try-error") {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("An additional file is required for this operator."),
          br(),
          h4("The grid information of infile2 are the target grid for the interpolation. This File may also be an ASCII-File containing the grid information."),
          br(),
          h4("For more information please type '?cmsafops::remap' in your R console."),
          title = "No remap file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        req(FALSE)
      } else if (!file.exists(infile2) || !(endsWith(infile2, ".nc") || endsWith(infile2, ".txt"))) {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("The additional file needs to be a", tags$strong(".nc"), " file (or", tags$strong(".txt"), " in case of ASCII-format)."),
          br(),
          h4("For more information please type '?cmsafops::remap' in your R console."),
          title = "No remap file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")

        req(FALSE)
      }

      argumentList <- list(var = input$usedVariable,
                           infile1 = nc_path_analyze(),
                           infile2 = infile2,
                           outfile = newOutfile,
                           method = input$method,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc1 = nc_object_analyze())
    } else if (currentOperatorOption() == "file_select") {
      # Select second input file depending on local or remote session
      if (!isRunningLocally) {
        infile2 <- second_infile()
      } else {
        infile2 <- try( file.choose(new = TRUE) )
      }

      if (class(infile2) == "try-error") {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("An additional file is required for this operator."),
          br(),
          h4("If you want to add/subtract a constant value, please select the operator 'Add constant to data'/'Subtract constant from data'"),
          title = "No additional file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        req(FALSE)
      } else if (!file.exists(infile2) || !(endsWith(infile2, ".nc"))) {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("The additional file needs to be a", tags$strong(".nc"), " file."),
          br(),
          h4("For more information please type '?cmsafops::cmsaf.add' in your R console."),
          title = "File selection error",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")

        req(FALSE)
      }
      argumentList <- list(var1 = input$usedVariable,
                           var2 = input$usedVariable,
                           infile1 = nc_path_analyze(),
                           infile2 = infile2,
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE,
                           nc1 = nc_object_analyze())
    } else if (currentOperatorOption() == "file_selection") {
      if(infile2_analyze_value() == "" || second_variable_analyze() == 0){
        
        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        showNotification("This operator requires a second NetCDF file. Please select a second file. ")
        req(FALSE)
      }
      
      if(operatorInput_value() == "cmsaf.adjust.two.files") {
        two_files_compare_data_vis(1)
        time <- as.numeric(format(Sys.time(), "%H%M%S"))
        newOutfile1 <- file.path(outputDir, paste0("match_data_", time, "_", cmsafops::get_basename(nc_path_analyze(), nc_object_analyze())))
        newOutfile2 <- file.path(outputDir, paste0("match_data_", time, "_", cmsafops::get_basename(infile2_analyze_value())))
    
        argumentList <- list(var1 = input$usedVariable, infile1 = nc_path_analyze(), 
                             var2 = second_variable_analyze(), infile2 = infile2_analyze_value(), 
                             outfile1 = newOutfile1, outfile2 = newOutfile2, 
                             nc34 = input$format, overwrite = FALSE, verbose = FALSE,
                             nc1 = nc_object_analyze())
      } else {
        plot_type_1d <<- operatorInput_value()
        plot_type_2d <<- operatorInput_value()
        # adjust the two input files
        temp_infile_one <- file.path(tempdir(), "infile_one_tmp.nc")
        temp_infile_two <- file.path(tempdir(), "infile_two_tmp.nc")
        
        # unlink two tmp files
        if(file.exists(temp_infile_one)){
          unlink(temp_infile_one)
        }
        if(file.exists(temp_infile_two)){
          unlink(temp_infile_two)
        }
        
        cmsafops::cmsaf.adjust.two.files(var1 = input$usedVariable, infile1 = nc_path_analyze(), 
                                         var2 = second_variable_analyze(), infile2 = infile2_analyze_value(), 
                                         outfile1 = temp_infile_one, outfile2 = temp_infile_two, 
                                         nc34 = 4, overwrite = FALSE, verbose = FALSE,
                                         nc1 = nc_object_analyze())
        
        # set the new location of input files
        nc_path_analyze(temp_infile_one)
        nc_object_analyze(NULL)
        infile2_analyze_value(temp_infile_two)
        
        argumentList <- list(var1 = input$usedVariable,
                             var2 = second_variable_analyze(),
                             infile1 = nc_path_analyze(),
                             infile2 = infile2_analyze_value(),
                             outfile = newOutfile,
                             nc34 = input$format,
                             overwrite = TRUE,
                             verbose = TRUE,
                             # This is currently always NULL but included for consistency.
                             nc1 = nc_object_analyze())
      }
    } else if(currentOperatorOption() == "compare_data") {
      if(infile2_analyze_value() == ""){
        
        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        showNotification("This operator requires a second NetCDF file. Please select a second file. ")
        req(FALSE)
      }
      
      compare_temp_dir <- file.path(tempdir(), "compare_temp")
      # remove if it exists
      if (dir.exists(compare_temp_dir)) {
        unlink(compare_temp_dir, recursive = TRUE)
      }

      # create new nc_temp
      if (!dir.exists(compare_temp_dir)) {
        dir.create(compare_temp_dir)
      }
      
      if((endsWith(infile2_analyze_value(), ".nc"))) {
        # Set as the filename of the nc object if it is being used.
        if (!is.null(nc_object_analyze())) analyze_file1_plot(nc_object_analyze()$filename)
        else analyze_file1_plot(nc_path_analyze())
        analyze_file2_plot(infile2_analyze_value())
        
        temp_infile_one_sel <- file.path(tempdir(), "infile_one_tmp_sel.nc")
        temp_infile_two_sel <- file.path(tempdir(), "infile_two_tmp_sel.nc")
        
        if(file.exists(temp_infile_one_sel)){
          unlink(temp_infile_one_sel)
        }
        if(file.exists(temp_infile_two_sel)){
          unlink(temp_infile_two_sel)
        }
        
        cmsafops::selperiod(var = input$usedVariable, 
                            start = input$dateRange_compare_data[1], end = input$dateRange_compare_data[2], 
                            infile = nc_path_analyze(), outfile = temp_infile_one_sel, 
                            nc34 = input$format, overwrite = TRUE,
                            nc = nc_object_analyze())
        cmsafops::selperiod(var = second_variable_analyze(), 
                            start = input$dateRange_compare_data[1], end = input$dateRange_compare_data[2], 
                            infile = infile2_analyze_value(), outfile = temp_infile_two_sel, 
                            nc34 = input$format, overwrite = TRUE)
        
        # set the new location of input files
        nc_path_analyze(temp_infile_one_sel)
        nc_object_analyze(NULL)
        infile2_analyze_value(temp_infile_two_sel)
        
        if(operatorInput_value() == "cmsaf.diff.absolute"){
          # set first operator of checkbox for dropdown visualize
          outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), time, ".nc"))
          if (outfile_compare == nc_path_analyze()) {
            outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), time, "x.nc"))
          }
          checkboxCompare_value <- operatorInput_value()
          list_compare_data[[checkboxCompare_value]] <<- outfile_compare
          #list_compare_data[[operatorInput_value()]] <<- newOutfile
          newOutfile <- outfile_compare
          argumentList <- list(var1 = input$usedVariable,
                               var2 = second_variable_analyze(),
                               infile1 = nc_path_analyze(),
                               infile2 = infile2_analyze_value(),
                               outfile = outfile_compare,
                               nc34 = input$format,
                               relative = FALSE,
                               overwrite = TRUE,
                               toolbox = TRUE,
                               nc1 = nc_object_analyze())
        } else if(operatorInput_value() == "cmsaf.diff.relative"){
          # set first operator of checkbox for dropdown visualize
          outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), time, ".nc"))
          if (outfile_compare == nc_path_analyze()) {
            outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), time, "x.nc"))
          }
          checkboxCompare_value <- operatorInput_value()
          list_compare_data[[checkboxCompare_value]] <<- outfile_compare
          #list_compare_data[[operatorInput_value()]] <<- newOutfile
          newOutfile <- outfile_compare
          argumentList <- list(var1 = input$usedVariable,
                               var2 = second_variable_analyze(),
                               infile1 = nc_path_analyze(),
                               infile2 = infile2_analyze_value(),
                               outfile = outfile_compare,
                               nc34 = input$format,
                               relative = TRUE,
                               overwrite = TRUE,
                               toolbox = TRUE,
                               nc1 = nc_object_analyze())
        } else {
          two_files_compare_data_vis(1)
          time <- as.numeric(format(Sys.time(), "%H%M%S"))
          newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), "_1_", time, ".nc"))
          newOutfile2 <- file.path(compare_temp_dir, paste0(second_variable_analyze(), "_", operatorInput_value(), "_2_", time, ".nc"))
          if (newOutfile1 == nc_path_analyze()) {
            newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), "_1_", time, "x.nc"))
          }
          if (newOutfile2 == nc_path_analyze()) {
            newOutfile2 <- file.path(compare_temp_dir, paste0(second_variable_analyze(), "_", operatorInput_value(), "_2_", time, "x.nc"))
          }
          checkboxCompare_value <- operatorInput_value()
          list_compare_data[[checkboxCompare_value]] <<- newOutfile1
          checkboxCompare_value <- paste0(checkboxCompare_value, "2")
          list_compare_data[[checkboxCompare_value]] <<- newOutfile2
          
          argumentList <- list(var1 = input$usedVariable, 
                               infile1 = nc_path_analyze(), 
                               var2 = second_variable_analyze(), 
                               infile2 = infile2_analyze_value(), 
                               outfile1 = newOutfile1, 
                               outfile2 = newOutfile2, 
                               nc34 = input$format, 
                               overwrite= TRUE, 
                               verbose = FALSE,
                               toolbox = TRUE,
                               nc1 = nc_object_analyze())
        } 
  
        if(length(checkboxCompareData()) > 1) {
          for(i in 2:length(checkboxCompareData())){
            # Create a time stamp
            time <- as.numeric(format(Sys.time(), "%H%M%S"))
  
            if(checkboxCompareData()[i] == "cmsaf.diff.absolute"){
              outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, ".nc"))
              if (outfile_compare == nc_path_analyze()) {
                outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, "x.nc"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- outfile_compare
              
              argumentList_compare_data <- list(var1 = input$usedVariable,
                                               var2 = second_variable_analyze(),
                                               infile1 = nc_path_analyze(),
                                               infile2 = infile2_analyze_value(),
                                               outfile = outfile_compare,
                                               nc34 = input$format,
                                               relative = FALSE,
                                               overwrite = TRUE,
                                               toolbox = TRUE,
                                               nc1 = nc_object_analyze())
              fun <- get("cmsaf.diff", asNamespace("cmsafvis"))
            }
            else if(checkboxCompareData()[i] == "cmsaf.diff.relative"){
              outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, ".nc"))
              if (outfile_compare == nc_path_analyze()) {
                outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, "x.nc"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- outfile_compare
              
              argumentList_compare_data <- list(var1 = input$usedVariable,
                                               var2 = second_variable_analyze(),
                                               infile1 = nc_path_analyze(),
                                               infile2 = infile2_analyze_value(),
                                               outfile = outfile_compare,
                                               nc34 = input$format,
                                               relative = TRUE,
                                               overwrite = TRUE,
                                               toolbox = TRUE,
                                               nc1 = nc_object_analyze())
              fun <- get("cmsaf.diff", asNamespace("cmsafvis"))
            } else if(checkboxCompareData()[i] == "cmsaf.stats") {
              outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, ".csv"))
              if (outfile_compare == nc_path_analyze()) {
                outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, "x.csv"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- outfile_compare
              argumentList_compare_data <- list(var1 = input$usedVariable,
                                                var2 = second_variable_analyze(),
                                                infile1 = nc_path_analyze(),
                                                infile2 = infile2_analyze_value(),
                                                outfile = outfile_compare,
                                                nc34 = input$format,
                                                overwrite = TRUE,
                                                nc1 = nc_object_analyze())
              fun <- get("cmsaf.stats", asNamespace("cmsafops"))
              cmsaf_stats_enable(1)
            } else {
              # Update outfile path
              newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], "_1_", time, ".nc"))
              newOutfile2 <- file.path(compare_temp_dir, paste0(second_variable_analyze(), "_", checkboxCompareData()[i], "_2_",time, ".nc"))
              if (newOutfile1 == nc_path_analyze()) {
                newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], "_1_", time, "x.nc"))
              }
              if (newOutfile2 == nc_path_analyze()) {
                newOutfile2 <- file.path(compare_temp_dir, paste0(second_variable_analyze(), "_", checkboxCompareData()[i], "_2_", time, "x.nc"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- newOutfile1
              checkboxCompare_value <- paste0(checkboxCompare_value, "2")
              list_compare_data[[checkboxCompare_value]] <<- newOutfile2
        
              argumentList_compare_data <- list(var1 = input$usedVariable, 
                                                infile1 = nc_path_analyze(), 
                                                var2 = second_variable_analyze(), 
                                                infile2 = infile2_analyze_value(), 
                                                outfile1 = newOutfile1, 
                                                outfile2 = newOutfile2, 
                                                nc34 = input$format, 
                                                overwrite= TRUE, 
                                                verbose = FALSE,
                                                toolbox = TRUE,
                                                nc1 = nc_object_analyze())
              fun <- get(checkboxCompareData()[i], asNamespace("cmsafvis"))
            } 
            try(do.call(fun, argumentList_compare_data))
          }
        }
      } else {
        if (!is.null(nc_object_analyze())) analyze_file1_plot(nc_object_analyze()$filename)
        else analyze_file1_plot(nc_path_analyze())
        analyze_file2_plot(infile2_analyze_value())
        
        if(operatorInput_value() %in% c("cmsaf.diff.absolute", "cmsaf.diff.relative")) {
          outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), time, ".nc"))
          if (outfile_compare == nc_path_analyze()) {
            outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), time, "x.nc"))
          }
          checkboxCompare_value <- operatorInput_value()
          list_compare_data[[checkboxCompare_value]] <<- outfile_compare
          newOutfile <- outfile_compare
          cmsafops::selperiod(var = input$usedVariable, 
                              start = input$dateRange_compare_data[1], end = input$dateRange_compare_data[2], 
                              infile = nc_path_analyze(), outfile = outfile_compare, 
                              nc34 = input$format, overwrite = TRUE,
                              nc = nc_object_analyze())
        } else {
          two_files_compare_data_vis(1)
          time <- as.numeric(format(Sys.time(), "%H%M%S"))
          newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), "_1_", time, ".nc"))
          if (newOutfile1 == nc_path_analyze()) {
            newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", operatorInput_value(), "_1_", time, "x.nc"))
          }
          
          checkboxCompare_value <- operatorInput_value()
          list_compare_data[[checkboxCompare_value]] <<- newOutfile1
          checkboxCompare_value <- paste0(checkboxCompare_value, "2")
          list_compare_data[[checkboxCompare_value]] <<- infile2_analyze_value()
          cmsafops::selperiod(var = input$usedVariable, 
                              start = input$dateRange_compare_data[1], end = input$dateRange_compare_data[2], 
                              infile = nc_path_analyze(), outfile = newOutfile1, 
                              nc34 = input$format, overwrite = TRUE,
                              nc = nc_object_analyze())
        }
        
        if(length(checkboxCompareData()) > 1) {
          for(i in 2:length(checkboxCompareData())) {
            if(checkboxCompareData()[i] == "cmsaf.diff.absolute") {
              outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, ".nc"))
              if (outfile_compare == nc_path_analyze()) {
                outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, "x.nc"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- outfile_compare
              
              cmsafops::selperiod(var = input$usedVariable, 
                                  start = input$dateRange_compare_data[1], end = input$dateRange_compare_data[2], 
                                  infile = nc_path_analyze(), outfile = outfile_compare, 
                                  nc34 = input$format, overwrite = TRUE,
                                  nc = nc_object_analyze())
            } else if(checkboxCompareData()[i] == "cmsaf.diff.relative") {
              outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, ".nc"))
              if (outfile_compare == nc_path_analyze()) {
                outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, "x.nc"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- outfile_compare
              
              cmsafops::selperiod(var = input$usedVariable, 
                                  start = input$dateRange_compare_data[1], end = input$dateRange_compare_data[2], 
                                  infile = nc_path_analyze(), outfile = outfile_compare, 
                                  nc34 = input$format, overwrite = TRUE,
                                  nc = nc_object_analyze())
            } else if(checkboxCompareData()[i] == "cmsaf.stats") {
              outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, ".csv"))
              if (outfile_compare == nc_path_analyze()) {
                outfile_compare <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], time, "x.csv"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- outfile_compare
              
              cmsafops::cmsaf.stats.station.data(
                var = input$usedVariable,
                infile = nc_path_analyze(),
                data_station = station_data_compare(),
                outfile = outfile_compare,
                overwrite = TRUE,
                nc = nc_object_analyze()
              )
              cmsaf_stats_enable(1)
            } else {
              newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], "_1_", time, ".nc"))
              if (newOutfile1 == nc_path_analyze()) {
                newOutfile1 <- file.path(compare_temp_dir, paste0(input$usedVariable, "_", checkboxCompareData()[i], "_1_", time, "x.nc"))
              }
              checkboxCompare_value <- checkboxCompareData()[i]
              list_compare_data[[checkboxCompare_value]] <<- newOutfile1
              checkboxCompare_value <- paste0(checkboxCompare_value, "2")
              list_compare_data[[checkboxCompare_value]] <<- infile2_analyze_value()
              
              cmsafops::selperiod(var = input$usedVariable, 
                                  start = input$dateRange_compare_data[1], end = input$dateRange_compare_data[2], 
                                  infile = nc_path_analyze(), outfile = newOutfile1, 
                                  nc34 = input$format, overwrite = TRUE,
                                  nc = nc_object_analyze())
            }
          }
        }
      }
    }
    else if (currentOperatorOption() == "monitor_climate") {
      # THE MONITOR CLIMATE FROM CMSAFVIS
      if (input$plot_format == "graphic") {
        fileext <- ".png"
      }  else {
        fileext <- ".mp4"
      }
      
      if(operatorInput_value() %in% c("warming_stripes_plot", "time_series_plot", "trend_plot")){
        fileext <- ".png"
      }

      #monitor_climate_out_dir <- tempdir()
      monitor_climate_temp_dir <- file.path(outputDir, "mc_temp")
      if (!dir.exists(monitor_climate_temp_dir)) {
        dir.create(monitor_climate_temp_dir)
      }

      monitor_climate_out_dir <- file.path(outputDir)

      monitor_climate_outfile <- paste0(input$usedVariable, "_", operatorInput_value(), time, fileext)
      monitor_climate_outfile_path <- file.path(monitor_climate_out_dir, monitor_climate_outfile)

      if (input$attachToExisting) {
      # Select second input file depending on local or remote session
      if (!isRunningLocally) {
        infile_attach <- second_infile()
      } else {
        infile_attach <- try( file.choose(new = TRUE) )
      }

      if (class(infile_attach) == "try-error") {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("An additional file is required for this operator."),
          br(),
          h4("If you want to add/subtract a constant value, please select the operator 'Add constant to data'/'Subtract constant from data'"),
          title = "No additional file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        req(FALSE)
      } else if (!file.exists(infile_attach) || !(endsWith(infile_attach, ".nc"))) {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("The additional file needs to be a", tags$strong(".nc"), " file."),
          br(),
          h4("For more information please type '?cmsafvis::monitor_climate' in your R console."),
          title = "File selection error",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")

        req(FALSE)
      }
      } else {
      infile_attach <- "auto"
    }

      argumentList <- list(
        plot_type = operatorInput_value(),
        infile = nc_path_analyze(),
        accumulate = input$accumulateInfile,   # if true: accumulate infile
        mean_value = !(input$accumulateInfile),   # if true: mean infile
        variable = input$usedVariable,
        output_format = input$plot_format,
        animation_pace = input$animation_pace,
        freeze_animation = FALSE,
        lang = "eng",
        outfile_name = monitor_climate_outfile,
        start_date = input$dateRange_analyze[1],
        end_date = input$dateRange_analyze[2],
        country_code = input$country,
        # lon_min = input$lonRegionSlider[1],
        # lon_max = input$lonRegionSlider[2],
        # lat_min = input$latRegionSlider[1],
        # lat_max = input$latRegionSlider[2],
        lon_min = input$lonRegionMin,
        lon_max = input$lonRegionMax,
        lat_min = input$latRegionMin,
        lat_max = input$latRegionMax,
        out_dir = monitor_climate_out_dir,
        temp_dir = monitor_climate_temp_dir,
        climate_dir = monitor_climate_temp_dir,
        attach = input$attachToExisting,
        infile_attach = infile_attach
      )

      if (operatorInput_value() != "absolute_map") {
        argumentList <- append(
          argumentList,
          list(
            climate_year_start = as.numeric(input$climate_year_start),
            climate_year_end = as.numeric(input$climate_year_end)
          )
        )
      }
	  
	  if (operatorInput_value() == "anomaly_map") {
        argumentList <- append(
          argumentList,
          list(
            color_pal = as.numeric(input$stripecol),
            relative  = as.logical(input$absrel)
          )
        )
      }
      
      if(operatorInput_value() %in% c("time_series_plot", "trend_plot")){
        argumentList <- append(
          argumentList,
          list(
            analyze_method = input$monitorClimateAnalyzeMethod,
            selected_number = as.numeric(input$analyzeTimeSize)
          )
        )
      }
      if(operatorInput_value() %in% c("warming_stripes_plot")){
        argumentList <- append(
          argumentList,
          list(
            analyze_method = input$monitorClimateAnalyzeMethod,
            selected_number = as.numeric(input$analyzeTimeSize),
            color_pal = as.numeric(input$stripecol),
            circ_plot = as.numeric(input$circular)
          )
        )
      }
    }

    climate_analysis_ops <- c("absolute_map", "anomaly_map", "climatology_map", "fieldmean_plot", "fieldmean_and_anomaly_map", 
                              "warming_stripes_plot", "time_series_plot", "trend_plot")

    # Get package and function
    if (operatorInput_value() %in% climate_analysis_ops) {
      fun <- get("monitor_climate", asNamespace("cmsafvis"))
    } else if(operatorInput_value() %in% operatorOptionsDict[["compare_data"]]) {
      if((endsWith(infile2_analyze_value(), ".nc"))) {
        if(operatorInput_value() == "cmsaf.diff.absolute" || operatorInput_value() == "cmsaf.diff.relative") {
          fun <- get("cmsaf.diff", asNamespace("cmsafvis"))
        } else {
          fun <- get(operatorInput_value(), asNamespace("cmsafvis"))
        }
      }
    }
    else {
      fun <- get(operatorInput_value(), asNamespace("cmsafops"))
    }
    
    if((endsWith(infile2_analyze_value(), ".nc")) || (infile2_analyze_value() == "")) {   # if second file is a nc-file or no second file is available
      res <- try(do.call(fun, argumentList))
    } else {
      res <- ""
    }
    
    # Error handling
    if (class(res) == "try-error") {
      showModal(modalDialog(
        h4("An error occured while applying the operator."),
        tags$p(paste0("Message: ", res)),
        title = "Error!",
        size = "l"
      ))
    } else {
      shinyjs::hide("add_point")
      # No error. Continue with rest.

      # If monitor climate store png/mp4 paths
      if (operatorInput_value() %in% climate_analysis_ops) {
        image_path_visualize( monitor_climate_outfile_path )
        actionVisualizeMonitorClimate(actionVisualizeMonitorClimate() + 1)

        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        return()
      } else {
        if(two_files_compare_data_vis() == 1){   # two files to visualize 
          operator_Input2 <- paste0(operatorInput_value(), "2")
          nc_path_analyze(list_compare_data[[operatorInput_value()]])
          nc_object_analyze(NULL)
          infile2_analyze_value(list_compare_data[[operator_Input2]])
          nc_path_visualize(list_compare_data[[operatorInput_value()]])
          nc_object_visualize(NULL)
          nc_path_visualize_2(list_compare_data[[operator_Input2]])
        } else {
          if((endsWith(infile2_analyze_value(), ".nc"))) {
            nc_path_analyze(newOutfile)
            nc_object_analyze(NULL)
            nc_path_visualize(newOutfile)
            nc_object_visualize(NULL)
          } else {
            nc_path_analyze(newOutfile)
            nc_object_analyze(NULL)
            nc_path_visualize(newOutfile)
            nc_object_visualize(NULL)
          }
        }
      }

      # Variable 'isRunningLocally' can be found in global.R
      if (repeatWarning() && !isRunningLocally) {
        showModal(modalDialog(
          tags$p("A .nc file has been created for you. ",
                 "Its contents are temporarily stored in the session folder.",
                 "Please download the session folder if you want to save it.",
                 "All files will be removed after closing this app!"),
          checkboxInput("noRepeat", "Do not show this message again."),
          title = "Warning to prevent data loss!",
          size = "l"
        ))
      }

      if (input$applyAnother) {
        # Depending on operator give option details.
        if (currentOperatorOption() == "None") {
          newRow <- data.frame(operatorInput_value(), "None", "None")
        } else if (currentOperatorOption() == "point") {
          newRow <- data.frame(operatorInput_value(), "point", paste0("lat: ", input$latPoint, ", lon: ", input$lonPoint))
        } else if (currentOperatorOption() == "region") {
          # newRow <- data.frame(operatorInput_value(), "region", paste0("lat: [", input$latRegionSlider[1], " ", input$latRegionSlider[2], "], ",
          #                                                            "lon: [", input$lonRegionSlider[1], " ", input$lonRegionSlider[2], "]"))
          newRow <- data.frame(operatorInput_value(), "region", paste0("lat: [", input$latRegionMin, " ", input$latRegionMax, "], ",
                                                                       "lon: [", input$lonRegionMin, " ", input$lonRegionMax, "]"))
        } else if (currentOperatorOption() == "dateRange") {
          newRow <- data.frame(operatorInput_value(), "dateRange", paste0("from ", input$dateRange_analyze[1], " to ", input$dateRange_analyze[1]))
        } else {
          newRow <- data.frame(operatorInput_value(), currentOperatorOption(), input[[currentOperatorOption()]])
        }

        lastCols <- data.frame(cmsafops::get_basename(newOutfile))
        newRow <- cbind(newRow, lastCols)

        # Give the row names.
        names(newRow) <- c("Operator", "Option", "Value", "Output File")
        # Bind row to table
        if (nrow(operatorDataFrame)) {
          operatorDataFrame <<- rbind(operatorDataFrame, newRow)
          operatorDataFrameAction(operatorDataFrameAction() + 1)
        } else {
          operatorDataFrame <<- newRow
          operatorDataFrameAction(operatorDataFrameAction() + 1)
        }
      } else if (input$instantlyVisualize) {
        # Switching to visualize screen.
        actionVisualize(actionVisualize() + 1)
      } else {
        # Remove spinner here and reset to analyze.
        resetToAnalyzePanel()
      }
    }

    # Either way allow to apply another operator and remove spinner.
    shinyjs::hide("spinner_analyze")
    if (input$applyAnother) {
      shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
    }
    shinyjs::enable("applyOperator")

  }, ignoreInit = TRUE)

  observeEvent({
    actionVisualize()
    action_visualize_post_modal()
  },{
    shinyjs::hide("downloadExitMonitorClimate")
    shinyjs::hide("myImage_monitorClimate")
    shinyjs::show("downloadExit")
  })

  observeEvent(actionVisualizeMonitorClimate(), {
    shinyjs::hide("downloadExit")
    shinyjs::show("downloadExitMonitorClimate")
  })
  
  # second file with variable
  observeEvent(input$file_selection_button, {
    # Select second input file depending on local or remote session
    if (!isRunningLocally) {
      infile2 <- second_infile()
    } else {
      infile2 <- try( file.choose(new = TRUE) )
    }
    
    if (class(infile2) != "try-error") {
      if((endsWith(infile2, ".nc"))) {
        station_data_compare(0)
        infile2_analyze_value(infile2)
        id <- ncdf4::nc_open(infile2)
        variable_second <- names(id$var)
        
        showModal(modalDialog(
          h4("It seems that you have selected a second file. Please select a variable to continue."),
          br(),
          fluidRow(column(7, selectInput(inputId = "second_variable_analyze",
                                         label = "Please choose a variable.",
                                         choices = variable_second)),
                   column(5, actionButton("action_visualize_two_variables",
                                          "Using this variable.",
                                          width = "100%"))),
          title = "We need your help.",
          size = "l",
          footer = NULL
        ))
      } else if((endsWith(infile2, ".RData"))){
        infile2_analyze_value(infile2)
        station_rdata <- get(load(infile2))
        station_data_compare(as.data.frame(station_rdata))
        showModal(modalDialog(
          h4("It seems that you have selected a .RData file. "),
          br(),
          fluidRow(column(5, actionButton("action_visualize_two_variables",
                                          "Using this file.",
                                          width = "100%"))),
          title = "We need your help.",
          size = "l",
          footer = NULL
        ))
      } else if((endsWith(infile2, ".csv"))){
        infile2_analyze_value(infile2)
        station_csv <- utils::read.csv(infile2,header = TRUE, sep = ";")
        station_data_compare(as.data.frame(station_csv))
        showModal(modalDialog(
          h4("It seems that you have selected a .csv file. "),
          br(),
          fluidRow(column(5, actionButton("action_visualize_two_variables",
                                          "Using this file.",
                                          width = "100%"))),
          title = "We need your help.",
          size = "l",
          footer = NULL
        ))
      }
    } else {
      station_data_compare(0)
      showModal(modalDialog(
        h4("An additional file is required for this operator."),
        title = "No additional file given.",
        size = "l"
      ))
      
      # Hide spinner allow new operation and leave silently.
      shinyjs::hide("spinner_analyze")
      if (input$applyAnother) {
        shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
      }
      shinyjs::enable("applyOperator")
      req(FALSE)
    }
  })
  
  observeEvent(input$action_visualize_two_variables, {
    if(endsWith(infile2_analyze_value(), ".nc")) {
      # Update the variable
      second_variable_analyze(input$second_variable_analyze)
      
      # Show selected file name in UI
      output$ncFile_analyze_second_file <- renderUI({
        tags$pre("You selected the following .nc file: ",
                 cmsafops::get_basename(infile2_analyze_value()))
      })
      shinyjs::show("ncFile_analyze_second_file")
      
      if(currentOperatorOption() == "compare_data"){
        list_date_range <- cmsafops::calc_overlapping_time(var1 = input$usedVariable, infile1 = nc_path_analyze(), 
                                                           var2 = second_variable_analyze(), infile2 = infile2_analyze_value())
        
        # Show date range of second file
        start_date_reac(list_date_range[[1]])
        end_date_reac(list_date_range[[2]])
        
        output$date_range_compare_data <- renderUI({
          dateRangeInput(inputId = "dateRange_compare_data",
                         label = "Please select a date range.",
                         start = start_date_reac(),
                         end = end_date_reac(),
                         min = start_date_reac(),
                         max = end_date_reac(),
                         startview = "year"
          )
        })
        shinyjs::show("date_range_compare_data")
      }
    } else {
      # Show selected file name in UI
      output$ncFile_analyze_second_file <- renderUI({
        tags$pre("You selected the following file: ",
                 cmsafops::get_basename(infile2_analyze_value()))
      })
      shinyjs::show("ncFile_analyze_second_file")
      
      if(currentOperatorOption() == "compare_data") {
        list_date_range <- cmsafops::calc_overlapping_time(var1 = input$usedVariable, infile1 = nc_path_analyze(), 
                                                           var2 = second_variable_analyze(), infile2 = infile2_analyze_value(),
                                                           nc1 = nc_object_analyze())
        
        # Show date range of second file
        start_date_reac(list_date_range[[1]])
        end_date_reac(list_date_range[[2]])
        
        output$date_range_compare_data <- renderUI({
          dateRangeInput(inputId = "dateRange_compare_data",
                         label = "Please select a date range.",
                         start = start_date_reac(),
                         end = end_date_reac(),
                         min = start_date_reac(),
                         max = end_date_reac(),
                         startview = "year"
          )
        })
        shinyjs::show("date_range_compare_data")
      }
    }
    # This will remove the modal.
    removeModal()
  })

  #### VISUALIZE ####
  getVariableData <- function(timestep_index, id, var) {
    return(ncdf4::ncvar_get(id, var, start = c(1, 1, timestep_index), count = c(-1, -1, 1)))
  }

  # A function to read all required information from nc file
  # that's needed for the visualize options.
  # Also sets the correct image width and height.
  get_visualize_options <- function(infile, var, infile2 = NULL, var2 = NULL,
                                    nc1 = NULL, nc2 = NULL) {
    # Open file and get data
    if (!is.null(nc1)) id <- nc1
    else id <- ncdf4::nc_open(infile)
    # Remap to regGrid if necessary
    file_info <- cmsafops:::check_dims(id)
    if (is.null(nc1)) ncdf4::nc_close(id)
    if (!file_info$isRegGrid) {
      remap_timestamp <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
      remap_name <- paste0("remap_", remap_timestamp, ".nc")
      outfile <- file.path(userDir, remap_name)
      # grid_filepath can be  found in global.R
      cmsafops::remap(var, infile, grid_filepath, outfile, overwrite = TRUE, nc1 = nc1)
      infile <- outfile
      # To prevent nc1 being used in place of outfile.
      nc1 <- NULL
      nc_path_visualize(infile)
      nc_object_visualize(NULL)
    }
    
    if((!is.null(nc2) || !is.null(infile2)) && (endsWith(infile2_analyze_value(), ".nc"))) {
      if (!is.null(nc2)) id2 <- nc2
      else id2 <- ncdf4::nc_open(infile2)
      
      lon2 <- ncdf4::ncvar_get(id2, "lon")
      lat2 <- ncdf4::ncvar_get(id2, "lat")
      
      data2 <- try(ncdf4::ncvar_get(id2, var2, collapse_degen = FALSE))
      if (all(is.na(data2))) {
        stop("The second file you are trying to visualize constains only NA values.")
      }
    }
    # Open file and get data
    if (!is.null(nc1)) id <- nc1
    else id <- ncdf4::nc_open(infile)

    lon <- ncdf4::ncvar_get(id, "lon")
    lat <- ncdf4::ncvar_get(id, "lat")
    
    data <- try(ncdf4::ncvar_get(id, var, collapse_degen = FALSE))

    if (all(is.na(data))) {
      stop("The file you are trying to visualize constains only NA values.")
    }

    visualizeDataTimestep(getVariableData(1, id, var))
    
    date <- ncdf4::ncvar_get(id, "time")
    t_unit <- ncdf4::ncatt_get(id, "time", "units")$value
    date.time <- as.character(cmsafops::get_time(t_unit, date))
    
    unit <- ncdf4::ncatt_get(id, var, "units")$value
    if (unit == 0)
      (unit <- "-")
    vn <- var
    varname <- ncdf4::ncatt_get(id, var, "long_name")$value
    if (varname == 0)
      (varname <- ncdf4::ncatt_get(id, var, "standard_name")$value)
    if (varname == 0)
      (varname <- var)
    
    time_bounds = NULL
    if (file_info$has_time_bnds){
      time_bound0 <- ncdf4::ncvar_get(id, "time_bnds", collapse_degen = FALSE)
      time_bound1 <- as.character(cmsafops::get_time(t_unit, time_bound0[1,]))
      time_bound2 <- as.character(cmsafops::get_time(t_unit, time_bound0[2,]))
      if (startsWith(t_unit, "hours")) {
        time_bound1 <- as.POSIXct(time_bound1, format = "%Y-%m-%d %R")
        time_bound2 <- as.POSIXct(time_bound2, format = "%Y-%m-%d %R")
      } else {
        time_bound1 <- as.Date(time_bound1)
        time_bound2 <- as.Date(time_bound2)
      }
      for (i in seq_along(time_bound1)){
        time_bounds <- append(time_bounds, paste(time_bound1[i], " \u2013 ", time_bound2[i], sep = "")) 
      }
    }

    creator_att <- ncdf4::ncatt_get(id, 0, "institution")
    if (!creator_att$hasatt) {
      creator_att <- ncdf4::ncatt_get(id, 0, "publisher_name")
      if (!creator_att$hasatt) {
        creator_att <- ncdf4::ncatt_get(id, 0, "creator_name")  
      } 
    } 
    creator <- ifelse(creator_att$hasatt, creator_att$value, "-")
    copyrightText <- paste0("Data Source: ", creator)

    if (is.null(nc1)) ncdf4::nc_close(id)

    # In HOAPS files problems can occur due to different sorted values
    # check data dimensions

    validData <- (class(data) != "try-error")

    reversedDimensions$transpose  <- FALSE
    reversedDimensions$lonReverse <- FALSE
    reversedDimensions$latReverse <- FALSE
    if(length(lon) > 1 && length(lat) > 1){   # added this, because of some new operators (eg.zon and mer)
      if (dim(visualizeDataTimestep())[1] != length(lon)) {
        if (dim(visualizeDataTimestep())[2] == length(lon)) {
          reversedDimensions$transpose <- TRUE
          visualizeDataTimestep(aperm(visualizeDataTimestep(), c(2, 1)))
          if (validData) {
            data <- aperm(data, c(2, 1, 3))
          }
        }
      }
    }
    
    # check longitude
    if (lon[1] > lon[length(lon)]) {
      reversedDimensions$lonReverse <- TRUE
      lon <- rev(lon)

      visualizeDataTimestep(visualizeDataTimestep()[rev(seq_len(length(lon))), ])
      if (validData) {
        if (dim(data)[3] == 1) {
          data[, , 1] <- data[rev(seq_len(length(lon))), , ]
        } else {
          data <- data[rev(seq_len(length(lon))), , ]
        }
      }
    }
    # check latitude
    if (lat[1] > lat[length(lat)]) {
      lat <- rev(lat)
      reversedDimensions$latReverse <- TRUE
      visualizeDataTimestep(visualizeDataTimestep()[, rev(seq_len(length(lat)))])

      if (validData) {
        if (dim(data)[3] == 1) {
          data[, , 1] <- data[, rev(seq_len(length(lat))), ]
        } else {
          data <- data[, rev(seq_len(length(lat))), ]
        }
      }
    }
    
    if(!is.null(nc2) || !is.null(infile2)) { # Returning compare data plots for two files
      #visualizeDataTimestep(getVariableData(1, id2, var2))
      if((endsWith(infile2_analyze_value(), ".nc"))) {
        date2 <- ncdf4::ncvar_get(id2, "time")
        t_unit2 <- ncdf4::ncatt_get(id2, "time", "units")$value
        date.time2 <- as.character(cmsafops::get_time(t_unit2, date2))
        
        unit2 <- ncdf4::ncatt_get(id2, var2, "units")$value
        if (unit2 == 0)
          (unit2 <- "-")
        vn2 <- var2
        varname2 <- ncdf4::ncatt_get(id2, var2, "long_name")$value
        if (varname2 == 0)
          (varname2 <- ncdf4::ncatt_get(id2, var2, "standard_name")$value)
        if (varname2 == 0)
          (varname2 <- var2)
        
        creator_att2 <- ncdf4::ncatt_get(id2, 0, "publisher_name")
        if (!creator_att2$hasatt) {
          creator_att2 <- ncdf4::ncatt_get(id2, 0, "creator_name")  
        } 
        creator2 <- ifelse(creator_att2$hasatt, creator_att2$value, "-")
        copyrightText2 <- paste0("Data Source: ", creator2)
        
        ylabel <- paste0(varname2, " [", unit2, "]")
        
        if (is.null(nc2)) ncdf4::nc_close(id2)
      }
      visualizeDataMin(min(data, na.rm = TRUE))
      visualizeDataMax(max(data, na.rm = TRUE))
      
      # if (startsWith(t_unit, "hours")) {
      #   date.time <- as.POSIXct(date.time, format = "%Y-%m-%d %R")
      # } else {
      #date.time <- as.Date(date.time)
      #}
      
      fit <- stats::lm(as.vector(data) ~ c(seq_along(data)))
      dummy <- fit$fitted.values
      if (sum(!is.na(data)) == length(dummy)) {
        fitted <- data
        fitted[which(!is.na(fitted))] <- dummy
        rm(dummy)
      }
      
      xlabel <- paste0(varname, " [", unit, "]")
      x_range <- length(date.time)
      
      
      # Returning 2D plot information (for side by side plot)
      min_lon <- min(lon, na.rm = TRUE)
      max_lon <- max(lon, na.rm = TRUE)
      
      min_lat <- min(lat, na.rm = T)
      max_lat <- max(lat, na.rm = T)
      
      if (class(data) != "try-error") {
        min_data <- min(data, na.rm = TRUE)
        max_data <- max(data, na.rm = TRUE)
        
        if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
          min_data <- min_data - 0.05
          max_data <- max_data + 0.05
        }
        
        visualizeDataMin(min_data)
        visualizeDataMax(max_data)
      }
      
      if(checkboxCompareData_dropdown() == "cmsaf.side.by.side"){
        plot_dim_value = 2
      } else {
        date.time <- as.Date(date.time)
        plot_dim_value = 1
      }
      if((endsWith(infile2_analyze_value(), ".nc"))) {
        return(
          list(
            plot_type = checkboxCompareData_dropdown(),
            plot_dim = plot_dim_value,
            min_lon = min_lon,
            max_lon = max_lon,
            min_lat = min_lat,
            max_lat = max_lat,
            data = data,
            data2 = data2,
            date.time = date.time,
            time_bounds = time_bounds,
            x_range = x_range,
            ylabel = ylabel,
            xlabel = xlabel,
            vn = vn,
            vn2 = vn2,
            varname = varname,
            varname2 = varname2,
            file_name = infile,
            file_name2 = infile2,
            unit = unit,
            lat = lat,
            lon = lon,
            fitted = fitted,
            copyrightText = copyrightText,
            nc = nc1,
            nc2 = nc2
          )
        )
      } else {   # actual .RData and csv files
        if(checkboxCompareData_dropdown() == "cmsaf.time.series") {
          station_data_time_series_plot <- cmsafvis::helper_time_series_compare(list(
            data = data,
            data2 = station_data_compare(),
            date.time = date.time,
            lat = lat,
            lon = lon
          ))
        } else {
          station_data_time_series_plot <- list()
        }
        ylabel <- paste0(varname, " [", unit, "]")
        return(
          list(
            plot_type = checkboxCompareData_dropdown(),
            plot_dim = plot_dim_value,
            min_lon = min_lon,
            max_lon = max_lon,
            min_lat = min_lat,
            max_lat = max_lat,
            data = data,
            data2 = station_data_compare(),
            data3 = station_data_time_series_plot,   # only for time series plot (compare data)
            date.time = date.time,
            time_bounds = time_bounds,
            x_range = x_range,
            ylabel = ylabel,
            xlabel = xlabel,
            vn = vn,
            varname = varname,
            file_name = infile,
            unit = unit,
            lat = lat,
            lon = lon,
            fitted = fitted,
            copyrightText = copyrightText,
            nc = nc1,
            nc2 = NULL
          )
        )
      }
    } else if (length(lon) == 1 && length(lat) == 1) {   # Returning 1D plot information
      min_lon <- lon
      max_lon <- lon
      min_lat <- lat
      max_lat <- lat

      visualizeDataMin(min(data, na.rm = TRUE))
      visualizeDataMax(max(data, na.rm = TRUE))
      x_range <- length(data)
      ltype <- c("l", "p", "o", "s", "h")
      
      if (startsWith(t_unit, "hours")) {
        date.time <- as.POSIXct(date.time, format = "%Y-%m-%d %R")
      } else {
        date.time <- as.Date(date.time)
      }
      
      fit <- stats::lm(as.vector(data) ~ c(seq_along(data)))
      dummy <- fit$fitted.values
      if (sum(!is.na(data)) == length(dummy)) {
        fitted <- data
        fitted[which(!is.na(fitted))] <- dummy
        rm(dummy)
      }
      ylabel <- paste0(varname, " [", unit, "]")
    
      return(
        list(
          plot_type = plot_type_1d,
          plot_dim = 1,
          data = data,
          date.time = date.time,
          time_bounds = time_bounds,
          x_range = x_range,
          ylabel = ylabel,
          vn = vn,
          varname = varname,
          unit = unit,
          lat = lat,
          lon = lon,
          fitted = fitted,
          copyrightText = copyrightText
        )
      )
    } else if ((length(lon) == 1 && length(lat) != 1) ||
               (length(lon) != 1 && length(lat) == 1)) {   # plot x-axis: lat or lon and y-axis: data
      # Rendering 1D Plot information
      if(length(lon) == 1 && length(lat) != 1){    # x-axis is lat
        x_range <- length(lat)
      } else if(length(lon) != 1 && length(lat) == 1){    # x-axis is lon
        x_range <- length(lon)
      }
      
      visualizeDataMin(min(data, na.rm = TRUE))
      visualizeDataMax(max(data, na.rm = TRUE))
      
      if (startsWith(t_unit, "hours")) {
        date.time <- as.POSIXct(date.time, format = "%Y-%m-%d %R")
      } else {
        date.time <- as.Date(date.time)
      }
      
      fit <- stats::lm(as.vector(data) ~ c(seq_along(data)))
      dummy <- fit$fitted.values
      if (sum(!is.na(data)) == length(dummy)) {
        fitted <- data
        fitted[which(!is.na(fitted))] <- dummy
        rm(dummy)
      }
      ylabel <- paste0(varname, " [", unit, "]")
      
      return(
        list(
          plot_dim = 1,
          data = data,
          date.time = date.time,
          time_bounds = time_bounds,
          x_range = x_range,
          ylabel = ylabel,
          vn = vn,
          varname = varname,
          unit = unit,
          lat = lat,
          lon = lon,
          fitted = fitted,
          copyrightText = copyrightText
        )
      )
    } else if(length(lon) != 1 && length(lat) != 1) {
      # Returning 2D plot information
      min_lon <- min(lon, na.rm = TRUE)
      max_lon <- max(lon, na.rm = TRUE)

      min_lat <- min(lat, na.rm = T)
      max_lat <- max(lat, na.rm = T)

      if (class(data) != "try-error") {
        min_data <- min(data, na.rm = TRUE)
        max_data <- max(data, na.rm = TRUE)

        if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
          min_data <- min_data - 0.05
          max_data <- max_data + 0.05
        }

        visualizeDataMin(min_data)
        visualizeDataMax(max_data)
      }
      
      if(!(endsWith(analyze_file2_plot(), ".nc"))){   # second file station data for compare diff plot
        plot_station_data = 1;
      } else {
        plot_station_data = 0;
      }
    
      return(
        list(
          plot_type = checkboxCompareData_dropdown(),
          plot_station_data = plot_station_data,
          plot_dim = 2,
          data = data,
          date.time = date.time,
          time_bounds = time_bounds,
          min_lon = min_lon,
          max_lon = max_lon,
          min_lat = min_lat,
          max_lat = max_lat,
          vn = vn,
          varname = varname,
          unit = unit,
          lat = lat,
          lon = lon,
          copyrightText = copyrightText
        )
      )
    }
  }
 
  observeEvent(input$action_visualize_variable_modal, {
    # Update the variable
    variable_visualize_modal(input$variable_visualize_modal)

    # This will re-trigger the visualization process
    action_visualize_post_modal(action_visualize_post_modal() + 1)

    # This will remove the modal.
    removeModal()
  })
  
  switchButtonCompare <<- FALSE
  # Compare data dropdown
  observeEvent(input$operatorInput_dropdown, {
    req(operatorInput_value() != "")
    if(switchButtonCompare){
      if(input$operatorInput_dropdown %in% operators[["Compare Data"]]) {
        shinyjs::hide("visualizePage")
        checkboxCompareData_dropdown(input$operatorInput_dropdown)
        if(input$operatorInput_dropdown == "cmsaf.diff.absolute" || input$operatorInput_dropdown == "cmsaf.diff.relative") {
          nc_path_visualize_2("")   # reset path of second file
          nc_path_visualize(list_compare_data[[input$operatorInput_dropdown]])
          nc_object_visualize(NULL)
        }
        else {
          operatorInput_file_2 <- paste0(input$operatorInput_dropdown, "2")
          nc_path_visualize_2(list_compare_data[[operatorInput_file_2]])
          nc_path_visualize(list_compare_data[[input$operatorInput_dropdown]])
          nc_object_visualize(NULL)
        }
        actionVisualize(actionVisualize() + 1)
        shinyjs::show("spinner_visualize_compare_data", anim = TRUE, animType = "fade")
      } 
    } else {
      switchButtonCompare <<- TRUE
    }
  })
  
  # This will set up the intial input values on visualize page.
  observeEvent({
    actionVisualize()
    action_visualize_post_modal()
  }, {
    req(nc_path_visualize())
    shinyjs::hide("panel_visualizeGo")
    shinyjs::show("spinner_visualize", anim = TRUE, animType = "fade")
    shiny::showTab(inputId = "mainVisualizeTabset", target = "Statistics")
    shiny::showTab(inputId = "mainVisualizeTabset", target = "File Summary")
    shiny::hideTab(inputId = "mainVisualizeTabset", target = "Parameters") # hide main parameters of monitor climate
    
    # compare data
    if(operatorInput_value() %in% operatorOptionsDict[["compare_data"]]){
      shinyjs::show("dropdown_compare_data")
      
      tmp_vec <- checkboxCompareData()
      
      tmp_vec <- tmp_vec[tmp_vec != "cmsaf.stats"]
      
      if((endsWith(infile2_analyze_value(), ".csv")) || endsWith(infile2_analyze_value(), ".RData")) {
        tmp_vec <- tmp_vec[tmp_vec != "cmsaf.hovmoller"]
        plots_station <- c()
        plots_station[["Compare Data"]] <- c(
          "Difference plot (absolute)" = "cmsaf.diff.absolute",
          "Difference plot (relative)" = "cmsaf.diff.relative",
          "Scatterplot" = "cmsaf.scatter",
          "Bar chart" = "cmsaf.hist",
          "Side-by-Side plot" = "cmsaf.side.by.side",
          "Comparison of time series" = "cmsaf.time.series"
        )
        match_plot <- match(tmp_vec, plots_station[["Compare Data"]])
        new_operators <- plots_station[["Compare Data"]]
      } else {
        match_plot <- match(tmp_vec, operators[["Compare Data"]])
        new_operators <- operators[["Compare Data"]]
      }
      
      output$dropdown_compare_data <- renderUI({
        selectInput("operatorInput_dropdown",
                    label = "Select a plot type",
                    choices = new_operators[match_plot],
                    selected = checkboxCompareData_dropdown(),
                    selectize = FALSE)
      })
      
      if(nc_path_visualize_2() != ""){
        if((endsWith(infile2_analyze_value(), ".nc"))) {
          id_2 <- ncdf4::nc_open(nc_path_visualize_2())
          vn_2 <- names(id_2$var)
          ncdf4::nc_close(id_2)
        }
      }
      
      if("cmsaf.stats" %in% checkboxCompareData()){
        path_cmsaf_stats <- list_compare_data[["cmsaf.stats"]]
        
        if(file.exists(path_cmsaf_stats)){
          stats_compare_data <- read.csv(path_cmsaf_stats)
          stats_compare_data_df <- as.data.frame(stats_compare_data)
          output$metrics <- renderPrint({
            print(stats_compare_data_df)
          })
        }
      }
    } else
        shinyjs::hide("dropdown_compare_data")

    if (!is.null(nc_object_visualize())) id <- nc_object_visualize()
    else id <- ncdf4::nc_open(nc_path_visualize())
    vn <- names(id$var)
    dn <- names(id$dim)
    if (is.null(nc_object_visualize())) ncdf4::nc_close(id)

    if (!("time" %in% dn)) {
      showModal(modalDialog(
        h4("Sorry, the file you chose does not contain a time dimension."),
        title = "Error!",
        size = "l"))

      resetToPreparePanel()
    } else {

    vn <- subset(vn, !(vn %in% c("lat", "lon", "latitude", "longitude", "time_bnds", "nb2", "time", "crs")))

    # If more than one we allow user to choose a variable. Catch this input here.
    if (!is.null(variable_visualize_modal())) {
      if (is.element(variable_visualize_modal(), vn)) {
        vn <- variable_visualize_modal()

        variable_visualize_modal(NULL)
      } else {
        showModal(modalDialog(
          h4("The chosen input variable doesn't exist in this file. Try another file or input argument."),
          br(),
          title = "Oops.",
          size = "l"
        ))
      }
    }

    if (length(vn) > 1) {
      showModal(modalDialog(
        h4("Seems like you are trying to visualize a file with multiple variables. Please select a variable in order to continue."),
        br(),
        fluidRow(column(7, selectInput(inputId = "variable_visualize_modal",
                                       label = "Please choose a variable.",
                                       choices = vn)),
                 column(5, actionButton("action_visualize_variable_modal",
                                        "Visualize using this variable.",
                                        width = "100%"))),
        title = "We need your help.",
        size = "l",
        footer = NULL
      ))

      # Leave
      req(FALSE)
    } else if (length(vn) < 1) {
      showModal(modalDialog(
        h4("The chosen file doesn't have a variable."),
        title = "Error!",
        size = "m"
      ))

      # Leave
      req(FALSE)
    } else if (vn == "sinusoidal") {
      # TODO: Change this as soon as sinusodial is done.
      # Stop if data are in sinusoidal projection
      showModal(modalDialog(
        h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
             Please use the 'change projection' option during the order process.
             Your NetCDF data have to be on a regular lon-lat grid."),
        title = "Error!",
        size = "l"
      ))

      # Leave
      req(FALSE)
    } else {
      # Trying. If error go back to Visualize page.
      # Maybe visualizeVariables isn't loaded correctly second time?
      if(nc_path_visualize_2() != "") {
        res <- try(visualizeVariables(get_visualize_options(nc_path_visualize(), vn, nc_path_visualize_2(), vn_2, nc1 = nc_object_visualize())))
      } else {
        res <- try(visualizeVariables(get_visualize_options(nc_path_visualize(), vn, nc1 = nc_object_visualize())))
      }

      if (class(res) == "try-error") {
        showModal(modalDialog(
          h3("An error occured while preparing the visualization. Please try another file."),
          br(),
          h5(paste0(res)),
          title = "Error!",
          size = "l"))

        # Go to visualize page.
        resetToVisualizePanel()
      } else {

        shinyjs::hide("setupPage")
        shinyjs::hide("spinner_visualize")
        shinyjs::hide("spinner_visualize_compare_data")
        shinyjs::show("visualizePage", anim = TRUE, animType = "fade")

        # No error. Let's plot!
        # Rendering all gui elements dependent on file.
        # 2D Plot:
        if (visualizeVariables()$plot_dim == 2) {

          shiny::showTab(inputId = "mainVisualizeTabset", target = "Statistics")
          shinyjs::hide("sidebar_1d_plot")
          shinyjs::hide("myImage_1d")
          shinyjs::hide("spinner_visualize")
          shinyjs::hide("filedownload")
          shinyjs::hide("dropdown_station_number")

          shinyjs::show("myImage_2d")
          shinyjs::show("sidebar_2d_plot")
          
          if(cmsaf_stats_enable() == 0){   # checkbox "Show statistics" is not enabled
            shiny::hideTab(inputId = "mainVisualizeTabset", target = "Metrics")
          } else {
            shiny::showTab(inputId = "mainVisualizeTabset", target = "Metrics")
          }
          
          value_text2 <- " "
          if(checkboxCompareData_dropdown() == "cmsaf.side.by.side"){
            shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")
            shinyjs::hide("num_rmax")
            shinyjs::hide("num_rmin")
            if((endsWith(infile2_analyze_value(), ".nc"))){
              shinyjs::show("title_text2")
              shinyjs::show("subtitle_text2")
              output$title_text2 <- renderUI({
                textInput("text1_2",
                          label = "Title",
                          value = visualizeVariables()$varname2)
              })
              
              # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
              # regex1 <- regmatches( analyze_file1_plot(), regexpr(pattern, analyze_file1_plot()))
              # Replace with cmsafops::get_basename() which also accounts for URLs
              regex1 <- cmsafops::get_basename(analyze_file1_plot())
              value_text2 <- regex1
              regex2 <- regmatches(analyze_file2_plot(), regexpr(pattern, analyze_file2_plot()))
              output$subtitle_text2 <- renderUI({
                textInput("text2_2",
                          label = "Subtitle",
                          value = regex2)
              })
            } else {
              shinyjs::hide("title_text2")
              shinyjs::hide("subtitle_text2")
            }
            shinyjs::hide("proj")
          } else {
            shinyjs::hide("title_text2")
            shinyjs::hide("subtitle_text2")
            shinyjs::show("num_rmax")
            shinyjs::show("num_rmin")
            if(checkboxCompareData_dropdown() == "cmsaf.diff.absolute" || checkboxCompareData_dropdown() == "cmsaf.diff.relative"){
              shinyjs::hide("proj")
            } else {
              shinyjs::show("proj")
            }
          }
          
          # initialize timestep_c
          timestep_c(visualizeVariables()$date.time[1])
          
          if (!is.null(visualizeVariables()$time_bounds)){
            output$timestep_visualize <- renderUI({
              selectInput("timestep",
                          label = "Select Time Step",
                          choices = visualizeVariables()$time_bounds,
                          selected = visualizeVariables()$time_bounds[1],
                          selectize = FALSE)
            })
          } else {
            output$timestep_visualize <- renderUI({
              selectInput("timestep",
                          label = "Select Time Step",
                          choices = visualizeVariables()$date.time,
                          selected = visualizeVariables()$date.time[1],
                          selectize = FALSE)
            })
          }
          
          observeEvent(input$timestep, {
            if (!is.null(visualizeVariables()$time_bounds)){
              timestep_c(visualizeVariables()$date.time[which(visualizeVariables()$time_bounds == input$timestep)])
            }
          })
          
          # output$lon_visualize <- renderUI({
          #   tmp <- c(max(round(as.numeric(visualizeVariables()$min_lon)), -180), min(round(as.numeric(visualizeVariables()$max_lon)), 180))
          #   lon_bounds(tmp)
          #   sliderInput("slider1",
          #               label = "Longitude",
          #               min = max(round(as.numeric(visualizeVariables()$min_lon)) - 20, -180),
          #               max = min(round(as.numeric(visualizeVariables()$max_lon)) + 20, 180),
          #               value = c(tmp[1], tmp[2]))
          # })
          
          # Use shinyWidgets instead of lon / lat slider
          #
          output$lon_visualize <- renderUI({
            tmp <- c(max(round(as.numeric(visualizeVariables()$min_lon), digits = 2), -180), 
                     min(round(as.numeric(visualizeVariables()$max_lon), digits = 2), 180))
            lon_bounds(tmp)
            shinyWidgets::numericRangeInput("slider1",
                        label = "Longitude",
                        min = -180,
                        max = 180,
                        value = c(tmp[1], tmp[2]))
          })
         
          output$lat_visualize <- renderUI({
            tmp <- c(max(round(as.numeric(visualizeVariables()$min_lat), digits = 2), -90), 
                     min(round(as.numeric(visualizeVariables()$max_lat), digits = 2), 90))
            lat_bounds(tmp)
            shinyWidgets::numericRangeInput("slider2",
                        label = "Latitude",
                        min = -90,
                        max = 90,
                        value = c(tmp[1], tmp[2]))
          })
          
          # output$lat_visualize <- renderUI({
          #   tmp <- c(max(round(as.numeric(visualizeVariables()$min_lat)), -90), min(round(as.numeric(visualizeVariables()$max_lat)), 90))
          #   lat_bounds(tmp)
          # 
          #   sliderInput("slider2",
          #               label = "Latitude",
          #               min = max(round(as.numeric(visualizeVariables()$min_lat)) - 20, -90),
          #               max = min(round(as.numeric(visualizeVariables()$max_lat)) + 20, 90),
          #               value = c(tmp[1], tmp[2]))
          # })
          
          output$ihsf_visualize <- renderUI({
            tmp <- 0.1
            ihsf(tmp)
            sliderInput("decimal",
                        label = "Image Ratio",
                        min = -0.9, max = 0.9,
                        value = 0.1, ticks = FALSE)
          })
          
          output$title_text <- renderUI({
            textInput("text1",
                      label = "Title",
                      value = visualizeVariables()$varname)
          })
          
          output$subtitle_text <- renderUI({
            textInput("text2",
                      label = "Subtitle",
                      value = value_text2)   # because side.by.side plot has a default value
          })
          
          if(plot_type_2d %in% c("timcor", "timcovar")){
            if(plot_type_2d == "timcor"){
              caption_scale <- "Correlation over time"
            }
            if(plot_type_2d == "timcovar"){
              caption_scale <- "Covariance over time"
            }
          } else {
            caption_scale <- paste0(visualizeVariables()$varname, " [", visualizeVariables()$unit, "]")
          }
          
          output$scale_caption <- renderUI({
            textInput("text3",
                      label = "Scale Caption",
                      value = caption_scale)
          })
        } else if(nc_path_visualize_2() != "") {
          if(cmsaf_stats_enable() == 0){   # checkbox "Show statistics" is not enabled
            shiny::hideTab(inputId = "mainVisualizeTabset", target = "Metrics")
          } else {
            shiny::showTab(inputId = "mainVisualizeTabset", target = "Metrics")
          }
          
          # Hide Statistics panel
          shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")
          
          if(checkboxCompareData_dropdown() == "cmsaf.time.series") {
            shinyjs::hide("sidebar_2d_plot")
            shinyjs::hide("myImage_2d")
            
            shinyjs::show("myImage_1d")
            shinyjs::show("sidebar_1d_plot")
            shinyjs::hide("analyze_timeseries")
            shinyjs::hide("trend")
            shinyjs::show("filedownload")
            shinyjs::show("dateformat")
            
            shinyjs::hide("date_dropdown_visualize")
            
            output$title_text_1d <- renderUI({
              textInput("text1_1d",
                        label = "Title",
                        value = visualizeVariables()$varname)
            })
            
            output$subtitle_text_1d <- renderUI({
              textInput("text2_1d",
                        label = "Subtitle",
                        value = " ")
            })
            
            if((endsWith(infile2_analyze_value(), ".csv")) || (endsWith(infile2_analyze_value(), ".RData"))){
              if("time" %in% colnames(station_data_compare())){
                station_time_first <- station_data_compare()$time[1]
                b <- lengths(regmatches( station_data_compare()$time, gregexpr(station_time_first,  station_data_compare()$time)))
                station_count <- length(b[b==1])
              } else if ("date" %in% colnames(station_data_compare())){
                station_count <- length(unique(station_data_compare()$date))
              } else if ("zeit" %in% colnames(station_data_compare())){
                station_count <- length(unique(station_data_compare()$zeit))
              } else if ("t" %in% colnames(station_data_compare())){
                station_count <- length(unique(station_data_compare()$t))
              } else if ("get_time.file_data.time_info.units..file_data.dimension_data.t." %in% colnames(station_data_compare())){
                station_count <- length(unique(station_data_compare()$get_time.file_data.time_info.units..file_data.dimension_data.t.))
              }
              shinyjs::show("dropdown_station_number")
              #station_count_seq <- seq(station_count)
              
              labs <- NULL
              for (i in seq_along(station_data_compare()$lon)) {
                dummy <- paste0("[", round(station_data_compare()$lon[i], digits = 1), ";", round(station_data_compare()$lat[i], digits = 1), "]")
                labs <- append(labs, dummy)
              }
              station_seq <- unique(labs)
              
              output$dropdown_station_number <- renderUI({
                selectInput("operatorInput_dropdown_station_number",
                            label = "Select a station",
                            choices = station_seq,
                            selected = station_seq[1],
                            selectize = FALSE)
              })
              
            } else {
              shinyjs::hide("dropdown_station_number")
            }
            
            # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
            # regex1 <- regmatches( analyze_file1_plot(), regexpr(pattern, analyze_file1_plot()))
            # Replace with cmsafops::get_basename() which also accounts for URLs
            regex1 <- cmsafops::get_basename(analyze_file1_plot())
            
            if((endsWith(infile2_analyze_value(), ".csv"))){
              pattern <- "[^\\/]+(\\.csv)$" # regular exp. to extract filenames
            }
            if((endsWith(infile2_analyze_value(), ".RData"))){
              pattern <- "[^\\/]+(\\.RData)$" # regular exp. to extract filenames
            }
            regex2 <- regmatches(analyze_file2_plot(), regexpr(pattern, analyze_file2_plot()))
            output$x_axis_text_1d <- renderUI({
              textInput("x_axis_label_1d",
                        label = "Legend label 1",
                        value = regex1)
            })
            
            output$y_axis_text_1d <- renderUI({
              textInput("y_axis_label_1d",
                        label = "Legend label 2",
                        value = regex2)
            })
            
            shinyjs::show("x_axis_text_1d")
            shinyjs::show("y_axis_text_1d")
            
            shinyjs::hide("x_visualize")
            shinyjs::hide("y_visualize")
            shinyjs::show("title_text_1d")
            shinyjs::show("subtitle_text_1d")
            shinyjs::hide("integer")
            shinyjs::hide("ticknumber")
            shinyjs::hide("checkGroup_type")
          } else if(checkboxCompareData_dropdown() == "cmsaf.hovmoller"){
            shinyjs::hide("sidebar_2d_plot")
            shinyjs::hide("myImage_2d")
            
            shinyjs::show("myImage_1d")
            shinyjs::show("sidebar_1d_plot")
            shinyjs::hide("analyze_timeseries")
            shinyjs::hide("trend")
            shinyjs::show("filedownload")
            shinyjs::hide("dateformat")
            shinyjs::hide("date_dropdown_visualize")
            shinyjs::hide("dropdown_station_number")
            
            # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
            # regex1 <- regmatches( analyze_file1_plot(), regexpr(pattern, analyze_file1_plot()))
            # Replace with cmsafops::get_basename() which also accounts for URLs
            regex1 <- cmsafops::get_basename(analyze_file1_plot())
            if((endsWith(infile2_analyze_value(), ".csv"))){
              pattern <- "[^\\/]+(\\.csv)$" # regular exp. to extract filenames
            }
            if((endsWith(infile2_analyze_value(), ".RData"))){
              pattern <- "[^\\/]+(\\.RData)$" # regular exp. to extract filenames
            }
            regex2 <- regmatches(analyze_file2_plot(), regexpr(pattern, analyze_file2_plot()))
            output$x_axis_text_1d <- renderUI({
              textInput("x_axis_label_1d",
                        label = "Title 1",
                        value = paste0(visualizeVariables()$xlabel, " (", regex1, ")"))
            })
            
            output$y_axis_text_1d <- renderUI({
              textInput("y_axis_label_1d",
                        label = "Title 2",
                        value = paste0(visualizeVariables()$ylabel, " (", regex2, ")"))
            })
            
            shinyjs::show("x_axis_text_1d")
            shinyjs::show("y_axis_text_1d")
            
            shinyjs::hide("x_visualize")
            shinyjs::hide("y_visualize")
            shinyjs::hide("title_text_1d")
            shinyjs::hide("subtitle_text_1d")
            shinyjs::hide("integer")
            shinyjs::hide("ticknumber")
            shinyjs::hide("checkGroup_type")
          }
          else {
            shinyjs::hide("sidebar_2d_plot")
            shinyjs::hide("myImage_2d")
            
            shinyjs::show("myImage_1d")
            shinyjs::show("sidebar_1d_plot")
            shinyjs::hide("analyze_timeseries")
            shinyjs::hide("trend")
            shinyjs::hide("dateformat")
            shinyjs::show("filedownload")
            shinyjs::hide("dropdown_station_number")
            
            output$date_dropdown_visualize <- renderUI({
              selectInput("timestep_1d_visualize",
                          label = "Select Time Step",
                          choices = visualizeVariables()$date.time,
                          selected = visualizeVariables()$date.time[1],
                          selectize = FALSE)
            })
            shinyjs::show("date_dropdown_visualize")
            
            output$title_text_1d <- renderUI({
              textInput("text1_1d",
                        label = "Title",
                        value = visualizeVariables()$varname)
            })
            
            output$subtitle_text_1d <- renderUI({
              textInput("text2_1d",
                        label = "Subtitle",
                        value = " ")
            })
            
            if(checkboxCompareData_dropdown() == "cmsaf.scatter") {
              # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
              # regex1 <- regmatches( analyze_file1_plot(), regexpr(pattern, analyze_file1_plot()))
              # Replace with cmsafops::get_basename() which also accounts for URLs
              regex1 <- cmsafops::get_basename(analyze_file1_plot())
              if((endsWith(infile2_analyze_value(), ".csv"))){
                pattern <- "[^\\/]+(\\.csv)$" # regular exp. to extract filenames
              }
              if((endsWith(infile2_analyze_value(), ".RData"))){
                pattern <- "[^\\/]+(\\.RData)$" # regular exp. to extract filenames
              }
              regex2 <- regmatches(analyze_file2_plot(), regexpr(pattern, analyze_file2_plot()))
              output$x_axis_text_1d <- renderUI({
                textInput("x_axis_label_1d",
                          label = "Label x axis",
                          value = paste0(visualizeVariables()$xlabel, " (", regex1, ")"))
              })
              
              output$y_axis_text_1d <- renderUI({
                textInput("y_axis_label_1d",
                          label = "Label y axis",
                          value = paste0(visualizeVariables()$ylabel, " (", regex2, ")"))
              })
              
              shinyjs::show("x_axis_text_1d")
              shinyjs::show("y_axis_text_1d")
            } else if(checkboxCompareData_dropdown() == "cmsaf.hist"){
                shinyjs::hide("dropdown_station_number")
              
              # pattern <- "[^\\/]+(\\.nc)$" # regular exp. to extract filenames
              # regex1 <- regmatches( analyze_file1_plot(), regexpr(pattern, analyze_file1_plot()))
              # Replace with cmsafops::get_basename() which also accounts for URLs
              regex1 <- cmsafops::get_basename(analyze_file1_plot())
              
              if((endsWith(infile2_analyze_value(), ".csv"))){
                pattern <- "[^\\/]+(\\.csv)$" # regular exp. to extract filenames
              }
              if((endsWith(infile2_analyze_value(), ".RData"))){
                pattern <- "[^\\/]+(\\.RData)$" # regular exp. to extract filenames
              }
              regex2 <- regmatches(analyze_file2_plot(), regexpr(pattern, analyze_file2_plot()))
              output$x_axis_text_1d <- renderUI({
                textInput("x_axis_label_1d",
                          label = "Legend label 1",
                          value = regex1)
              })
              
              output$y_axis_text_1d <- renderUI({
                textInput("y_axis_label_1d",
                          label = "Legend label 2",
                          value = regex2)
              })
              
              shinyjs::show("x_axis_text_1d")
              shinyjs::show("y_axis_text_1d")
            }
            else {
              shinyjs::hide("x_axis_text_1d")
              shinyjs::hide("y_axis_text_1d")
            }
            
            
            shinyjs::hide("x_visualize")
            shinyjs::hide("y_visualize")
            shinyjs::show("title_text_1d")
            shinyjs::show("subtitle_text_1d")
            shinyjs::hide("integer")
            shinyjs::hide("ticknumber")
            shinyjs::hide("checkGroup_type")
          }
        } else{
          shinyjs::show("separator")
          shinyjs::show("downloadFile")
          if(length(visualizeVariables()$lon) == 1 && length(visualizeVariables()$lat) == 1){
            # 1D-Plot (x-axis: time and y-axis: variable)
            # Hide Statistics panel
            shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")
            shiny::hideTab(inputId = "mainVisualizeTabset", target = "Metrics")
            
            shinyjs::hide("sidebar_2d_plot")
            shinyjs::hide("myImage_2d")
            
            shinyjs::show("myImage_1d")
            shinyjs::show("sidebar_1d_plot")
            shinyjs::show("dateformat")
            shinyjs::show("filedownload")
            
            if(plot_type_1d %in% c("fldcor", "fldcovar")){
              shinyjs::hide("analyze_timeseries")
              shinyjs::hide("trend")
            } else {
              shinyjs::show("analyze_timeseries")
              shinyjs::show("trend")
            }
            
            output$x_visualize <- renderUI({
              sliderInput("sliderx",
                          label = "X-Range",
                          min = 1,
                          max = visualizeVariables()$x_range,
                          value = c(1, visualizeVariables()$x_range))
            })
            
            output$title_text_1d <- renderUI({
              textInput("text1_1d",
                        label = "Title",
                        value = visualizeVariables()$varname)
            })
            
            output$subtitle_text_1d <- renderUI({
              textInput("text2_1d",
                        label = "Subtitle",
                        value = " ")
            })
          } else if((length(visualizeVariables()$lon) == 1 && length(visualizeVariables()$lat) != 1) ||
                    (length(visualizeVariables()$lon) != 1 && length(visualizeVariables()$lat) == 1)){
            # 1D-Plot (x-axis: lat or lon and y-axis: variable)
            
            # Hide Statistics panel
            shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")
            shiny::hideTab(inputId = "mainVisualizeTabset", target = "Metrics")
            
            shinyjs::hide("sidebar_2d_plot")
            shinyjs::hide("myImage_2d")
            
            shinyjs::show("myImage_1d")
            shinyjs::show("sidebar_1d_plot")
            shinyjs::hide("analyze_timeseries")
            shinyjs::hide("trend")
            shinyjs::hide("dateformat")
            shinyjs::show("filedownload")
            
            output$date_dropdown_visualize <- renderUI({
              selectInput("timestep_1d_visualize",
                          label = "Select Time Step",
                          choices = visualizeVariables()$date.time,
                          selected = visualizeVariables()$date.time[1],
                          selectize = FALSE)
            })
            shinyjs::show("date_dropdown_visualize")
            
            output$x_visualize <- renderUI({
              sliderInput("sliderx",
                          label = "X-Range",
                          min = 1,
                          max = visualizeVariables()$x_range,
                          value = c(1, visualizeVariables()$x_range))
            })
            
            output$title_text_1d <- renderUI({
              textInput("text1_1d",
                        label = "Title",
                        value = visualizeVariables()$varname)
            })
            
            output$subtitle_text_1d <- renderUI({
              textInput("text2_1d",
                        label = "Subtitle",
                        value = " ")
            })
          }
        }

        # Start timer independently of 1D/2D plot.
        shinyjs::delay(2000, readyToPlot(TRUE))
      }
    }}

  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Logic after png or mp4 passed to visualizer
  # This will set up the intial input values on visualize page.
  observeEvent({
    actionVisualizeMonitorClimate()
  }, {
    req(image_path_visualize())
    shinyjs::hide("panel_visualizeGo")
    shinyjs::show("spinner_visualize", anim = TRUE, animType = "fade")

    shinyjs::hide("setupPage")
    shinyjs::hide("spinner_visualize")
    shinyjs::show("visualizePage", anim = TRUE, animType = "fade")

    shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")
    shiny::showTab(inputId = "mainVisualizeTabset", target = "Parameters") # show main parameters of monitor climate
    shiny::hideTab(inputId = "mainVisualizeTabset", target = "File Summary")
    shinyjs::hide("sidebar_1d_plot")
    shinyjs::hide("myImage_1d")
    shinyjs::hide("sidebar_2d_plot")
    shinyjs::hide("myImage_2d")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("filedownload")
    shinyjs::hide("date_dropdown_visualize")
    shinyjs::hide("dropdown_compare_data")
    
    # if warming stripes plot or time series plot
    climate_analysis_ops <- c("warming_stripes_plot", "time_series_plot", "trend_plot")
    if(operatorInput_value() %in% climate_analysis_ops)
    {
      #TODO add Interactivity 
    }
    
    shinyjs::show("myImage_monitorClimate")
    
    # show parameters monitor climate
    infile_path <- file.path(tempdir(),"mc_parameters.csv")
    
    if(file.exists(infile_path)){
      mc_parameters <- read.csv(infile_path)
      df_parameters <- as.data.frame(mc_parameters)
      
      # number.of.rows <- nrow(df_parameters)
      # number.of.columns <- ncol(df_parameters)
      
      output$mc_parameters <- renderPrint({
        # print(df_parameters[1:number.of.rows, 2:number.of.columns])
        print(df_parameters)
      })
      
      unlink(infile_path)
    }
    
    # show ranking monitor climate
    infile_ranking_path <- file.path(tempdir(),"mc_ranking.csv")
    if(file.exists(infile_ranking_path)){
      mc_ranking <- read.csv(infile_ranking_path)
      df_ranking <- as.data.frame(mc_ranking)
      # number.of.rows <- nrow(df_ranking)
      # number.of.columns <- ncol(df_ranking)
      
      output$mc_ranking <- renderPrint({
        cat("Ranking\n")
        #print(head(df_ranking[1:number.of.rows, 2:number.of.columns]))
        print(df_ranking)
      })
      
      # remove ranking file
      unlink(infile_ranking_path)
    }

    if (endsWith(image_path_visualize(), "png")) {
      rnd <- list(
        src = image_path_visualize(),
        contentType = "image/png"
      )

      shinyjs::show("monitorClimate_PNG")
      shinyjs::hide("monitorClimate_MP4")

      output$monitorClimate_PNG <- renderImage(rnd, deleteFile = FALSE)
    } else if (endsWith(image_path_visualize(), "mp4")) {
      rnd <- list(
        src = image_path_visualize(),
        contentType = "video/mp4"
      )

      shinyjs::show("monitorClimate_MP4")
      shinyjs::hide("monitorClimate_PNG")

      output$monitorClimate_MP4 <- renderUI({
        # Temporarily copy video to www directory
        # Reason: Giving absolute path is not allowed due to security issues
        if (!dir.exists(videoDir())) {
          dir.create(videoDir())
        }

        tmpVideo <- file.path(videoDir(), "animation.mp4")

        file.copy(
          from = image_path_visualize(),
          to = tmpVideo,
          overwrite = TRUE
        )

        # Render the video
        tags$video(
          id = "video",
          type = "video/mp4",
          src = "video/animation.mp4",
          controls = "controls")
      })
    }

    # Start timer independently of 1D/2D plot.
    shinyjs::delay(2000, readyToPlot(TRUE))

  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Observe changes to lon, lat slider and update image plot width/height
  observeEvent({
    lon_bounds()
    lat_bounds()
    ihsf()
  }, {

    req(visualizeVariables()$plot_dim == 2)

    imDim <- cmsafvis::recalculateImageDimensions(
      visualizeVariables = visualizeVariables(),
      lon_bounds = lon_bounds(),
      lat_bounds = lat_bounds(),
      image_def = image_def,
      ihsf = ihsf()
      )

    imagewidth(imDim$imagewidth)
    imageheight(imDim$imageheight)
    #
    #
    # lon <- visualizeVariables()$lon[visualizeVariables()$lon <= lon_bounds()[2]]
    # lon <- lon[lon_bounds()[1] <= lon]
    #
    # lat <- visualizeVariables()$lat[visualizeVariables()$lat <= lat_bounds()[2]]
    # lat <- lat[lat_bounds()[1] <= lat]
    #
    # # Update this value if you want to change min width/height of plot.
    # minSize <- 200
    # tmpWidth  <- max(minSize, image_def)
    # tmpHeight <- max(minSize, image_def)
    #
    # # Update width and height according to visualizeVariables lat and lon vectors
    # if (length(lon) >= length(lat)) {
    #   # Shrink height
    #   tmpHeight <- round(tmpWidth * length(lat) / length(lon))
    #   if (tmpHeight < minSize) {
    #     tmpWidth <- minSize / tmpHeight * tmpWidth
    #     tmpHeight <- minSize
    #   }
    #
    #   # Why are we doing this? (And why not in the else block?)
    #   imageheight(tmpHeight + (round((ihsf * tmpHeight))))
    #   imagewidth(tmpWidth)
    # } else {
    #   # No need to check against minSize since we will multiply with a value > 1.
    #   tmpWidth <- round(tmpHeight * length(lat) / length(lon))
    #
    #   imagewidth(tmpWidth)
    #   imageheight(tmpHeight)
    # }

    lat_lon_trigger(lat_lon_trigger() + 1)
  })

  # Render scale ranges when data range changes.
  output$num_rmin <- renderUI({
    numericInput("num_rmin",
                 label = "Scale Range Min",
                 value = round(visualizeDataMin(), digits = 1),
                 step = 0.1)
  })

  # Render scale ranges when data range changes.
  output$num_rmax <- renderUI({
    if (round(visualizeDataMin(), digits = 1) == round(visualizeDataMax(), digits = 1)) {
      value <- round(visualizeDataMax(), digits = 1) + 0.1
    } else {
      value <- round(visualizeDataMax(), digits = 1)
    }

    numericInput("num_rmax",
                 label = "Scale Range Max",
                 value = value,
                 step = 0.1)
  })

  # Y-Range of 1D plot.
  output$y_visualize <- renderUI({
    req(visualizeDataMax())
    req(visualizeDataMin())
    
    # max values between -1 and 1
    if(visualizeDataMax() <= 1 && visualizeDataMax() >= -1) {
      value_max <- round(visualizeDataMax(), 4)
    } else {
      value_max <- trunc(visualizeDataMax())
    }
      
    # min values between -1 and 1
    if(visualizeDataMin() <= 1 && visualizeDataMin() >= -1) {
      value_min <- round(visualizeDataMin(), 4)
    } else {
      value_min <- trunc(visualizeDataMin())
    }
    
    diff <- visualizeDataMax() - visualizeDataMin()

    sliderInput("slidery",
                label = "Y-Range",
                min = round(visualizeDataMin() - (0.25 * diff), 1),
                max = round(visualizeDataMax() + (0.25 * diff), 1),
                value = c(value_min, value_max))
  })
  
  output$x_visualize <- renderUI({
    req(visualizeVariables()$x_range)
    sliderInput("sliderx",
                label = "X-Range",
                min = 1,
                max = visualizeVariables()$x_range,
                value = c(1, visualizeVariables()$x_range))
  })
  

  # Observe changes to visualize data. If all data are available update min and max values globally.
  # Else we'll need to keep track of them
  observe({
    req(class(visualizeVariables()$data) == "try-error")

    min_data <- min(visualizeDataTimestep(), na.rm = TRUE)
    max_data <- max(visualizeDataTimestep(), na.rm = TRUE)

    if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
      min_data <- min_data - 0.05
      max_data <- max_data + 0.05
    }

    visualizeDataMin(min_data)
    visualizeDataMax(max_data)
  })

  # Observing changes to shape file path
  observeEvent(shapeFile_path_action(), {
    region_data(countriesHigh)
    req(shapeFile_path())
    req(file.exists(shapeFile_path()))

    # data of all regions
    region_data(suppressWarnings(maptools::readShapePoly(shapeFile_path())))
  })

  # Set divisions
  observeEvent(region_data(), {
    all_divisions <- names(region_data())
    if (!is.element("COUNTRY", all_divisions)) {
      all_divisions <- c("COUNTRY", all_divisions)
    }
    output$division_options <- renderUI({
      selectInput("division",
                  "Division",
                  choices = c("Select division", all_divisions))
    })
  })

  # Set regions
  observeEvent(input$division, {
    if (input$division != "Select division") {
      if (input$division != "COUNTRY") {
        all_regions <- levels(region_data()[[input$division]])
      } else {
        # data of all countries
        countries_choosable <- codes[, "iso3c"]
        names(countries_choosable) <- codes[, "country.name.en"]

        all_regions <- countries_choosable
      }

      output$region_options <- renderUI({
        selectInput("region",
                    "Region",
                    choices = c("Select region", all_regions),
                    selected = "Select region")
      })
    } else {
      output$region_options <- renderUI({
        selectInput("region",
                    "Region",
                    choices = c("Select region"))
      })
    }
  })

  # Toggle instat file upload
  observeEvent(input$plot_rinstat, {
    # Is set in global.R
    if (isRunningLocally) {
      if (input$plot_rinstat) {
        shinyjs::show("instat_file_local")
      } else {
        shinyjs::hide("instat_file_local")
      }
    } else {
      if (input$plot_rinstat) {
        shinyjs::show("instat_file_remote")
      } else {
        shinyjs::hide("instat_file_remote")
      }
    }
  }, ignoreInit = TRUE)

  # Go back to set up page.
  observeEvent(input$backToSetup, {
    operatorInput_value("")
    nc_path_visualize_2("")
    resetToVisualizePanel()
  }, ignoreInit = TRUE)

  # Go back to set up page (from monitor climate visualization).
  observeEvent(input$backToSetup2, {
    # Unlink viedo dir
    if (dir.exists(videoDir())) {
      unlink(
        x = videoDir(),
        recursive = TRUE,
        force = TRUE
      )
    }

    resetToVisualizePanel()
  }, ignoreInit = TRUE)

  # Reacting to instat file data.
  instat.data <- reactive({
    req(instat_path())
    req(endsWith(instat_path(), ".RData") || endsWith(instat_path(), ".csv"))
    if(endsWith(instat_path(), ".RData")){
      a <- get(load(instat_path()))
    } else if(endsWith(instat_path(), ".csv")){
      a <- data.table::fread(instat_path(), sep="auto")
    }
    a <- as.data.frame(a)
    a
  })

  # get data for diff plots in compare data
  co.data.compare.diff <- reactive({ 
    req(analyze_file2_plot())
    req(endsWith(analyze_file2_plot(), ".RData") || endsWith(analyze_file2_plot(), ".csv"))
    req(timestep_c())
    
    a <- station_data_compare()
    data_nc <- visualizeVariables()$data
    date.time <- visualizeVariables()$date.time

    lon <- visualizeVariables()$lon
    lat <- visualizeVariables()$lat
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
        match_time   <- which(format(as.Date(station_data_compare()[, ti_n]), "%Y-%m") == format(as.Date(timestep_c()), "%Y-%m"), arr.ind = TRUE)
      } else {
        match_time   <- which(station_data_compare()[, ti_n] == timestep_c(), arr.ind = TRUE)
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
        
        data_sat[istation] <- data_nc[which(lon == lon_coor),which(lat == lat_coor), which(date.time == timestep_c())]
      }
      
      data_station_diff_absolute <- data_sat - data_station
      
      # relative diff plot data: ((datensatz1 - datensatz2)/datensatz2)*100
      data_station_diff_relative <- ((data_sat - data_station)/data_station)*100
      
      if(checkboxCompareData_dropdown() == "cmsaf.diff.absolute"){
        # max
        station_max <- max(data_station_diff_absolute, na.rm = TRUE)
        visualizeDataMax(station_max)
        # min
        station_min <- min(data_station_diff_absolute, na.rm = TRUE)
        visualizeDataMin(station_min)
      }
      
      if(checkboxCompareData_dropdown() == "cmsaf.diff.relative") {
        # max
        station_max <- max(data_station_diff_relative, na.rm = TRUE)
        visualizeDataMax(station_max)
        # min
        station_min <- min(data_station_diff_relative, na.rm = TRUE)
        visualizeDataMin(station_min)
      }
     
      cd <- data.frame(data_station_diff_absolute, data_station_diff_relative, data_sat, lon_station, lat_station)
      cd
    }
  })
  
  # Combine the CM SAF R Toolbox and R-Instat
  co.data <- reactive({
    # Only recompute if we want to add r_instat stuff to plot.
    req(instat_path())
    req(endsWith(instat_path(), ".RData") || endsWith(instat_path(), ".csv"))
    
    data_nc <- visualizeVariables()$data
    date.time <- visualizeVariables()$date.time
    lon <- visualizeVariables()$lon
    lat <- visualizeVariables()$lat
    
    # check row.names of data frame
    lo_dummy <- c("lon", "longitude", "laenge", "x", "lon_rep")
    la_dummy <- c("lat", "latitude", "breite", "y", "lat_rep")
    ti_dummy <- c("time", "date", "zeit", "t", "get_time.file_data.time_info.units..file_data.dimension_data.t.")
    da_dummy <- c("data", "daten", "z", "element", "result")

    dn <- attr(instat.data(), "element_name")
    if (!is.null(dn)) {
      da_dummy <- append(da_dummy, dn)
    } else {
      dn <- attr(instat.data(), "data_name")
      if (!is.null(dn)) {
        da_dummy <- append(da_dummy, dn)
      }
    }

    instat_names <- names(instat.data())

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
      time_station <- instat.data()[, ti_n]
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
      time_sat <- visualizeVariables()$date.time
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
        match_time   <- which(format(as.Date(instat.data()[, ti_n]), "%Y-%m") == format(as.Date(timestep_c()), "%Y-%m"), arr.ind = TRUE)
      } else {
        match_time   <- which(instat.data()[, ti_n] == timestep_c(), arr.ind = TRUE)
      }

      lon_station  <- instat.data()[, lo_n][match_time]
      lat_station  <- instat.data()[, la_n][match_time]
      data_station <- instat.data()[, da_n][match_time]

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
        
        data_sat[istation] <- data_nc[which(lon == lon_coor),which(lat == lat_coor), which(date.time == timestep_c())]
      }
      
      cd <- data.frame(data_station, data_sat, lon_station, lat_station)
      print(cd)
      cd
    } # end if lo_n,la_n,ti_n,da_n
  })

  # A function to validate all numeric inputs.
  # This should only be called knowing all required inputs exist!
  validNumericInputs <- reactive({
    validity <- list()
    # Set to false first
    validity$valid <- FALSE

    # Occurs either way
    if (!is.numeric(input$num_brk) || input$num_brk < 2 || input$num_brk > 64) {
      validity$argument <- "Number of colors"
      validity$value <- input$num_brk
      validity$required <- "in [2, 64]"
      return(validity)
    }

    if (input$slider1[1] == input$slider1[2]) {
      validity$argument <- "Longitude slider"
      validity$value <- "min = max"
      validity$required <- "min < max"
      return(validity)
    }

    if (input$slider2[1] == input$slider2[2]) {
      validity$argument <- "Latitude slider"
      validity$value <- "min = max"
      validity$required <- "min < max"
      return(validity)
    }

    if (input$proj == "rect") {
      # inputs only in rectangular

      if (!is.numeric(input$num_tick) || input$num_tick < 2 || input$num_tick > 64) {
        validity$argument <- "Number of ticks"
        validity$value <- input$num_tick
        validity$required <- "in [2, 64]"
        return(validity)
      }

      if (!is.numeric(input$num_rmin) || !is.numeric(input$num_rmax) || input$num_rmin >= input$num_rmax) {
        if (is.na(input$num_rmin) && is.na(input$num_rmax)) {
          validity$argument <- "Scale range"
          validity$value <- "Seems like you are trying to visualize data that consits of only 'NA' values. Please choose another file."
          validity$required <- "min < max"

        } else {
          validity$argument <- "Scale range"
          validity$value <- paste("min =", input$num_rmin, "max =", input$num_rmax)
          validity$required <- "min < max"

        }
        return(validity)
      }

      if (input$location) {
        if (!is.numeric(input$lon_loc) || abs(input$lon_loc) > 180) {
          validity$argument <- "Location"
          validity$value <- input$lon_loc
          validity$required <- "in [-180, 180]"
          return(validity)
        }

        if (!is.numeric(input$lat_loc) || abs(input$lat_loc) > 90) {
          validity$argument <- "Location"
          validity$value <- input$lat_loc
          validity$required <- "in [-180, 180]"
          return(validity)
        }
      }
    } else if (input$proj == "ortho") {
      # inputs only in orthoprojection
      if (!is.numeric(input$yort) || abs(input$yort) > 180) {
        validity$argument <- "Center Lon"
        validity$value <- input$yort
        validity$required <- "in [-180, 180]"
        return(validity)
      }

      if (!is.numeric(input$xort) || abs(input$xort) >= 90) {
        validity$argument <- "Center Lat"
        validity$value <- input$xort
        validity$required <- "in (-90, 90)"
        return(validity)
      }

      if (!is.numeric(input$rort) || abs(input$rort) >= 90) {
        validity$argument <- "Rotation"
        validity$value <- input$rort
        validity$required <- "in (-90, 90)"
        return(validity)
      }
    }

    # if not returned false yet, return true.
    validity$valid <- TRUE
    return(validity)
  })

  observeEvent(input$add_loc, {
    req(input$add_loc)
    req(abs(input$lon_loc) <= 180)
    req(abs(input$lat_loc) <= 90)
    if (!is.element(input$lon_loc, lon_loc_vec()) || !is.element(input$lat_loc, lat_loc_vec())) {
      lon_loc_vec(append(lon_loc_vec(), input$lon_loc))
      lat_loc_vec(append(lat_loc_vec(), input$lat_loc))
      name_loc_vec(append(name_loc_vec(), input$name_loc))
    }
  }, ignoreInit = TRUE)

  # Get new data if timestep is updated
  observeEvent(timestep_c(), {
    req(is.element(timestep_c(), visualizeVariables()$date.time))
    # Update the data according to the time step using the visualize nc file's id and previously calculated variable.
    if (!is.null(nc_object_visualize())) id <- nc_object_visualize()
    else id <- ncdf4::nc_open(nc_path_visualize())
    tmp <- getVariableData(which(visualizeVariables()$date.time == timestep_c()), id, visualizeVariables()$vn)
    if (is.null(nc_object_visualize())) ncdf4::nc_close(id)

    if (reversedDimensions$transpose) {
      tmp <- aperm(tmp, c(2, 1))
    }

    if (reversedDimensions$lonReverse) {
      tmp <- tmp[rev(seq_len(length(visualizeVariables()$lon))), ]
    }

    if (reversedDimensions$latReverse) {
      tmp <- tmp[, rev(seq_len(length(visualizeVariables()$lat)))]
    }
    visualizeDataTimestep(tmp)
  })

  ## Colorpalette Stuff ##
  palettes <- GetPaletteConfig(gui = TRUE)

  # ----------------------------------------------------------------
  # Getting currently selected color scheme
  # ----------------------------------------------------------------
  palettes <- GetPaletteConfig(gui = TRUE)
  names(palettes) <- tolower(names(palettes))
  names(palettes)[names(palettes) == "typ"] <- "type"

  # add more color schemes
  new_row <- data.frame("more", NA, NA, NA, NA, NA, NA, NA, NA, NA, 1)
  names(new_row) <- names(palettes)
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[79] <- "tim.colors"
  
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[80] <- "larry"
  
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[81] <- "albedo"
  
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[82] <- "albedo2"

  palettes <- rbind(palettes, new_row)
  rownames(palettes)[83] <- "sunny"

  x <- list()
  for (i in seq_len(nrow(palettes))) {
    x[[sprintf("%s", rownames(palettes)[i])]] <- rownames(palettes)[i]
  }
  updateSelectInput(session, "PAL", choices = x, selected = "sunny")

  # Debouncing
  db_xort  <- shiny::debounce(reactive({input$xort}),  750)
  db_yort  <- shiny::debounce(reactive({input$yort}),  750)
  db_rort  <- shiny::debounce(reactive({input$rort}),  750)
  db_num_tick <- shiny::debounce(reactive({input$num_tick}), 750)
  db_num_rmin <- shiny::debounce(reactive({input$num_rmin}), 750)
  db_num_rmax <- shiny::debounce(reactive({input$num_rmax}), 750)
  db_num_brk  <- shiny::debounce(reactive({input$num_brk}),  750)
  db_lat_lon_trigger <- shiny::debounce(reactive({lat_lon_trigger()}), 750)
  db_int        <- shiny::debounce(reactive({input$int}),    750)
  db_region     <- shiny::debounce(reactive({input$region}), 750)
  db_checkGroup <- shiny::debounce(reactive({input$checkGroup}), 750)
  db_visualizeDataTimestep <- shiny::debounce(reactive({visualizeDataTimestep()}), 750)
  db_visualizeDataMax <- shiny::debounce(reactive({visualizeDataMax()}), 750)
  db_proj <- shiny::debounce(reactive({input$proj}), 750)
  db_text2 <- shiny::debounce(reactive({input$text2}), 750)
  db_text3 <- shiny::debounce(reactive({input$text3}), 750)
  db_text1 <- shiny::debounce(reactive({input$text1}), 1000)
  
  db_text1_2 <- shiny::debounce(reactive({input$text1_2}), 750)
  db_text2_2 <- shiny::debounce(reactive({input$text2_2}), 750)

  getPlot_1d <- reactive({
    req(readyToPlot())
    if(nc_path_visualize_2() != ""){
      c(db_text1_1d())
      c(db_text2_1d())
      c(db_x_axis_label_1d())
      c(db_y_axis_label_1d())
      c(db_operatorInput_dropdown_station_number())
    } else {
      # Triggers and requirements
      req(is.character(db_text1_1d()))
      req(db_sliderx())
      req(db_slidery())
      req(db_integer())
      req(db_checkGroup_type())
      c(db_trend())
      req(db_ticknumber())
      c(db_text2_1d())
    }

    # Catch data is error exception
    if (class(visualizeVariables()$data) == "try-error") {
      showModal(modalDialog(
        h4("We can't handle your file at the moment. Please try another file."),
        br(),
        title = "Sorry!",
        size = "l"
      ))

      # Silently leave.
      req(FALSE)
    }
    shinyjs::hide("spinner_plot1")
    if(nc_path_visualize_2() != "") {
      if(checkboxCompareData_dropdown() == "cmsaf.scatter") {
        req(db_timestep_1d_visualize())
        isolate({
          res_plot <- try(cmsafvis::render_plot_scatter(fileExtension = ".png",
                                                        visualizeVariables = visualizeVariables(),
                                                        ticknumber = input$ticknumber,
                                                        dateformat = input$dateformat,
                                                        imagewidth = imagewidth(),
                                                        imageheight = imageheight(),
                                                        text1_1d = input$text1_1d,   # title
                                                        text2_1d = input$text2_1d,   # subtitle
                                                        textsize = textsize,
                                                        linesize = linesize,
                                                        x_axis_label_1d = input$x_axis_label_1d,
                                                        y_axis_label_1d = input$y_axis_label_1d,
                                                        timestep_1d_visualize = input$timestep_1d_visualize))
        })
      } else if(checkboxCompareData_dropdown() == "cmsaf.hist") {
        req(db_timestep_1d_visualize())
        # if(!(endsWith(infile2_analyze_value(), ".nc"))){
        #   req(!is.null(input$operatorInput_dropdown_station_number))
        # }
        isolate({
          res_plot <- try(cmsafvis::render_plot_hist_compare(fileExtension = ".png",
                                                        visualizeVariables = visualizeVariables(),
                                                        imagewidth = imagewidth(),
                                                        imageheight = imageheight(),
                                                        text1_1d = input$text1_1d,   # title
                                                        text2_1d = input$text2_1d,   # subtitle
                                                        textsize = textsize,
                                                        legend_label1 = input$x_axis_label_1d,
                                                        legend_label2 = input$y_axis_label_1d,
                                                        timestep_1d_visualize = input$timestep_1d_visualize
                                                        ))
        })
      } else if(checkboxCompareData_dropdown() == "cmsaf.time.series") {
        req((visualizeVariables()$plot_type == "cmsaf.time.series"))
        req(db_dateformat())
        if(!(endsWith(infile2_analyze_value(), ".nc"))){
          req(!is.null(input$operatorInput_dropdown_station_number))
        }
        isolate({
          res_plot <- try(cmsafvis::render_plot_time_series_compare(fileExtension = ".png",
                                                             visualizeVariables = visualizeVariables(),
                                                             ticknumber = input$ticknumber,
                                                             dateformat = input$dateformat,
                                                             sliderx = input$sliderx,
                                                             slidery = input$slidery,
                                                             checkGroup_type = input$checkGroup_type,   # line type
                                                             imagewidth = imagewidth(),
                                                             imageheight = imageheight(),
                                                             text1_1d = input$text1_1d,   # title
                                                             text2_1d = input$text2_1d,   # subtitle
                                                             textsize = textsize,
                                                             linesize = linesize,
                                                             col = input$integer,   # color
                                                             legend_label1 = input$x_axis_label_1d,
                                                             legend_label2 = input$y_axis_label_1d,
                                                             station_number = input$operatorInput_dropdown_station_number))
        })
      } else if(checkboxCompareData_dropdown() == "cmsaf.hovmoller") {
        req((visualizeVariables()$plot_type == "cmsaf.hovmoller"))
        isolate({
          res_plot <- try(cmsafvis::render_plot_hovmoller(fileExtension = ".png",
                                                                    visualizeVariables = visualizeVariables(),
                                                                    imagewidth = imagewidth(),
                                                                    imageheight = imageheight(),
                                                                    textsize = textsize,
                                                                    linesize = linesize,
                                                                    title_data1 = input$x_axis_label_1d,
                                                                    title_data2 = input$y_axis_label_1d))
        })
      } 
      else {
        req(FALSE)
      }
    }
    else if(length(visualizeVariables()$lon) == 1 && length(visualizeVariables()$lat) == 1){   # plot for time and variable
      c(db_analyze_timeseries())
      req(db_dateformat())
      isolate({
        res_plot <- try(cmsafvis::render_plot_1d(fileExtension = ".png",
                                                 visualizeVariables = visualizeVariables(),
                                                 ticknumber = input$ticknumber,
                                                 dateformat = input$dateformat,
                                                 analyze_timeseries = input$analyze_timeseries,
                                                 addTrend = input$trend,
                                                 sliderx = input$sliderx,
                                                 slidery = input$slidery,
                                                 checkGroup_type = input$checkGroup_type,   # line type
                                                 imagewidth = imagewidth(),
                                                 imageheight = imageheight(),
                                                 text1_1d = input$text1_1d,   # title
                                                 text2_1d = input$text2_1d,   # subtitle
                                                 textsize = textsize,
                                                 linesize = linesize,
                                                 col = input$integer))   # color
      })
    }
    else if((length(visualizeVariables()$lon) == 1 && length(visualizeVariables()$lat) != 1) ||
       (length(visualizeVariables()$lon) != 1 && length(visualizeVariables()$lat) == 1)){   # plot for lat/lon and variable
      req(db_timestep_1d_visualize())
      isolate({
        res_plot <- try(cmsafvis::render_plot_1d_advanced(fileExtension = ".png",
                                                 visualizeVariables = visualizeVariables(),
                                                 ticknumber = input$ticknumber,
                                                 addTrend = input$trend,
                                                 sliderx = input$sliderx,
                                                 slidery = input$slidery,
                                                 checkGroup_type = input$checkGroup_type,   # line type
                                                 imagewidth = imagewidth(),
                                                 imageheight = imageheight(),
                                                 text1_1d = input$text1_1d,   # title
                                                 text2_1d = input$text2_1d,   # subtitle
                                                 textsize = textsize,
                                                 linesize = linesize,
                                                 col = input$integer,   # color
                                                 timestep_1d_visualize = input$timestep_1d_visualize))
      })
    } else 
      req(FALSE)
    if (class(res_plot) != "try-error") {
      return(res_plot)
    } else {
      showModal(modalDialog(
        br(),
        h3("Something went wrong while creatin 1D Plot."),
        title = "Error.",
        size = "l"
      ))
      req(NULL)
    }
  })

  # A reactive, throttled function for generating the plot.
  getPlot_2d <- reactive({
    req(readyToPlot())

    # Required triggers
    req(nrow(db_visualizeDataTimestep()) > 0)  # when data at timestep changes
    req(nchar(db_text1()) > 0)                        # Need a title
    req(db_proj())                         # projection
    req(db_visualizeDataMax())             # max data (not sure why want to trigger this?)
    
    # Isolated requirements
    isolate(req(lon_bounds()))    # Require this to prevent error message
    isolate(req(lat_bounds()))    # However, do not trigger on change
    # isolate( req(imagewidth()) )    # image width (triggering is done by lat_lon_trigger)
    # isolate( req(imageheight()) )   # image height (triggering is done by lat_lon_trigger)
    
    # Non-required triggers (some are required but will be caught in vilidation of numeric inputs.)
    c(name_loc_vec(),    # new location
      db_checkGroup(),  # colorbar
      db_text2(),        # sub-title
      db_text3(),        # scale caption
      db_int(),         # country borders
      db_region(),      # for region plot
      db_xort(),         # center lat
      db_yort(),         # center lon
      db_rort(),         # rotation
      instat_path(),     # instat file path change
      db_num_tick(),     # number ticks
      db_num_rmin(),         # min val
      db_num_rmax(),         # max val
      db_num_brk(),          # number breaks
      db_lat_lon_trigger(),  # changes to lat, lon, bounds
      input$reverse,         # invert colors
      input$bordercolor2,     # border color for outlines
      input$PAL,             # colorspace pallete
      db_text1_2(),
      db_text2_2()
    )
    
    # Everything below this point is non-reactive.
    isolate({
      # First check validity in region plot
      if (input$plot_region) {
        if (is.null(region_data()) && (is.null(shapeFile_path()) || !file.exists(shapeFile_path()))) {
          # show message
          shinyjs::hide("spinner_plot1")
          shinyjs::enable("backToSetup")

          showModal(modalDialog(
            br(),
            h3("Please select a shape file ",
               tags$strong("(.shp)"),
               " or select the COUNTRY division to continue."),
            title = "Wrong file format.",
            size = "l"
          ))

          req(FALSE)
        }
        req(input$region != "Select region")
      }

      # Show spinner
      shinyjs::show("spinner_plot1", anim = TRUE, animType = "fade")
      shinyjs::disable("backToSetup")

      # Toggle state of plot_rinstat.
      plot_rinstat <- FALSE
      if (input$plot_rinstat) {
        if (!is.null(instat_path()) && file.exists(instat_path())) {
          plot_rinstat <- TRUE
        } else {
          # show message
          shinyjs::hide("spinner_plot1")
          shinyjs::enable("backToSetup")

          showModal(modalDialog(
            br(),
            h3("Please select a .RData instat file to continue."),
            title = "Wrong file format.",
            size = "l"
          ))

          # Leave silently.
          req(FALSE)
        }
      }

      # Validate numeric inputs
      validity <- validNumericInputs()
      if (validity$valid != TRUE) {
        # show message
        shinyjs::hide("spinner_plot1")
        shinyjs::enable("backToSetup")

        showModal(modalDialog(
          br(),
          h5(paste0("Argument: ", validity$argument)),
          h5(paste0("Value: ", validity$value)),
          h5(paste0("Expected: ", validity$required)),
          title = "Wrong input argument.",
          size = "l"
        ))

        # Leave silently.
        req(FALSE)
      }
      shinyjs::hide("spinner_plot1", anim = TRUE, animType = "fade")
      if (input$plot_region) {
        res <- try(ls <- cmsafvis::render_region_plot(infile = isolate(nc_path_visualize()),
                                                      visualizeVariables = visualizeVariables(),
                                                      visualizeDataMax = visualizeDataMax(),
                                                      lon_bounds = lon_bounds(),
                                                      lat_bounds = lat_bounds(),
                                                      lon_loc_vec = lon_loc_vec(),
                                                      lat_loc_vec = lat_loc_vec(),
                                                      name_loc_vec = name_loc_vec(),
                                                      division = input$division,
                                                      selectedRegion = input$region,
                                                      region_data = region_data(),
                                                      timestep = timestep_c(),
                                                      num_tick = input$num_tick,
                                                      num_rmin = input$num_rmin,
                                                      num_rmax = input$num_rmax,
                                                      location = input$location,
                                                      text1 = input$text1,
                                                      text2 = input$text2,
                                                      text3 = input$text3,
                                                      PAL = input$PAL,
                                                      palettes = palettes,
                                                      num_brk = input$num_brk,
                                                      reverse = input$reverse,
                                                      textsize = textsize,
                                                      bordercolor = input$bordercolor2,
                                                      plot_grid = plot_grid,
                                                      grid_col = grid_col,
                                                      image_def = image_def,
                                                      ihsf = ihsf(),
                                                      nc = nc_object_visualize()))
      } else {
        if(checkboxCompareData_dropdown() == "cmsaf.side.by.side"){
          req((visualizeVariables()$plot_type == "cmsaf.side.by.side"))
          isolate({
            res <- try(ls <- cmsafvis::render_plot_side_by_side(plot_rinstat = input$plot_rinstat,
                                                               visualizeVariables = visualizeVariables(),
                                                               visualizeDataTimestep = visualizeDataTimestep(),
                                                               nc_path_visualize = nc_path_visualize(),
                                                               visualizeDataMax = visualizeDataMax(),
                                                               timestep_2d = timestep_c(),
                                                               lon_bounds = lon_bounds(),
                                                               lat_bounds = lat_bounds(),
                                                               lon_loc_vec = lon_loc_vec(),
                                                               lat_loc_vec = lat_loc_vec(),
                                                               name_loc_vec = name_loc_vec(),
                                                               timestep = timestep_c(),
                                                               num_tick = input$num_tick,
                                                               num_rmin = input$num_rmin,
                                                               num_rmax = input$num_rmax,
                                                               num_brk = input$num_brk,
                                                               co.data = co.data(),
                                                               proj = input$proj,
                                                               imagewidth = imagewidth(),
                                                               imageheight = imageheight(),
                                                               xort = input$xort,
                                                               yort = input$yort,
                                                               rort = input$rort,
                                                               slider1 = input$slider1,
                                                               slider2 = input$slider2,
                                                               location = input$location,
                                                               text1 = input$text1,
                                                               text2 = input$text2,
                                                               text3 = input$text3,
                                                               int = input$int,
                                                               textsize = textsize,
                                                               bordercolor = input$bordercolor2,
                                                               linesize = linesize,
                                                               na.color = na.color,
                                                               PAL = input$PAL,
                                                               palettes = palettes,
                                                               reverse = input$reverse,
                                                               plot_grid = plot_grid,
                                                               grid_col = grid_col,
                                                               text1_2 = input$text1_2,
                                                               text2_2 = input$text2_2))
          })
        } else {
          res <- try(ls <- cmsafvis::render_plot(plot_rinstat = input$plot_rinstat,
                                                 visualizeVariables = visualizeVariables(),
                                                 visualizeDataTimestep = visualizeDataTimestep(),
                                                 nc_path_visualize = nc_path_visualize(),
                                                 visualizeDataMax = visualizeDataMax(),
                                                 lon_bounds = lon_bounds(),
                                                 lat_bounds = lat_bounds(),
                                                 lon_loc_vec = lon_loc_vec(),
                                                 lat_loc_vec = lat_loc_vec(),
                                                 name_loc_vec = name_loc_vec(),
                                                 timestep = timestep_c(),
                                                 num_tick = input$num_tick,
                                                 num_rmin = input$num_rmin,
                                                 num_rmax = input$num_rmax,
                                                 num_brk = input$num_brk,
                                                 co.data = co.data(),
                                                 co.data.compare.diff = co.data.compare.diff(),
                                                 proj = input$proj,
                                                 imagewidth = imagewidth(),
                                                 imageheight = imageheight(),
                                                 xort = input$xort,
                                                 yort = input$yort,
                                                 rort = input$rort,
                                                 slider1 = input$slider1,
                                                 slider2 = input$slider2,
                                                 location = input$location,
                                                 text1 = input$text1,
                                                 text2 = input$text2,
                                                 text3 = input$text3,
                                                 int = input$int,
                                                 textsize = textsize,
                                                 bordercolor = input$bordercolor2,
                                                 linesize = linesize,
                                                 na.color = na.color,
                                                 PAL = input$PAL,
                                                 palettes = palettes,
                                                 reverse = input$reverse,
                                                 plot_grid = plot_grid,
                                                 grid_col = grid_col))
        }
      }
    })
    if (class(res) == "try-error") {
      showModal(modalDialog(
        h4("An error occured while creating the plot. Please try another file or other input arguments."),
        tags$p(paste0("Message: ", res)),
        title = "Error!",
        size = "m"
      ))
      shinyjs::enable("backToSetup")
      shinyjs::hide("spinner_plot1")

      # Leave silently
      req(FALSE)
    } else {
      # Updating path to png plot
      png_path(ls$src)

      # show plot
      shinyjs::enable("backToSetup")
      shinyjs::hide("spinner_plot1")
      shinyjs::show("myImage", anim = TRUE, animType = "fade")

      # Return the image and don't delete it for now. It is stored in tmp directory anyway so it will be gone after the session.
      # We want to keep it so we can downlaod it later.
      ls
    }
  })

  # Rendering image 2d
  output$myImage_2d <- renderImage({
    getPlot_2d()
  }, deleteFile = FALSE)

  # Debounce 1D plot
  db_sliderx    <- shiny::debounce(reactive({input$sliderx}), 750)
  db_slidery    <- shiny::debounce(reactive({input$slidery}), 750)
  db_integer    <- shiny::debounce(reactive({input$integer}), 750)
  db_timestep_1d_visualize <- shiny::debounce(reactive({input$timestep_1d_visualize}), 750)
  db_trend      <- shiny::debounce(reactive({input$trend}), 750)
  db_checkGroup_type    <- shiny::debounce(reactive({input$checkGroup_type}), 750)
  db_analyze_timeseries <- shiny::debounce(reactive({input$analyze_timeseries}), 750)
  db_ticknumber <- shiny::debounce(reactive({input$ticknumber}), 750)
  db_dateformat <- shiny::debounce(reactive({input$dateformat}), 750)
  db_text2_1d   <- shiny::debounce(reactive({input$text2_1d}), 750)
  db_x_axis_label_1d <- shiny::debounce(reactive({input$x_axis_label_1d}), 750)
  db_y_axis_label_1d <- shiny::debounce(reactive({input$y_axis_label_1d}), 750)
  db_operatorInput_dropdown_station_number <- shiny::debounce(reactive({input$operatorInput_dropdown_station_number}), 750)

  # Again, this debounce is special. Compare with special debounce of plot 2d
  db_text1_1d   <- shiny::debounce(reactive({input$text1_1d}), 1000)

  # Copied from app-4
  output$myImage_1d <- renderImage({
    ls <- getPlot_1d()
    png_path(ls$src)
    ls
  },deleteFile = FALSE)

  # Creating a preview plot.
  output$previewSpatialCoveragePlot_vis <- renderPlot({
    req(input$slider1)
    req(input$slider2)
    maps::map("world", fill = TRUE, col = "gray36", bg = "white", xlim = input$slider1, ylim = input$slider2)
    maps::map("world", fill = TRUE, col = "gray36", bg = "white", xlim = input$slider1, ylim = input$slider2)
    title(main = "Zoom by selecting a region on this plot.")
  })

  # Observing changes in brushing
  observe({
    if (is.null(input$zoom_brush)) {
      lon_bounds(input$slider1)
      lat_bounds(input$slider2)
      ihsf(input$decimal)
    } else {
      brush <- input$zoom_brush
      lon <- c(brush$xmin, brush$xmax)
      lat <- c(brush$ymin, brush$ymax)
      lon_bounds(lon)
      lat_bounds(lat)
    }
  })

  # Download data for 1D-plot.
  output$downloadFile <- downloadHandler(
    filename = function() {
      return(paste0("data_", input$text1_1d, sessionName, ".csv"))
    },
    content = function(file) {
      # TODO: NOT SURE IF THIS WILL WORK IN REMOTE HOST. PLEASE TRY ON SHADOW!!
      removeModal()
      sep <- switch(as.numeric(input$separator), ";", ",", "\t")
      dataframe <- data.frame(visualizeVariables()$date.time, visualizeVariables()$data[,,])
      names(dataframe) <- c("dateTime", "data")
      utils::write.table(dataframe, file, row.names = FALSE, sep = sep)
    },
    contentType = "text/comma-separated-values"
  )

  output$downloadPlot <- downloadHandler(
    #filename:
    filename = function() {
      ext <- switch(as.numeric(input$imageformat), ".png", ".kml", ".tif", ".jpg", ".pdf")
      selected <- input$imageformat
      if (visualizeVariables()$plot_dim == 2) {
        return(paste0("plot_", input$text1, sessionName, ext))
      } else {
        return(paste0("plot_", input$text1_1d, sessionName, ext))
      }
    },
    content = function(file) {
      # TODO: NOT SURE IF THIS WILL WORK IN REMOTE HOST. PLEASE TRY ON SHADOW!!
      withProgress(message = "downloading File", value = 0,
                   {
                     removeModal()
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                     if (input$imageformat == 1) {
                       file.copy(png_path(), file)
                     } else {
                       fileExtension <- switch(as.numeric(input$imageformat), ".png", ".kml", ".tif", ".jpg", ".pdf")

                       if (input$plot_region) {
                         res_plot <- cmsafvis::render_region_plot(infile = nc_path_visualize(),
                                                                  fileExtension = fileExtension,
                                                                  visualizeVariables = visualizeVariables(),
                                                                  visualizeDataMax = visualizeDataMax(),
                                                                  lon_bounds = lon_bounds(),
                                                                  lat_bounds = lat_bounds(),
                                                                  lon_loc_vec = lon_loc_vec(),
                                                                  lat_loc_vec = lat_loc_vec(),
                                                                  name_loc_vec = name_loc_vec(),
                                                                  division = input$division,
                                                                  selectedRegion = input$region,
                                                                  region_data = region_data(),
                                                                  timestep = timestep_c(),
                                                                  num_tick = input$num_tick,
                                                                  num_rmin = input$num_rmin,
                                                                  num_rmax = input$num_rmax,
                                                                  location = input$location,
                                                                  text1 = input$text1,
                                                                  text2 = input$text2,
                                                                  text3 = input$text3,
                                                                  PAL = input$PAL,
                                                                  palettes = palettes,
                                                                  num_brk = input$num_brk,
                                                                  reverse = input$reverse,
                                                                  textsize = textsize,
                                                                  bordercolor = input$bordercolor2,
                                                                  plot_grid = plot_grid,
                                                                  grid_col = grid_col,
                                                                  image_def = image_def,
                                                                  ihsf = ihsf(),
                                                                  nc = nc_object_visualize())
                         in_plot <- res_plot$src
                       } else {
                         res_plot <- cmsafvis::render_plot(plot_rinstat = input$plot_rinstat,
                                                           fileExtension = fileExtension,
                                                           visualizeVariables = visualizeVariables(),
                                                           visualizeDataTimestep = visualizeDataTimestep(),
                                                           nc_path_visualize = nc_path_visualize(),
                                                           visualizeDataMax = visualizeDataMax(),
                                                           lon_bounds = lon_bounds(),
                                                           lat_bounds = lat_bounds(),
                                                           lon_loc_vec = lon_loc_vec(),
                                                           lat_loc_vec = lat_loc_vec(),
                                                           name_loc_vec = name_loc_vec(),
                                                           timestep = timestep_c(),
                                                           num_tick = input$num_tick,
                                                           num_rmin = input$num_rmin,
                                                           num_rmax = input$num_rmax,
                                                           num_brk = input$num_brk,
                                                           co.data = co.data(),
                                                           proj = input$proj,
                                                           imagewidth = imagewidth(),
                                                           imageheight = imageheight(),
                                                           xort = input$xort,
                                                           yort = input$yort,
                                                           rort = input$rort,
                                                           slider1 = input$slider1,
                                                           slider2 = input$slider2,
                                                           location = input$location,
                                                           text1 = input$text1,
                                                           text2 = input$text2,
                                                           text3 = input$text3,
                                                           int = input$int,
                                                           textsize = textsize,
                                                           bordercolor = input$bordercolor2,
                                                           linesize = linesize,
                                                           na.color = na.color,
                                                           PAL = input$PAL,
                                                           palettes = palettes,
                                                           reverse = input$reverse,
                                                           plot_grid = plot_grid,
                                                           grid_col = grid_col)
                         in_plot <- res_plot$src
                       }
                       file.copy(in_plot,file)
                     }
                   })
    },
    contentType = switch(as.numeric(input$imageformat), "image/png", "image/kml", "image/tif", "image/jpg", "image/pdf")
  )

  # Statsics output
  observe({
    # Requirements
    req(input$lonPoint)
    req(input$latPoint)
    req(timestep_c())
    req(visualizeVariables()$date.time)
    req(visualizeDataMax())
    req(visualizeVariables()$plot_dim == 2)

    # compute some statistics
    xr <- which(visualizeVariables()$lon >= (input$slider1[1]) & visualizeVariables()$lon <= (input$slider1[2]))
    yr <- which(visualizeVariables()$lat >= (input$slider2[1]) & visualizeVariables()$lat <= (input$slider2[2]))
    dastat <- visualizeDataTimestep()[xr, yr]
    # dastat <- visualizeVariables()$data[xr, yr, which(visualizeVariables()$date.time == input$timestep, arr.ind = TRUE)]

    dg <- 2
    if (abs(visualizeDataMax()) >= 10) (dg <- 1)
    if (abs(visualizeDataMax()) >= 100) (dg <- 0)

    da_mean   <- round(mean(dastat, na.rm = TRUE), digits = dg)
    da_median <- round(stats::median(dastat, na.rm = TRUE), digits = dg)
    da_sd     <- round(stats::sd(dastat, na.rm = TRUE), digits = dg)
    da_max    <- round(max(dastat, na.rm = TRUE), digits = dg)
    da_min    <- round(min(dastat, na.rm = TRUE), digits = dg)

    # some numbers
    output$statistics <- renderPrint({
      cat(paste0("Mean:               ", da_mean), "\n")
      cat(paste0("Median:             ", da_median), "\n")
      cat(paste0("Standard deviation: ", da_sd), "\n")
      cat(paste0("Maximum:            ", da_max), "\n")
      cat(paste0("Minimum:            ", da_min), "\n")
      cat(paste0("Unit:               ", visualizeVariables()$unit), "\n")
      cat("\n")
      cat(paste0("To save the histogram figure: right-click + save image as..."), "\n")
    })

    # Missing values can be found in global.R
    # histogram
    output$myHist <- renderPlot({
      cmsafvis::render_hist_plot(dastat = as.numeric(dastat),
                                 shortDescription = input$text1,
                                 xlab = input$text3,
                                 grid_col = grid_col,
                                 bordercolor = bordercolor,
                                 linesize = linesize)
    })
  })

  # Observing changes to instat file
  observeEvent({
    instat_path_action()
    input$plot_rinstat
  }, {
    # Requirements
    req(instat_path())
    if (input$plot_rinstat && file.exists(instat_path())) {
      # grid_col, bordercolor, and linesize can be found in global.R
      output$myComp <- renderPlot({
        cmsafvis::render_instat_plot(co.data = co.data(),
                                     shortDescription = input$text1,
                                     ylab = input$text3,
                                     grid_col = grid_col,
                                     bordercolor = bordercolor,
                                     linesize = linesize)
      })
    }
  })

  # File summaries
  output$summary1 <- renderPrint({
    req(nc_path_visualize())
    cmsafops::ncinfo(nc_path_visualize(), nc = nc_object_visualize())
  })

  output$summary2 <- renderPrint({
    req(nc_path_visualize())
    cmsafops::ncinfo(nc_path_visualize(), "m", nc = nc_object_visualize())
  })

  # About part
  output$about <- renderPrint({
    cat("The CMSAF Visualizer is part of the CM SAF R Toolbox.", "\n")
    cat("This tool helps you to visualize 1D-timeseries and 2D-maps.", "\n")
    cat("\n")
    cat("This version ('Just Read The Instructions') was tested with the cmsaf", "\n")
    cat("R-package in version 3.4.2.", "\n")
    cat("\n")
    cat("Suggestions for improvements and praise for the developers", "\n")
    cat("can be send to contact.cmsaf@dwd.de.", "\n")
    cat("\n")
    cat("                              - Steffen Kothe - 2022-03-16 -", "\n")
    cat("\n")
    cat("\n")
  })

  # Tipps
  output$tipps <- renderPrint({
    cat("You can easily plot station data, which were exported by R-Instat.", "\n")
    cat("\n")
    cat("If saving an image fails, try right-click plus 'save image as'.", "\n")
    cat("\n")
    cat("The orthographic projection includes some interpolation,", "\n")
    cat("which can have an impact on local pixel values!", "\n")
    cat("\n")
    cat("If you swim with a friend, your chances of getting eaten", "\n")
    cat("by a shark will drop by 50%.", "\n")
    cat("\n")
  })

  output$link <- renderUI({
    url <- a("https://www.cmsaf.eu/R_toolbox", href = "https://www.cmsaf.eu/R_toolbox")
    tagList("URL link:", url)
  })

  #### Destructors ####
  
  # This modal is called if the app is running remotely and there exists output files
  # to warn the user that files will be lost after exiting.
  confirm_exit_modal <- modalDialog(
    p("You have files on the server which will be lost when you exit."),
    downloadButton("downloader_modal", "Download the session files."),
    title = "Are you sure you want to exit?",
    footer = tagList(actionButton("exit_app", "Exit App"),
                     modalButton("Cancel")
    ),
    easyClose = TRUE
  )
  output$downloader_modal <- session_dir_download_handler
  
  # Stop app on exit button.
  observeEvent(input$exit, {
    n_remote_files(length(list.files(userDir, recursive = TRUE)))
    if (!isRunningLocally && n_remote_files() > 0) {
      showModal(confirm_exit_modal)
    }
    else {
      stop_toolbox()
    }
  })
  
  observeEvent(input$exit_app, {
    stop_toolbox()
  })
  
  stop_toolbox <- function() {
    stopApp(returnValue = invisible(99))
  }

  # After app is closed do cleanup. (Only if directory hasn't existed before session.)
  session$onSessionEnded(function() {
    # Variable 'isRunningLocally' can be found in global.R
    # Only removing in remote session. Keeping all files in local versions.
    if (!isRunningLocally) {
      if (dir.exists(userDir)) {
        unlink(userDir, recursive = TRUE)
      }
    } else {
      # If no files contained remove user directory. (local host)
      if (length(list.files(userDir, recursive = TRUE, include.dirs = TRUE)) == 0) {
        unlink(userDir, recursive = TRUE)
      }
    }
  })


  downloadModal1d <- function(failed = FALSE) {
    modalDialog(
      if(infile2_analyze_value() == "") {
        column(6,
               radioButtons("separator",
                            label = "Choose separator",
                            choices = list("Semicolon" = 1,
                                           "Comma" = 2,
                                           "Tab" = 3),
                            selected = 1),
               downloadButton("downloadFile", "Download Data")
        )
      },
    column(6,
           selectInput("imageformat",
                       label = "File format",
                       choices = list("PNG" = 1,
                                      "jpeg" = 4,
                                      "pdf" = 5
                       ),
                       selected = 1),
           downloadButton("downloadPlot", "Download Image")
    ),
    footer = tagList(
      modalButton("Cancel")
    )

    )
  }
  downloadModal2d <- function(failed = FALSE) {
    modalDialog(
      selectInput("imageformat",
                  label = "File format",
                  if (input$proj == "ortho") {
                    choices <- list("PNG" = 1,
                                   "jpeg" = 4,
                                   "pdf" = 5
                    )
                  } else {
                    choices <- list("PNG" = 1,
                                   "KML" = 2,
                                   "GeoTiff" = 3,
                                   "jpeg" = 4,
                                   "pdf" = 5
                    )
                  },
                  selected = 1),
      downloadButton("downloadPlot", "download Image"),
      footer = tagList(
        modalButton("Cancel")
      )

    )
  }

  # Downloader for monitor_climate
  output$download_monitor_climate <- renderUI({
    downloadButton(
      outputId = "download_monitor_climate_output",
      label = cmsafops::get_basename(image_path_visualize()),
      style = "width:100%;")
  })

  output$download_monitor_climate_output <- downloadHandler(
    filename = function() {
      paste0(cmsafops::get_basename(image_path_visualize()))
    },
    content = function(con) {
      file.copy(image_path_visualize(), con)
    }
  )

  # Show modal when button is clicked.
  observeEvent(input$showModal, {
    if (visualizeVariables()$plot_dim == 1) {
      showModal(downloadModal1d())
    } else {
      showModal(downloadModal2d())
    }
  })
}
