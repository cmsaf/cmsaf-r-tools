# the CM SAF R Toolbox.
#
# You should not use this R-script on its own!
#
# Have fun with the CM SAF R TOOLBOX!
#                                              (Steffen Kothe / CM SAF 2022-03-15)
#__________________________________________________________________________________

descriptionString <-
  "

The CM SAF R TOOLBOX 3.4.2 -- 'Just Read the Instructions'

The intention of the CM SAF R Toolbox is to help you using
CM SAF NetCDF formatted climate data

This includes:
  1. Preparation of data files.
  2. Analysis and manipulation of prepared data.
  3. Visualization of the results.

To begin, choose a .tar file or a .nc file in the prepare section or jump
right in and analyze or visualize a .nc file.

Suggestions for improvements and praise for the developers
can be sent to contact.cmsaf@dwd.de.

- Steffen Kothe - 2022-03-16 -"

# Variable can be found in global.R
if (isRunningLocally) {
  analyzeString <-
    "<h2>Analyze</h2>
  <p>Please select a NetCDF file <strong>(.nc)</strong> to start the analysis process.</p>
  <p>This is the second step after you prepared your data.<p/>
  <p>The input for this application has to be a NetCDF file</p>
  <p>(usually the output of step one (Prepare)).</p>
  <br>
  <p>This application will help you to analyze and manipulate your data.</p>
  <p>The output is usually written in a NetCDF file in the according output folder.</p>"
} else {
  analyzeString <-
    "<h2>Analyze</h2>
  <p>Please select a NetCDF file <strong>(.nc)</strong> to start the analysis process.</p>
  <p>This is the second step after you prepared your data.<p/>
  <p>The input for this application has to be a NetCDF file</p>
  <p>(usually the output of step one (Prepare)).</p>
  <br>
  <p>This application will help you to analyze and manipulate your data.</p>
  <p>Again, make sure to download your session files before closing the application.</p>"
}

visualizeString <-
  "<h2>Visualize</h2>
<p>Please select a NetCDF file <strong>(.nc)</strong> to start the visualization.</p>
<br>
<p>This application can be used to display NetCDF data.</p>
<p>In addition, it provides information on the data and the NetCDF file.</p>"

# render string for colorspace
renderString <-
  '{
    option: function(item, escape) {
        // custom code to generate HTML before each option
        var tmp = item.value
        var imgname = tmp.toLowerCase().split(" ").join("_");
        imgname = "images/pal_"+imgname+".png";
        return( "<img class=\'select-pal\' src=\'"+imgname+"\'></img>" )
    }
}'

months_list <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tags$script(HTML(
    "var actionValue = 0;",
    "$(document).on('click', '.homeimg', function() {",
    "  actionValue = actionValue + 1;",
    "  Shiny.setInputValue('homeimg', actionValue);",
    "});"
  )),
  shinyjs::useShinyjs(),
  #### Setup Page ####
  tags$div(
    id = "setupPage",
    sidebarLayout(
      sidebarPanel(img(src = "R_Toolbox_Logo.png", class = "homeimg", width = "100%;", style = "cursor: pointer; padding: 10px;"),
                   actionButton("prepare", label = "Prepare", class = "btn btn-primary btn-lg btn-block",
                                style = "font-size: 30px"),
                   actionButton("analyze", label = "Analyze", class = "btn btn-primary btn-lg btn-block",
                                style = "font-size: 30px"),
                   actionButton("visualize", label = "Visualize", class = "btn btn-primary btn-lg btn-block",
                                style = "font-size: 30px"),
                   actionButton("exit", label = "EXIT", class = "btn btn-danger btn-lg btn-block",
                                style = "font-size: 30px"),
                   shinyjs::hidden(tags$div(id = "downloader",
                                            downloadButton("download",
                                                           "Download the session files."))),
                   shinyjs::hidden(actionButton("modify_userDir",
                                                "View or edit the user directory."))),

      # Description panel.
      mainPanel(tags$div(id = "panel_home",
                         tags$pre(descriptionString)),
                tags$div(id = "panel_content",
                         #### Preparation Stuff ####
                         shinyjs::hidden(
                           tags$div(id = "panel_prepareGo",
                                    uiOutput("prepareString"),
                                    br(),
                                    tags$div(id = "ncFileWrapper",
                                            shinyjs::hidden(actionButton("tarFileLocal",
                                                                         label = "Choose a .tar-file...")),
                                            shinyjs::hidden(shinyFiles::shinyFilesButton(
                                                                                         id = "tarFileRemote",
                                                                                         label = "Choose a .tar-file...",
                                                                                         multiple = FALSE,
                                                                                         title = "Please select a .tar-file.")),
                                            shinyjs::hidden(span(id = "or_prepare", "or", class = "or")),
                                            shinyjs::hidden(actionButton("ncFileLocal",
                                                                         label = "Choose .nc-files...")),
                                            shinyjs::hidden(shinyFiles::shinyFilesButton(
                                                                                         id = "ncFileRemote",
                                                                                         label = "Choose .nc-files...",
                                                                                         multiple = FALSE,
                                                                                         title = "Please select .nc-files.")),
                                            shinyjs::hidden(span(id = "or_prepare2", "or", class = "or")),
                                            shinyjs::hidden(actionButton("ncURL",
                                                                         label = "Enter .nc file URL..."))
                                            )
                                    )),
                                   
                         shinyjs::hidden(
                           tags$div(id = "panel_prepareInput1",
                                    uiOutput("dateRange_ui"),
                                    shinyjs::disabled(actionButton("untarAndUnzip",
                                                                   "Untar and unzip files.")),
                                    )),
                         shinyjs::hidden(
                           tags$div(id = "panel_prepare_nc_url",
                                    textInput("nc_url_text",
                                              label = "Please enter a URL to a NetCDF (.nc) file",
                                              placeholder = "Enter URL...",
                                              width = "100%"),
                                    shinyjs::disabled(actionButton("nc_url_connect", "Connect to URL")),
                                    tags$br(),
                                    tags$br(),
                                    htmlOutput("nc_url_valid_message"),
                                    shinyjs::hidden(
                                      tags$div(id = "nc_url_file_info",
                                               h3("Short File Information"),
                                               verbatimTextOutput("ncurlShortInfo"))
                                      ),
                                    shinyjs::hidden(actionButton("nc_url_download", "")),
                                    shinyjs::hidden(actionButton("nc_url_subset", "Subset this file")),
                                    shinyjs::hidden(span(id = "or_prepare3", "or", class = "or")),
                                    shinyjs::hidden(actionButton("nc_url_analyze", "Analyze this file")),
                                    shinyjs::hidden(span(id = "or_prepare4", "or", class = "or")),
                                    shinyjs::hidden(actionButton("nc_url_visualize", "Visualize this file")),
                                    tags$br(),
                                    tags$br(),
                                    shinyjs::hidden(
                                      tags$div(id = "nc_url_download_analyze_or_visualise",
                                               actionButton("nc_url_download_analyze", "Analyze this file"),
                                               span(id = "or_prepare5", "or", class = "or"),
                                               actionButton("nc_url_download_visualize", "Visualize this file"),
                                               )
                                      ),
                                    shinyjs::hidden(tags$div(id = "spinner_prepare_nc_url_connect",
                                                             class = "spinner",
                                                             tags$div(class = "spinner-title", h4("Connecting to URL...")),
                                                             tags$div(class = "double-bounce1"),
                                                             tags$div(class = "double-bounce2"))),
                                    shinyjs::hidden(tags$div(id = "spinner_prepare_nc_url_download",
                                                             class = "spinner",
                                                             tags$div(class = "spinner-title", h4("Downloading NetCDF (.nc) file...")),
                                                             tags$div(class = "double-bounce1"),
                                                             tags$div(class = "double-bounce2")))
                           )),
                         # NetCDF Subset Selection (NCSS) URL options
                         shinyjs::hidden(
                           tags$div(id = "panel_prepare_ncss_url_subset",
                                    htmlOutput("ncss_url_print"),
                                    fluidRow(column(5, 
                                                    uiOutput("ncss_var_list_ui"),
                                                    uiOutput("ncss_select_region_ui"),
                                                    radioButtons("ncss_time_type",
                                                                 "Time Selection",
                                                                 choices = c("Date Range" = "date_range",
                                                                             "Extract Months" = "extract_months"),
                                                                 inline = TRUE),
                                                    shinyjs::hidden(uiOutput("ncss_date_range_ui")),
                                                    shinyjs::hidden(
                                                      tags$div(id = "ncss_month_range",
                                                               tags$div(tags$b("Please select a month range")),
                                                               tags$div(style = "display:inline-block", selectInput("ncss_month_from", "From", months_list, selected = months_list[1], width = "130px")),
                                                               tags$div(style = "display:inline-block", selectInput("ncss_month_to", "To", months_list, selected = months_list[12], width = "130px"))
                                                      )
                                                    ),
                                                    shinyjs::hidden(uiOutput("ncss_year_range_ui")),
                                                    actionButton("ncss_subset_download", ""),
                                                    shinyjs::hidden(tags$div(id = "spinner_prepare_ncss_download",
                                                                             class = "spinner",
                                                                             tags$div(class = "spinner-title", h4("Downloading NetCDF (.nc) file...")),
                                                                             tags$div(class = "double-bounce1"),
                                                                             tags$div(class = "double-bounce2")))
                                    ),
                                             column(7,
                                                    plotOutput("preview_ncss_subset")
                                                    )
                                             )
                                    )),
                         # Downloaded NetCDF Subset Selection (NCSS) URL info
                         shinyjs::hidden(
                           tags$div(id = "panel_prepare_ncss_download_info",
                             tags$div(id = "ncss_file_info",
                                      h3("File Successfully Downloaded"),
                                      verbatimTextOutput("ncssShortInfo")),
                             tags$div(id = "ncss_download_analyze_or_visualise",
                                      actionButton("ncss_download_analyze", "Analyze this file"),
                                      span(id = "or_prepare6", "or", class = "or"),
                                      actionButton("ncss_download_visualize", "Visualise this file"),
                                      ),
                             tags$br(),
                             tags$br(),
                             actionButton("ncss_info_back", "Back to Selection", class = "exit-button")
                           )
                         ),
                         # date range .nc-files selection
                         shinyjs::hidden(
                           tags$div(id = "panel_prepareInput1Nc",
                                    uiOutput("variable_ui_nc"),
                                    uiOutput("dateRange_ui_nc"),
                                    checkboxInput("justmerge", 
                                                  "Just merge (no checks, no adaptations, ignore date range)",
                                                  value = FALSE),
                                    shinyjs::disabled(actionButton("applyDateRange",
                                                                   "Apply")),
                           )),
                         # Panel after untaring and unzipping.
                         # First a few spinner classes.
                         shinyjs::hidden(tags$div(id = "spinner_prepare1",
                                                  class = "spinner",
                                                  tags$div(class = "spinner-title", h4("Extracting date range...")),
                                                  tags$div(class = "double-bounce1"),
                                                  tags$div(class = "double-bounce2"))),
                         shinyjs::hidden(tags$div(id = "spinner_prepare2",
                                                  class = "spinner",
                                                  tags$div(class = "spinner-title", h4("Untaring files...")),
                                                  tags$div(class = "double-bounce1"),
                                                  tags$div(class = "double-bounce2"))),
                         shinyjs::hidden(tags$div(id = "spinner_prepare3",
                                                  class = "spinner",
                                                  tags$div(class = "spinner-title", h4("Creating your output file...")),
                                                  tags$div(class = "double-bounce1"),
                                                  tags$div(class = "double-bounce2"))),
                         shinyjs::hidden(tags$div(id = "spinner_prepare4",
                                                  class = "spinner",
                                                  tags$div(class = "spinner-title", h4("Applying...")),
                                                  tags$div(class = "double-bounce1"),
                                                  tags$div(class = "double-bounce2"))),
                         shinyjs::hidden(selectInput(inputId = "aux_select",
                                                     label = "File does not contain lon/lat information. Please provide an auxiliary file.",
                                                     choices = c("Choose an option" = "",
                                                                 "Provide local file" = "local",
                                                                 "Download file" = "download",
                                                                 "Cancel" = "cancel"))),

                         # Then the actual classes.
                         shinyjs::hidden(
                           tags$div(id = "panel_prepareInput2",
                                    fluidRow(column(5,
                                                    uiOutput("variable_ui"),
                                                    uiOutput("lonRange_ui"),
                                                    uiOutput("latRange_ui"),
                                                    shinyjs::hidden(uiOutput("level_ui")),
                                                    checkboxInput("checkboxInput_aggregate", 
                                                                  "Do you want to aggregate the data before analysis?",
                                                                  value = FALSE),
                                                    shinyjs::hidden(selectInput("operatorGroupsPrepare",
                                                                                label = "Select an operator for aggregation ",
                                                                                choices = operatorGroupsPrepare,
                                                                                width = "300px")),
                                                    shinyjs::hidden(tags$div(id = "timeRange_prepare",
                                                                             numericInput("timeRange_nts_prepare",
                                                                                          "Please enter a number of time steps",
                                                                                          value = 1,
                                                                                          min = 0,
                                                                                          max = 1000000,
                                                                                          step = 1,
                                                                                          width = "300px"))),
                                                    selectInput("outputFormat",
                                                                "Select output format",
                                                                choices = c("NetCDF4" = 4, "NetCDF3" = 3),
                                                                selected = "NetCDF4"),
                                                    shinyjs::hidden(checkboxInput("deleteExtracted",
                                                                  "Delete the extracted files after the output has been created? (Recommended)",
                                                                  value = TRUE)),
                                                    actionButton("createOutput",
                                                                 "Create output file!")),
                                             column(7,
                                                    plotOutput("previewSpatialCoveragePlot"))))),

                         #### Analyze Stuff ####
                         shinyjs::hidden(
                           tags$div(id = "panel_analyzeGo",
                                    HTML(analyzeString),
                                    shinyjs::hidden(uiOutput("ncFile_analyze")),
                                    tags$div(id = "ncFileWrapper",
                                             shinyjs::hidden(actionButton("useOutputFile_analyze",
                                                                          label = "Analyze this file!")),
                                             shinyjs::hidden(h5(id = "or_analyze", "or")),
                                             shinyjs::hidden(actionButton("ncFileLocal_analyze",
                                                                          label = "Choose a .nc-file...")),
                                             shinyjs::hidden(shinyFiles::shinyFilesButton(
                                                              id = "ncFileRemote_analyze",
                                                              label = "Choose a file...",
                                                              multiple = FALSE,
                                                              title = "Please select a .nc-file."))))),
                         shinyjs::hidden(
                           tags$div(id = "panel_analyze",
                                    fluidRow(
                                      column(width = 5,
                                             uiOutput("usedVariable"),
                                             # Operator groups and operators can be found in global.R
                                             selectInput("operatorGroup",
                                                         label = "Select a group of operators",
                                                         choices = operatorGroups,
                                                         width = "320px"),
                                             uiOutput("operator"),
                                             # Operator options
                                             # shinyjs::hidden(selectInput("monitorClimateAnalyzeMethod",
                                             #                             label = "Select an analyze method ",
                                             #                             choices = c("accumulate", "mean"),
                                             #                             width = "320px")),
                                             shinyjs::hidden(checkboxInput("monitorClimateAnalyzeMethod",
                                                                           "Do you want to accumulate the infile over time?",
                                                                           value = TRUE)),
                                             shinyjs::hidden(numericInput("analyzeTimeSize",
                                                                       label = "Select time range",
                                                                       min = 1,
                                                                       max = 12,
                                                                       value = 1)),                                             
                                             shinyjs::hidden(checkboxInput("accumulateInfile",
                                                                           "Do you want to accumulate the infile over time?",
                                                                           value = TRUE)),																		   
											                       shinyjs::hidden(selectInput("stripecol", label = "Select color", 
																		                      choices = list("Blue-Red" = 1, "Grey-Yellow" = 2, "Brown-Green" = 3),
																		                      selected = 1)),
											                       shinyjs::hidden(checkboxInput("circular", label = "Circular Plot", value = FALSE)),
											                       shinyjs::hidden(checkboxInput("absrel", label = "Relative Values", value = FALSE)),

                                             
                                             shinyjs::hidden(checkboxInput("attachToExisting",
                                                                           "Do you want to attach the data to that of an already existing file?"
                                                                           )),
                                             shinyjs::hidden(tags$div(id = "attach_warning",
                                                                      "Warning: Only use this option if you are sure that the spatial and temporal coverage are correct."
                                                                      )),
                                             shinyjs::hidden(numericInput("constant",
                                                                          "Enter a number",
                                                                          value = 1,
                                                                          width = "320px")),
											 shinyjs::hidden(numericInput("threshold",
                                                                          "Enter a number",
                                                                          value = 0,
                                                                          width = "320px")),
                                             shinyjs::hidden(uiOutput("select_country")),
                                             shinyjs::hidden(uiOutput("region_to_select")),
                                             shinyjs::hidden(tags$div(id = "point",
                                                                      numericInput("latPoint",
                                                                                   "Select latitude point",
                                                                                   value = 0),
                                                                      numericInput("lonPoint",
                                                                                   "Select longitude point",
                                                                                   value = 0))),
                                             shinyjs::hidden(tableOutput("chosen_points")),
                                             shinyjs::hidden(actionButton("add_point", "Add Point")),
                                             shinyjs::hidden(uiOutput("multi_warning")),
                                             shinyjs::hidden(uiOutput("points_to_select")),
                                             shinyjs::hidden(uiOutput("dateRange_to_select")),
                                             shinyjs::hidden(uiOutput("climatology_years")),
                                             shinyjs::hidden(checkboxInput("useFastTrend",
                                                                           "Do you want to use fast computation (memory consuming)",
                                                                           value = TRUE)),
                                             shinyjs::hidden(numericInput("percentile",
                                                                          "Please enter a percentile number",
                                                                          value = 0.95,
                                                                          min = 0,
                                                                          max = 1,
                                                                          step = 0.05,
                                                                          width = "320px")),
                                             shinyjs::hidden(tags$div(id = "gridbox",
                                                                      numericInput("gridbox_lat",
                                                                                   "Enter latitude index (integer)",
                                                                                   value = 1,
                                                                                   min = 0,
                                                                                   max = 1000000,
                                                                                   step = 1,
                                                                                   width = "320px"),
                                                                      numericInput("gridbox_lon",
                                                                                   "Enter longitude index (integer)",
                                                                                   value = 1,
                                                                                   min = 0,
                                                                                   max = 1000000,
                                                                                   step = 1,
                                                                                   width = "320px"))),
                                             shinyjs::hidden(tags$div(id = "running",
                                                                      numericInput("running_nts",
                                                                                   "Please enter a number of time steps",
                                                                                   value = 1,
                                                                                   min = 0,
                                                                                   max = 1000000,
                                                                                   step = 1,
                                                                                   width = "320px"))),
                                             shinyjs::hidden(tags$div(id = "timeRange",
                                                                      numericInput("timeRange_nts",
                                                                                   "Please enter a number of time steps",
                                                                                   value = 1,
                                                                                   min = 0,
                                                                                   max = 1000000,
                                                                                   step = 1,
                                                                                   width = "320px"))),
                                             shinyjs::hidden(checkboxGroupInput("months",
                                                                                "Please select months",
                                                                                c("January", "February", "March", "April", "May", "June",
                                                                                  "July", "August", "September", "October", "November", "December"),
                                                                                width = "320px")),
                                             shinyjs::hidden(uiOutput("years_to_select")),
                                             shinyjs::hidden(uiOutput("times_to_select")),
                                             shinyjs::hidden(selectInput("method",
                                                                         "Select regridding method",
                                                                         choices = c("Nearest Neighbour interpolation" = "nearest",
                                                                                     "Bilinear Interpolation" = "bilinear",
                                                                                     "Conservative remapping" = "conservative"),
                                                                         width = "320px")),
                                             shinyjs::hidden(tags$div(id = "twofiles",
                                                                      shinyFiles::shinyFilesButton("ncSecondFileRemote",
                                                                                                   "Choose second file",
                                                                                                   "The chosen operator requires a second nc infile.",
                                                                                                   FALSE),
                                                                      verbatimTextOutput("secondFile"))),
                                             shinyjs::hidden(selectInput("plot_format",
                                                                        "Select plot format",
                                                                        choices = c("graphic", "animation"),
                                                                        width = "320px")),
                                             # shinyjs::hidden(tags$div(id = "multiDayNonAccuGraphic",
                                             #                          "Creating a multi day graphic of non accumulated/averaged data is not possible. Please choose to accumulate/mean the infile, or plot format 'animation', or select a single day in the date range")),
                                             conditionalPanel(condition = "input.plot_format == 'animation'",
                                                              numericInput("animation_pace",
                                                                           "Select animation pace",
                                                                           value = 0.07,
                                                                           min = 0.01,
                                                                           max = 100000,
                                                                           step = 0.1)),
                                             shinyjs::hidden(tags$div(id = "file_selection",
                                                                      actionButton("file_selection_button",
                                                                          label = "Choose a second file..."))),
                                             shinyjs::hidden(uiOutput("ncFile_analyze_second_file")),
                                             shinyjs::hidden(uiOutput("multi_warning_adjust_two_files")),
                                             shinyjs::hidden(uiOutput("date_range_compare_data")),
                                             selectInput("format",
                                                         "Select output format",
                                                         choices = c("NetCDF4" = 4, "NetCDF3" = 3),
                                                         width = "320px"),
                                             tags$div(
                                               id = "applyAnotherOrVisualize",
                                               checkboxInput("applyAnother",
                                                             "Do you want to apply another operator afterwards?"),
                                               checkboxInput("instantlyVisualize",
                                                             "Do you want to visualize the results right away?",
                                                             value = TRUE)),
                                             shinyjs::hidden(
                                               tags$div(
                                                 id = "ClimateAnalysisMessage",
                                                 "Climate Analysis operators will result in instant visualization.")
                                             ),
                                             h5("Hint: You can start with a new input file by clicking on 'Analyze'."),
                                             actionButton("applyOperator",
                                                          label = "Apply operator",
                                                          class = "btn btn-success btn-lg btn-block",
                                                          style = "font-size: 30px"),
                                             h5("If you would like to have more functions included contact:"),
                                             h5("training.cmsaf@dwd.de"),
                                             br()
                                      ),
                                      column(width = 7,
                                             tags$div(id = "nc_info",
                                                      h3("Short File Information"),
                                                      verbatimTextOutput("ncShortInfo")),
											                       tags$div(id = "og_info",
                                                      h4("Operator Group Info"),
                                                      verbatimTextOutput("ogInfo")),
                                             shinyjs::hidden(tags$div(id = "listOfOperators",
                                                                      h3("List of applied operators"),
                                                                      tableOutput("appliedOperators"))),
                                             tags$div(id = "wrapper_analyze_spinner",
                                                      shinyjs::hidden(tags$div(id = "spinner_analyze",
                                                                               class = "spinner",
                                                                               tags$div(class = "spinner-title", h4("Applying the given operation...")),
                                                                               tags$div(class = "double-bounce1"),
                                                                               tags$div(class = "double-bounce2")))))))),

                         #### Visualize Stuff ####
                         shinyjs::hidden(
                           tags$div(id = "panel_visualizeGo",
                                    HTML(visualizeString),
                                    shinyjs::hidden(uiOutput("ncFile_visualize")),
                                    tags$div(id = "ncFileWrapper",
                                             shinyjs::hidden(actionButton("useOutputFile_visualize",
                                                                          label = "Visualize this file!")),
                                             shinyjs::hidden(h5(id = "or_visualize", "or")),
                                             shinyjs::hidden(actionButton("ncFileLocal_visualize",
                                                                          label = "Choose a file...")),
                                             shinyjs::hidden(shinyFiles::shinyFilesButton(
                                                              id = "ncFileRemote_visualize",
                                                              label = "Choose a file...",
                                                              multiple = FALSE,
                                                              title = "Please select a .nc-file.")))),
                           shinyjs::hidden(tags$div(id = "spinner_visualize",
                                                    class = "spinner",
                                                    tags$div(class = "spinner-title", h4("Preparing your plot...")),
                                                    tags$div(class = "double-bounce1"),
                                                    tags$div(class = "double-bounce2")))))))),

  #### VISUALIZE PAGE ####
  shinyjs::hidden(tags$div(id = "spinner_visualize_compare_data",
                           class = "spinner",
                           tags$div(class = "spinner-title", h4("Preparing your plot...")),
                           tags$div(class = "double-bounce1"),
                           tags$div(class = "double-bounce2"))),
  
  shinyjs::hidden(tags$div(
    id = "visualizePage",

    sidebarLayout(
      sidebarPanel(img(src = "R_Toolbox_Logo.png", width = "100%;", style = "padding: 10px;"),
                   h3("Visualizer Options:"),
                   shinyjs::hidden(uiOutput("dropdown_compare_data")),
                   shinyjs::hidden(uiOutput("dropdown_station_number")),
                   tags$div(id = "sidebar_2d_plot",
                            uiOutput("timestep_visualize"),
                            conditionalPanel(condition = "input.proj == 'rect'",
                                             checkboxInput("plot_region", "Plot region"),
                                             conditionalPanel(condition = "input.plot_region",
                                                              # All countries can be found in global.R
                                                              fluidRow(column(6, uiOutput("division_options")),
                                                                       column(6, uiOutput("region_options"))),
                                                              shinyjs::hidden(actionButton("shapefileLocal",
                                                                                           label = "Choose a shape file...")),
                                                              shinyjs::hidden(shinyFiles::shinyFilesButton(
                                                                                        id = "shapefileRemote",
                                                                                        label = "Choose a shape file...",
                                                                                        multiple = FALSE,
                                                                                        title = "Please select a .shp-file."))),
                                             conditionalPanel(condition = "input.plot_region",
                                                              br()),
                                             conditionalPanel(condition = "!input.plot_region",
                                                              checkboxInput("show_zoom", "Show Zoom"),
                                                              uiOutput("lon_visualize"),
                                                              uiOutput("lat_visualize"),
                                                              uiOutput("ihsf_visualize"))),
                            conditionalPanel(condition = "input.proj == 'ortho'",
                                             fluidRow(column(4,
                                                             numericInput("yort",
                                                                          label = "Center Lon",
                                                                          value = 0,
                                                                          step = 5,
                                                                          min = -175,
                                                                          max = 175)),
                                                      column(4,
                                                             numericInput("xort",
                                                                          label = "Center Lat",
                                                                          value = 0,
                                                                          step = 5,
                                                                          min = -85,
                                                                          max = 85)),
                                                      column(4,
                                                             numericInput("rort",
                                                                          label = "Rotation",
                                                                          value = 0,
                                                                          step = 5,
                                                                          min = -90,
                                                                          max = 90)))),
                            fluidRow(column(6,
                                            numericInput("num_brk",
                                                         label = "Number of Colors",
                                                         value = 32,
                                                         min = 2,
                                                         max = 128,
                                                         step = 1)),
                                    column(6,
                                            conditionalPanel(condition = "input.proj == 'rect' && !input.plot_region",
                                             numericInput("num_tick",
                                                          label = "Number of Ticks",
                                                          value = 5,
                                                          min = 2,
                                                          max = 64,
                                                          step = 1)))),
                                             selectizeInput("PAL",
                                                            label = "Colorbar",
                                                            choices = list(),
                                                            options = list(create = TRUE,
                                                                           render = I(renderString))),
                                             checkboxInput("reverse", "Invert Colors", value = FALSE, width = NULL),
                                             colourpicker::colourInput("bordercolor2",
                                                      "Border Color",
                                                      showColour = "background",
                                                      palette = "limited",
                                                      returnName = TRUE,
                                                      value = "gray20"),
                            conditionalPanel(condition = "input.proj == 'rect' || input.plot_region",
                                             fluidRow(column(6,
                                                             shinyjs::hidden(uiOutput("num_rmin"))),
                                                      column(6,
                                                             shinyjs::hidden(uiOutput("num_rmax"))))),
                            conditionalPanel(condition = "!input.plot_region",
                                             checkboxInput("int", "Plot Country Borders"),
                                             checkboxInput("plot_rinstat", "Plot R-Instat"),
                                             shinyjs::hidden(actionButton("instat_file_local",
                                                                          label = "Browse files...")),
                                             shinyjs::hidden(shinyFiles::shinyFilesButton(
                                                              id = "instat_file_remote",
                                                              label = "Choose a file...",
                                                              multiple = FALSE,
                                                              title = "Please select a .RData-file."))),
                            conditionalPanel(condition = "input.plot_region || input.proj == 'rect'",
                                             checkboxInput("location", "Plot Own Location"),
                                             conditionalPanel(condition = "input.location == true",
                                                              tags$div(id = "ownLocation",
                                                                       fluidRow(column(6,
                                                                                       numericInput("lon_loc",
                                                                                                    label = "Lon",
                                                                                                    value = 8.75,
                                                                                                    min = -180,
                                                                                                    max = 180)),
                                                                                column(6,
                                                                                       numericInput("lat_loc",
                                                                                                    label = "Lat",
                                                                                                    value = 50.11,
                                                                                                    min = -90,
                                                                                                    max = 90)),
                                                                                column(6,
                                                                                       textInput("name_loc",
                                                                                                 label = "Name",
                                                                                                 value = "Offenbach")),
                                                                                column(6,
                                                                                       tags$label(" "),
                                                                                       actionButton("add_loc", label = "Add now", width = "100%")))))),
                            conditionalPanel(condition = "!input.plot_region",
                                             shinyjs::hidden(selectInput("proj",
                                                         label = "Projection",
                                                         choices = c(Rectangular = "rect", Orthographic = "ortho")))),
                            uiOutput("title_text"),
                            uiOutput("subtitle_text"),
                            shinyjs::hidden(uiOutput("title_text2")),
                            shinyjs::hidden(uiOutput("subtitle_text2")),
                            uiOutput("scale_caption")),
                   # FOR NOW NOT ALLOWING CHANGES TO WIDTH AND HEIGHT IN APP. (DO IT IN GLOBAL.R)
                   # uiOutput("width_height"),

                   tags$div(id = "sidebar_1d_plot",
                            shinyjs::hidden(uiOutput("date_dropdown_visualize")),
                            uiOutput("x_visualize"),
                            uiOutput("y_visualize"),
                            colourpicker::colourInput("integer",
                                                      "Color",
                                                      showColour = "background",
                                                      palette = "limited",
                                                      returnName = TRUE,
                                                      value = "royalblue4"),
                            checkboxInput("analyze_timeseries", "Analyze timeseries"),
                            conditionalPanel(
                              selectInput("checkGroup_type",
                                          label = "Line Type",
                                          choices = list("Line" = 1,
                                                         "Points" = 2,
                                                         "Line+Points" = 3,
                                                         "Steps" = 4,
                                                         "Histogram" = 5),
                                          selected = 1),
                              condition = '!input.analyze_timeseries',
                              checkboxInput("trend", "Add linear trend line"),
                              numericInput("ticknumber",
                                           label = "Number of major ticks",
                                           value = 6,
                                           min = 2),
                              selectInput("dateformat",
                                          label = "Date format",
                                          choices = list("YYYY" = 1,
                                                         "YYYY-MM" = 2,
                                                         "YYYY-MM-DD" = 3,
                                                         "YYYY-MM-DD HH:MM" = 4),
                                          selected = 2),
                              uiOutput("title_text_1d"),
                              uiOutput("subtitle_text_1d"),
                              uiOutput("x_axis_text_1d"),
                              uiOutput("y_axis_text_1d"))),

                   # FOR NOW NOT ALLOWING CHANGES TO WIDTH AND HEIGHT IN APP. (DO IT IN GLOBAL.R)
                   # uiOutput("width_height"),
                   fluidRow(
                     shinyjs::hidden(
                       tags$div(
                         id = "downloadExit",
                         column(8,
                                actionButton("showModal", "Download")),
                         column(4,
                                actionButton("backToSetup",
                                             label = "Back",
                                             class = "exit-button",
                                             width = "100%")))),
                     shinyjs::hidden(
                       tags$div(
                         id = "downloadExitMonitorClimate",
                         uiOutput("download_monitor_climate"),
                         br(),
                         actionButton("backToSetup2",
                                      label = "Back",
                                      class = "exit-button",
                                      width = "100%"))))),

      # Description panel.
      mainPanel(tabsetPanel(id = "mainVisualizeTabset",
                            tabPanel("Plot",
                                     tags$div(id = "plots_div",
                                              conditionalPanel(condition = "input.show_zoom",
                                                               plotOutput("previewSpatialCoveragePlot_vis",
                                                                          brush = brushOpts(id = "zoom_brush",
                                                                                            resetOnNew = TRUE))),
                                              tags$div(id = "myImage",
                                                       shinyjs::hidden(imageOutput("myImage_1d")),
                                                       shinyjs::hidden(imageOutput("myImage_2d")),
                                                       shinyjs::hidden(tags$div(id = "myImage_monitorClimate",
                                                                                imageOutput("monitorClimate_PNG"),
                                                                                uiOutput("monitorClimate_MP4")))),
                                              shinyjs::hidden(tags$div(id = "spinner_plot1",
                                                                       h3("Rendering plot..."))))),
                            tabPanel("Statistics",
                                     h4("Some numbers for the selected region."),
                                     verbatimTextOutput("statistics"),
                                     fluidRow(column(6,
                                                     imageOutput("myHist")),
                                              column(6,
                                                     imageOutput("myComp")))),
                            tabPanel("Metrics",
                                     h4("Metrics Compare Data"),
                                     verbatimTextOutput("metrics")),
                            tabPanel("File Summary",
                                     fluidRow(column(12,
                                                     h4("Short File Info"),
                                                     verbatimTextOutput("summary1")),
                                              column(12,
                                                     h4("Detailed File Info"),
                                                     verbatimTextOutput("summary2")))),
                            tabPanel("Parameters",
                                     h4("Some parameters"),
                                     verbatimTextOutput("mc_parameters"),
                                     #h4("Ranking"),
                                     verbatimTextOutput("mc_ranking"),
                            ),
                            tabPanel("About",
                                     fluidRow(column(8,
                                                     h4("The CM SAF Visualizer"),
                                                     verbatimTextOutput("about")),
                                              column(8,
                                                     h4("Good to know"),
                                                     verbatimTextOutput("tipps")),
                                              column(6,
                                                     uiOutput("link")))))))))
)
