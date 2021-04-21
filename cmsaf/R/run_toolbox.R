#' Run the CMSAF R Toolbox.
#'
#' Run the interactive shiny-based CM SAF R Toolbox. It uses functions from
#' \link[cmsafops:cmsafops]{cmsafops} and \link[cmsafvis:cmsafvis]{cmsafvis}.
#'
#'
#' @param ... Arguments to be passed to \code{\link[shiny]{runApp}}.
#'
#' @export
#' @import cmsafops cmsafvis
#'
#' @examples \dontrun{run_toolbox(launch.browser = TRUE)}
run_toolbox <- function(...) {
  app_dir <- system.file("toolbox", package = "cmsaf")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing 'cmsaf'.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}
