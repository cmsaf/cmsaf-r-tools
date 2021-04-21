#' Check state dependencies
#'
#' Check whether the package requirements for plotting state borders are met.
#'
#' @noRd
check_state_dependencies <- function() {
  dependencies <- c(
    "rnaturalearth",
    "rnaturalearthhires"
  )
  dependencies_missing <- character(0)
  for (dependency in dependencies) {
    if (!requireNamespace(dependency, quietly = TRUE)) {
      dependencies_missing <- c(dependencies_missing, dependency)
    }
  }

  if (length(dependencies_missing != 0)) {
    stop(
      "For showing state borders, the packages \"rnaturalearth\" and ",
      "\"rnaturalearthhires\" are required.\n",
      "You can either set \"states = FALSE\" or install them with:\n",
      "install.packages(\"rnaturalearth\")\n",
      "and\n",
      "install.packages(\"rnaturalearthhires\", repos = \"http://packages.ropensci.org\", type = \"source\")"
    )
  }
}

#' Check package dependency
#'
#' Check whether package is installed and show useful error message otherwise.
#'
#' @param dependency is the package dependency to check for (character).
#' @param reason is the reason for the dependency (character).
#'
#' @importFrom assertthat assert_that is.string
#'
#' @noRd
check_package_dependency <- function(dependency, reason) {
  assert_that(is.string(dependency))
  assert_that(is.string(reason))

  if (!requireNamespace(dependency, quietly = TRUE)) {
    stop(
      "For ",
      reason,
      ", the package ",
      "\"",
      dependency,
      "\" is required.\n",
      "Please install it with: ",
      "install.packages(\"",
      dependency,
      "\")"
    )
  }
}
