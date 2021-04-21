#' Release questions
#'
#' These questions will be asked when publishing the package. See
#' http://r-pkgs.had.co.nz/release.html#release-submission for details.
#'
#' @noRd
release_questions <- function() {
  c(
    "Have you made sure that the code contains no occurence of `browser()`?",
    "Have you made sure that the package data is current by running `source('data-raw/generate_internal_data.R')`?",
    paste0("Have you updated the changelog at ", normalizePath("NEWS.md"), "?"),
    "Have you created current documentation with `devtools::document()`?",
    "Have you checked that the documentation content is up-to-date?",
    "Have you run a spell check with `spelling::spell_check_package()`?",
    paste0("Have you checked whether ", normalizePath(".Rbuildignore"), " is complete?"),
    "Have you run `goodpractice::gp()` and fixed all easily fixable issues?"
  )
}
