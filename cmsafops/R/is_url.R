is_url <- function(x) {
  startsWith(x, "http://") | startsWith(x, "https://")
}