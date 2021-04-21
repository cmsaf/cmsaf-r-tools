check_format <- function(format) {
  if (is.null(format)) {
    stop("format must not be NULL")
  }

  allowed_formats <- c("nc", "csv")
  if (!format %in% allowed_formats) {
    stop(paste0("format must be either nc or csv, but was ", format))
  }
}
