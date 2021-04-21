check_nc_version <- function(nc_version) {
  if (is.null(nc_version)) {
    stop("nc_version must not be NULL")
  }

  allowed_versions <- c(3, 4)
  if (!nc_version %in% allowed_versions) {
    stop("nc version must be in c(",
         paste0(allowed_versions, collapse = ", "),
         "), but was ", nc_version
    )
  }
}
