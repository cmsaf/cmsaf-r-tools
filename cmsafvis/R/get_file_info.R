get_file_info <- function(filename) {
  product <- substr(basename(filename), 1, 3)
  interval <- substr(basename(filename), 4,4)
  stats <- substr(basename(filename), 5, 5)
  date_time <- substr(basename(filename), 6, 17)
  version_number <- substr(basename(filename), 18, 20)
  if (substr(basename(filename), 21, 22) == "UD") {
    grd <- substr(basename(filename), 21, 22)
  } else {
    grd <- as.character(as.numeric(substr(basename(filename), 21, 22)))
  }
  source <- substr(basename(filename), 23, 27)
  level <- substr(basename(filename), 28, 29)
  ar <- substr(basename(filename), 30, 31)
  id <- paste0(product, interval, stats, version_number, source, level, ar)
  # `naming_conventions` is generated in data-raw/generate_internal_data.R
  return(list(
    product_type = product,
    time_interval = naming_conventions$TimeInterval[[interval]],
    statistics = naming_conventions$Statistics[[stats]],
    date_time = as.POSIXct(date_time, format = "%Y%m%d%H%M"),
    version_number = version_number,
    grid = naming_conventions$Grid[[grd]],
    data_source = naming_conventions$Source[[source]],
    processing_level = level,
    area = naming_conventions$Area[[ar]],
    id = id
  ))
}
