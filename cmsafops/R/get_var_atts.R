get_var_atts <- function(attributes, var_name) {
  if (is.null(attributes$`_FillValue`)) {
    attributes$`_FillValue` <- -999
  }
  if (is.null(attributes$missing_value)) {
    attributes$missing_value <- -999
  }
  if (is.null(attributes$units)) {
    attributes$units <- UNDEFINED
  }
  if (is.null(attributes$standard_name)) {
    attributes$standard_name <- UNDEFINED
  }
  if (is.null(attributes$long_name)) {
    attributes$long_name <- var_name
  }
  return(attributes)
}
