define_vars <- function(variable, dims, compression, with_time_bnds = TRUE, precision = variable$prec) {
  var1 <- ncvar_def(
    name = variable$name,
    units = variable$attributes$units,
    dim = dims[c("x", "y", "t")],
    missval = variable$attributes$missing_value,
    prec = precision,
    compression = compression
  )
  if (with_time_bnds) {
    var2 <- ncvar_def(
      name = TIME_BOUNDS_NAMES$DEFAULT,
      units = UNITS$ONE,
      dim = dims[c("tb", "t")],
      prec = PRECISIONS_VAR$DOUBLE
    )
    vars <- list(var1, var2)
  }else{
    vars <- list(var1)
  }

  return(vars)
}
