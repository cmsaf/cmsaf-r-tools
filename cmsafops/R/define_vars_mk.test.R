# helper function for mk.test to define vars
define_vars_mk.test <- function(variable, dims, compression, S, Z)
{
  var1 <- ncvar_def(
    name = S$name,
    units = S$units,
    dim = dims[c("x", "y", "t")],
    missval = variable$attributes$missing_value,
    prec = "double",
    compression = compression
  )
  
  var2 <- ncvar_def(
    name = TIME_BOUNDS_NAMES$DEFAULT,
    units = UNITS$ONE,
    dim = dims[c("tb", "t")],
    prec = PRECISIONS_VAR$DOUBLE
  )
  
  var3 <- ncvar_def(
    name = Z$name,
    units = Z$units,
    dim = dims[c("x", "y", "t")],
    missval = variable$attributes$missing_value,
    prec = "double",
    compression = compression
  )
  
  vars <- list(var1, var2, var3)
  
  return(vars)
}