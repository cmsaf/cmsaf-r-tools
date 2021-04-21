get_var_from_vars_all <- function(varnames, var) {
  var_default <- varnames

  if (toupper(var) %in% toupper(var_default)) {
    var <- var_default[which(toupper(var) == toupper(var_default))]
  } else {
    warning("Variable '", var, "' not found. ",
            "Variable '", var_default[1], "' will be used instead.")
    var <- var_default[1]
  }

  return(var)
}
