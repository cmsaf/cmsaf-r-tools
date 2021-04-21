get_var_prec <- function(varprec, var) {
  if (!is.null(varprec) && varprec %in% PRECISIONS_VAR) {
    var_prec <- varprec
  }  else{
    var_prec <- "float"
  }
  return(var_prec)
}
