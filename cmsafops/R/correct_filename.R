correct_filename <- function(file) {
  ext <- unlist(strsplit(file, split = "\\."))
  ext <- ext[length(ext)]

  file_correct <- ifelse(ext %in% c("nc"),
                         file,
                         paste0(file, ".nc"))

  return(file_correct)
}
