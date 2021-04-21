check_overwrite <- function(file, overwrite) {
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(paste0(c("File", paste0("'", file, "'"), "already exists.",
                  "Specify 'overwrite = TRUE' if you want to overwrite it."),
                collapse = " "))
  }
}
