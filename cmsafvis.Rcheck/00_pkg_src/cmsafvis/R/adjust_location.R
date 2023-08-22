adjust_location <- function(variable,
                            variable_mask,
                            is_country,
                            mask_file,
                            var_file,
                            outfile) {
  if (is_country) {
    tryCatch(
      cmsafops::cmsaf.add(
        var1 = variable,
        var2 = variable_mask,
        infile1 = var_file,
        infile2 = mask_file,
        outfile = outfile,
        overwrite = TRUE
      ),
      error = function(cond) {
        if (endsWith(mask_file, "_final.nc")) {
          sub <- substr(mask_file, 1, nchar(mask_file) - 9)
          file2 <- paste0(sub, ".nc")
          stop(paste("An error occured while applying country mask.\nConsider deleting the files", mask_file, "and", file2, "and restarting the process."))
        } else {
          stop(paste("An error occured while applying country mask.\nConsider deleting the file", mask_file, "and restarting the process."))
        }
      })
  } else {
    if (!file.rename(var_file, outfile)) {
      stop(paste("Failed to rename", var_file, "to", outfile))
    }
  }
}
