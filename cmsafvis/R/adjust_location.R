adjust_location <- function(variable,
                            variable_mask,
                            is_country,
                            mask_file = NULL,
                            var_file,
                            outfile) {
  if (is_country) {
    if (is.null(mask_file) || !file.exists(mask_file)) {
      stop("Country mask file is missing.")
    }

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
          stop(paste("An error occurred while applying country mask.\nConsider deleting the files", mask_file, "and", file2, "and restarting the process."))
        } else {
          stop(paste("An error occurred while applying country mask.\nConsider deleting the file", mask_file, "and restarting the process."))
        }
      })
  } else {
    if (file.exists(outfile)) {
      file.remove(outfile)
    }

    if (!file.copy(var_file, outfile, overwrite = TRUE)) {
      stop(paste("Failed to copy", var_file, "to", outfile))
    }
  }

  invisible(outfile)
}
