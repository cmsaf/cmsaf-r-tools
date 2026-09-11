attach_file <- function(variable, infile, infile_attach, new_infile, temp_dir, verbose) {
  if (verbose) message(paste0("Attaching file ", infile, " to ", infile_attach))

  if (!file.exists(new_infile)) {
  tmpfile1 <- add_ncdf_ext(
    construct_filename(
      tools::file_path_sans_ext(basename(infile_attach)),
      "attach"
    )
  )

  tmpfile2 <- add_ncdf_ext(
    construct_filename(
      tools::file_path_sans_ext(basename(infile)),
      "attach"
    )
  )

  tmpfile1 <- file.path(temp_dir, tmpfile1)
  tmpfile2 <- file.path(temp_dir, tmpfile2)

  if (!file.copy(infile_attach, tmpfile1, overwrite = TRUE)) {
    stop(paste("Failed to copy", infile_attach, "to", tmpfile1))
  }

  if (!file.copy(infile, tmpfile2, overwrite = TRUE)) {
    stop(paste("Failed to copy", infile, "to", tmpfile2))
  }

  tryCatch({
  cmsafops::box_mergetime(
    var = variable,
    path = temp_dir,
    pattern = "_attach.nc",
    outfile = new_infile,
    overwrite = TRUE
  )
  }, error = function(cond) {
    stop(paste0("An error occured while merging the files: ",
                paste(list.files(path = temp_dir, pattern = "_attach.nc"), collapse = " & ")
                )
         )
  })

  if (file.exists(tmpfile1)) {
    file.remove(tmpfile1)
  }
  if (file.exists(tmpfile2)) {
    file.remove(tmpfile2)
  }
  } else if (verbose) {
    message("Merged file already exists, going to use it.")
  }
}
