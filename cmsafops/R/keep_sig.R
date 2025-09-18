# utils-keep-sig.R

# Add imports in NAMESPACE:
# importFrom(ncdf4, nc_open, nc_close, ncvar_get, ncatt_get, ncatt_put,
#            ncdim_def, ncvar_def, ncvar_add, nc_redef, nc_enddef)
# importFrom(stats, setNames)

keep_sig_alongside <- function(infile, outfile, result_varname) {
  # Opens both files, and if 'sig' exists in infile and its dimensions
  # are compatible with outfile, it gets copied and linked via
  # ancillary_variables=result_varname: "sig".
  #
  # Returns TRUE if copied, FALSE otherwise. Never errors fatally.
  
  if (!file.exists(infile) || !file.exists(outfile)) return(FALSE)
  
  src <- NULL; dst <- NULL
  on.exit({
    if (!is.null(src)) ncdf4::nc_close(src)
    if (!is.null(dst)) ncdf4::nc_close(dst)
  }, add = TRUE)
  
  src <- try(ncdf4::nc_open(infile), silent = TRUE)
  if (inherits(src, "try-error")) return(FALSE)
  
  if (!("sig" %in% names(src$var))) return(FALSE)
  
  dst <- try(ncdf4::nc_open(outfile, write = TRUE), silent = TRUE)
  if (inherits(dst, "try-error")) return(FALSE)
  
  sigv <- src$var[["sig"]]
  
  # Collect dim names & lengths in source
  dn_src <- vapply(sigv$dim, function(d) d$name, character(1))
  dl_src <- vapply(sigv$dim, function(d) d$len, integer(1))
  
  # All these dims must exist in dst with identical lengths
  dims_dst <- lapply(dn_src, function(nm) dst$dim[[nm]])
  if (any(vapply(dims_dst, is.null, logical(1)))) return(FALSE)
  dl_dst <- vapply(dims_dst, function(d) d$len, integer(1))
  if (!all(dl_dst == dl_src)) {
    # Try time alignment by values if there is a single time dim
    t_ix <- which(grepl("time", tolower(dn_src)))
    if (length(t_ix) == 1) {
      # Read time coords and attempt to align
      tname <- dn_src[t_ix]
      t_src <- try(ncdf4::ncvar_get(src, tname), silent = TRUE)
      t_dst <- try(ncdf4::ncvar_get(dst, tname), silent = TRUE)
      if (inherits(t_src, "try-error") || inherits(t_dst, "try-error")) return(FALSE)
      
      # If destination is a subset/reorder of source, we can reindex
      map <- match(t_dst, t_src)
      if (any(is.na(map))) return(FALSE)
      
      sig_data <- try(ncdf4::ncvar_get(src, "sig"), silent = TRUE)
      if (inherits(sig_data, "try-error")) return(FALSE)
      
      # Build index list to slice along the time dim
      dims <- dim(sig_data)
      if (length(dims) != length(dn_src)) return(FALSE)
      
      sel <- vector("list", length(dims))
      for (k in seq_along(dims)) sel[[k]] <- seq_len(dims[k])
      sel[[t_ix]] <- map
      
      sig_data <- try(do.call(`[`, c(list(sig_data), sel, list(drop = FALSE))), silent = TRUE)
      if (inherits(sig_data, "try-error")) return(FALSE)
      
      # Now the sizes should match the dst dims
      dims_dst <- lapply(dn_src, function(nm) dst$dim[[nm]])
      dl_dst <- vapply(dims_dst, function(d) d$len, integer(1))
      if (!all(dim(sig_data) == dl_dst)) return(FALSE)
      
      # Define var in dst (if absent) and write
      units_att <- try(ncdf4::ncatt_get(src, "sig", "units")$value, silent = TRUE)
      if (inherits(units_att, "try-error") || is.null(units_att)) units_att <- ""
      long_att  <- try(ncdf4::ncatt_get(src, "sig", "long_name")$value, silent = TRUE)
      if (inherits(long_att, "try-error") || is.null(long_att)) long_att <- "significance"
      missval   <- if (!is.null(sigv$missval)) sigv$missval else NA_real_
      
      if (!("sig" %in% names(dst$var))) {
        ncdf4::nc_redef(dst)
        sig_def <- ncdf4::ncvar_def(
          name   = "sig",
          units  = units_att,
          dim    = dims_dst,
          missval = missval,
          longname = long_att,
          prec   = "short"  # or "integer"/"double" according to your files
        )
        ncdf4::ncvar_add(dst, sig_def)
        ncdf4::nc_enddef(dst)
      }
      ncdf4::ncvar_put(dst, "sig", sig_data)
      # Link as ancillary to the result variable
      if (result_varname %in% names(dst$var)) {
        ncdf4::ncatt_put(dst, result_varname, "ancillary_variables", "sig")
      }
      return(TRUE)
    }
    # No clean match → skip
    return(FALSE)
  }
  
  # Simple case: same dims/lengths → copy entire variable as-is
  sig_data <- try(ncdf4::ncvar_get(src, "sig"), silent = TRUE)
  if (inherits(sig_data, "try-error")) return(FALSE)
  
  units_att <- try(ncdf4::ncatt_get(src, "sig", "units")$value, silent = TRUE)
  if (inherits(units_att, "try-error") || is.null(units_att)) units_att <- ""
  long_att  <- try(ncdf4::ncatt_get(src, "sig", "long_name")$value, silent = TRUE)
  if (inherits(long_att, "try-error") || is.null(long_att)) long_att <- "significance"
  missval   <- if (!is.null(sigv$missval)) sigv$missval else NA_real_
  
  if (!("sig" %in% names(dst$var))) {
    ncdf4::nc_redef(dst)
    sig_def <- ncdf4::ncvar_def(
      name    = "sig",
      units   = units_att,
      dim     = dims_dst,
      missval = missval,
      longname = long_att,
      prec    = "short"  # or "integer"/"double"
    )
    ncdf4::ncvar_add(dst, sig_def)
    ncdf4::nc_enddef(dst)
  }
  ncdf4::ncvar_put(dst, "sig", sig_data)
  
  if (result_varname %in% names(dst$var)) {
    ncdf4::ncatt_put(dst, result_varname, "ancillary_variables", "sig")
  }
  
  TRUE
}
