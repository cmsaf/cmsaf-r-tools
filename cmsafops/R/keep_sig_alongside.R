################################################################################
# Helper functions
entered_define <- FALSE
safe_redef <- function(dst) {
  res <- try(ncdf4::nc_redef(dst), silent = TRUE)
  if (!inherits(res, "try-error")) state$in_define <- TRUE
  invisible(TRUE)
}
safe_enddef <- function(dst) {
  if (isTRUE(state$in_define)) {
    try(ncdf4::nc_enddef(dst), silent = TRUE)
    state$in_define <- FALSE
  }
  invisible(TRUE)
}

################################################################################
# Keep sig variable in the resulting file

keep_sig_alongside <- function(infile, outfile, result_varname,
                               verbose = TRUE,
                               read_back_check = TRUE,
                               compress = TRUE,           # deflate/shuffle if possible
                               deflate_level = 4,         # 0–9, 2–4 is usually enough
                               chunks_xy = 256,           # chunk size for x/y
                               cast_byte = FALSE          # store -1/0/1 as int8
) {
  logf <- function(...) if (isTRUE(verbose)) message(sprintf(...))
  
  if (!file.exists(infile))  { logf("! infile missing: %s", infile);  return(FALSE) }
  if (!file.exists(outfile)) { logf("! outfile missing: %s", outfile); return(FALSE) }
  
  src <- dst <- NULL
  on.exit({
    if (!is.null(src)) try(ncdf4::nc_close(src), silent = TRUE)
    if (!is.null(dst)) try(ncdf4::nc_close(dst), silent = TRUE)
  }, add = TRUE)
  
  src <- try(ncdf4::nc_open(infile), silent = TRUE)
  if (inherits(src, "try-error")) { logf("! nc_open(src) failed"); return(FALSE) }
  if (!("sig" %in% names(src$var))) { logf("! 'sig' not in infile"); return(FALSE) }
  
  dst <- try(ncdf4::nc_open(outfile, write = TRUE), silent = TRUE)
  if (inherits(dst, "try-error")) { logf("! nc_open(dst) failed"); return(FALSE) }
  
  # --- src dims ---
  sigv   <- src$var[["sig"]]
  dn_src <- vapply(sigv$dim, function(d) d$name, character(1))
  dl_src <- vapply(sigv$dim, function(d) d$len,  integer(1))
  logf("src sig dims (order/len): %s | %s", paste(dn_src, collapse=","), paste(dl_src, collapse=","))
  
  # dims must exist in dst
  dims_dst <- lapply(dn_src, function(nm) dst$dim[[nm]])
  if (any(vapply(dims_dst, is.null, logical(1)))) {
    logf("! dst missing dims: %s", paste(dn_src[vapply(dims_dst, is.null, logical(1))], collapse=","))
    return(FALSE)
  }
  dl_dst <- vapply(dims_dst, function(d) d$len, integer(1))
  logf("dst dims (same names, len): %s | %s", paste(dn_src, collapse=","), paste(dl_dst, collapse=","))
  
  # --- read src WITHOUT collapsing singleton dims ---
  sig_data <- try(ncdf4::ncvar_get(src, "sig", raw_datavals = TRUE, collapse_degen = FALSE), silent = TRUE)
  if (inherits(sig_data, "try-error")) { logf("! ncvar_get(src,'sig') failed"); return(FALSE) }
  logf("read sig_data with dim: %s", paste(dim(sig_data), collapse=","))  # expect 3D
  
  # --- optional time alignment ---
  if (!all(dl_dst == dl_src)) {
    t_ix <- which(grepl("time", tolower(dn_src)))
    if (length(t_ix) != 1) { logf("! cannot identify time dim"); return(FALSE) }
    tname <- dn_src[t_ix]
    t_src <- try(ncdf4::ncvar_get(src, tname, raw_datavals = TRUE), silent = TRUE)
    t_dst <- try(ncdf4::ncvar_get(dst, tname, raw_datavals = TRUE), silent = TRUE)
    if (inherits(t_src, "try-error") || inherits(t_dst, "try-error")) { logf("! get time failed"); return(FALSE) }
    map <- match(as.vector(t_dst), as.vector(t_src))
    if (any(is.na(map))) { logf("! time map failed"); return(FALSE) }
    idx <- replicate(length(dim(sig_data)), quote(expr = ), simplify = FALSE)
    for (k in seq_along(idx)) idx[[k]] <- seq_len(dim(sig_data)[k])
    idx[[t_ix]] <- map
    sig_data <- do.call(`[`, c(list(sig_data), idx, list(drop = FALSE)))
    dl_src   <- dim(sig_data)
    logf("post-alignment sig_data dim: %s", paste(dl_src, collapse=","))
  }
  
  # --- attributes from src ---
  get_att <- function(var, att) {
    a <- try(ncdf4::ncatt_get(src, var, att), silent = TRUE)
    if (inherits(a, "try-error") || !isTRUE(a$hasatt)) return(NULL)
    a$value
  }
  `%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  
  units_att <- get_att("sig", "units")       %||% "1"
  long_att  <- get_att("sig", "long_name")   %||% "significance"
  stdname   <- get_att("sig", "standard_name")
  descr     <- get_att("sig", "description")
  fvals     <- get_att("sig", "flag_values")
  fmeans    <- get_att("sig", "flag_meanings")
  vrange    <- get_att("sig", "valid_range")
  fill_att  <- get_att("sig", "_FillValue") %||% get_att("sig", "missing_value")
  
  # decide precision for dst
  dst_prec <- if (isTRUE(cast_byte)) "byte" else sigv$prec
  
  # coerce fill to dst precision
  coerce_fill <- function(val, prec) {
    if (is.null(val)) return(val)
    switch(prec,
           "byte"  = as.integer(ifelse(is.na(val), 127L, val)),  # 127 default
           "short" = as.integer(val),
           "int"   = as.integer(val),
           "float" = as.numeric(val),
           "double"= as.numeric(val),
           as.numeric(val)
    )
  }
  if (isTRUE(cast_byte)) {
    # map data to int8 flags -1/0/1; set fill as 127
    sig_data <- ifelse(is.na(sig_data), 127L, as.integer(sig_data))
    fill_att <- 127L
    units_att <- "1"
    long_att  <- long_att
  } else {
    fill_att <- coerce_fill(fill_att, dst_prec)
  }
  
  # choose chunksizes
  cs <- vapply(dims_dst, function(d) d$len, integer(1))
  cs[grepl("lon|x", names(cs), ignore.case = TRUE)] <- min(cs[grepl("lon|x", names(cs), ignore.case = TRUE)], chunks_xy)
  cs[grepl("lat|y", names(cs), ignore.case = TRUE)] <- min(cs[grepl("lat|y", names(cs), ignore.case = TRUE)], chunks_xy)
  cs[grepl("time", names(cs), ignore.case = TRUE)]  <- 1L
  chunksizes <- as.integer(cs)
  
  # --- define var if missing ---
  # --- define var if missing (robust to ncdf4 version) ---
  need_define <- !("sig" %in% names(dst$var))
  if (need_define) {
    logf("defining 'sig' in dst...")
    safe_redef()
    
    # Build a safe argument list for ncvar_def() based on supported formals
    fmls <- names(formals(ncdf4::ncvar_def))
    args <- list(
      name     = "sig",
      units    = units_att,
      dim      = dims_dst,
      missval  = fill_att,
      longname = long_att,
      prec     = dst_prec
    )
    
    # Optional compression/chunking only if your ncdf4 supports them
    if ("compression" %in% fmls && isTRUE(compress)) {
      # many ncdf4 versions expect compression=0..9
      args$compression <- deflate_level
    }
    if ("shuffle" %in% fmls && isTRUE(compress)) {
      args$shuffle <- TRUE
    }
    if ("chunksizes" %in% fmls && isTRUE(compress)) {
      args$chunksizes <- chunksizes
    }
    # (Do NOT add deflate/deflate_level if they aren't supported)
    
    # Call ncvar_def() with only-supported args
    sig_def <- try(do.call(ncdf4::ncvar_def, args), silent = TRUE)
    if (inherits(sig_def, "try-error")) {
      safe_enddef()  # leave define mode cleanly
      logf("! ncvar_def failed: %s", as.character(sig_def))
      return(FALSE)
    }
    
    add_res <- try(ncdf4::ncvar_add(dst, sig_def), silent = TRUE)
    if (inherits(add_res, "try-error")) {
      safe_enddef()
      logf("! ncvar_add failed: %s", as.character(add_res))
      return(FALSE)
    }
    
    safe_enddef()
    
    # Refresh handle to avoid stale-var issues
    try(ncdf4::nc_close(dst), silent = TRUE)
    dst <- try(ncdf4::nc_open(outfile, write = TRUE), silent = TRUE)
    if (inherits(dst, "try-error")) { logf("! re-open dst failed after add"); return(FALSE) }
    logf("...defined 'sig' and refreshed handle.")
  } else {
    logf("'sig' already exists in dst.")
  }
  
  # --- confirm dst sig dims ---
  if (!("sig" %in% names(dst$var))) { logf("! 'sig' not visible in dst"); return(FALSE) }
  dn_dst <- vapply(dst$var[["sig"]]$dim, function(d) d$name, character(1))
  dl_dst_real <- vapply(dst$var[["sig"]]$dim, function(d) d$len,  integer(1))
  logf("dst sig dims (actual): %s | %s", paste(dn_dst, collapse=","), paste(dl_dst_real, collapse=","))
  
  # permute if order differs
  if (!identical(dn_src, dn_dst)) {
    perm <- match(dn_dst, dn_src)
    if (any(is.na(perm))) { logf("! cannot permute: name mismatch"); return(FALSE) }
    sig_data <- aperm(sig_data, perm)
  }
  
  # shape check
  if (!identical(as.integer(dim(sig_data)), as.integer(dl_dst_real))) {
    logf("! shape mismatch: data=%s vs dst=%s",
         paste(dim(sig_data), collapse=","), paste(dl_dst_real, collapse=","))
    return(FALSE)
  }
  
  # write data
  wr <- try(ncdf4::ncvar_put(dst, "sig", sig_data,
                             start = rep(1L, length(dl_dst_real)),
                             count = as.integer(dl_dst_real)),
            silent = TRUE)
  if (inherits(wr, "try-error")) { logf("! ncvar_put failed: %s", as.character(wr)); return(FALSE) }
  logf("wrote sig_data successfully.")
  
  # try attributes in data mode first
  put_att <- function(var, name, value) {
    if (is.null(value)) return(invisible(TRUE))
    res <- try(ncdf4::ncatt_put(dst, var, name, value), silent = TRUE)
    if (inherits(res, "try-error")) {
      safe_redef()
      try(ncdf4::ncatt_put(dst, var, name, value), silent = TRUE)
      safe_enddef()
    }
    invisible(TRUE)
  }
  
  put_att("sig", "units",          units_att)
  put_att("sig", "long_name",      long_att)
  put_att("sig", "standard_name",  stdname)
  put_att("sig", "description",    descr)
  put_att("sig", "flag_values",    fvals)
  put_att("sig", "flag_meanings",  fmeans)
  put_att("sig", "valid_range",    vrange)

  # ancillary link
  if (result_varname %in% names(dst$var)) {
    cur <- try(ncdf4::ncatt_get(dst, result_varname, "ancillary_variables"), silent = TRUE)
    cur <- if (!inherits(cur, "try-error") && isTRUE(cur$hasatt)) cur$value else ""
    new <- if (nzchar(cur) && !grepl("\\bsig\\b", cur)) paste(cur, "sig")
    else if (!nzchar(cur)) "sig" else cur
      put_att(result_varname, "ancillary_variables", new)
      logf("ancillary_variables(%s): %s", result_varname, new)
  }
  
  # optional read-back
  if (isTRUE(read_back_check)) {
    tmp <- try(ncdf4::nc_open(outfile), silent = TRUE)
    if (!inherits(tmp, "try-error")) {
      on.exit(try(ncdf4::nc_close(tmp), silent = TRUE), add = TRUE)
      rb <- try(ncdf4::ncvar_get(tmp, "sig", raw_datavals = TRUE, collapse_degen = FALSE), silent = TRUE)
      if (!inherits(rb, "try-error")) {
        rng <- range(rb, na.rm = TRUE)
        logf("read-back range(sig): [%s, %s]", format(rng[1]), format(rng[2]))
      }
    }
  }
  
  TRUE
}
