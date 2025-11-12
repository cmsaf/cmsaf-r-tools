keep_sig_alongside <- function(infile, outfile, result_varname,
                               verbose = TRUE,
                               read_back_check = TRUE,
                               compress = TRUE,           # deflate/shuffle if possible
                               deflate_level = 4,         # 0–9, 2–4 is usually enough
                               chunks_xy = 256,           # chunk size for x/y
                               cast_byte = TRUE,          # store -1/0/1 as int8 when (re)mapping
                               remap_sig = c("auto","conservative","nearest","off"),
                               dxy_factor = 1) {
  logf <- function(...) if (isTRUE(verbose)) message(sprintf(...))
  remap_sig <- match.arg(remap_sig)
  
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
  
  # --- helpers -------------------------------------------------------------
  put_att <- function(var, name, value) {
    if (is.null(value)) return(invisible(TRUE))
    invisible(try(ncdf4::ncatt_put(dst, var, name, value), silent = TRUE))
  }
  get_att <- function(nc, var, att) {
    a <- try(ncdf4::ncatt_get(nc, var, att), silent = TRUE)
    if (!inherits(a, "try-error") && isTRUE(a$hasatt)) a$value else NULL
  }
  `%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  
  # try to get 1D lon/lat coordinate vectors (regular grid heuristic)
  get_lonlat_1d <- function(nc) {
    lon_nm <- if (!is.null(nc$dim$lon)) "lon" else if (!is.null(nc$dim$x)) "x" else NULL
    lat_nm <- if (!is.null(nc$dim$lat)) "lat" else if (!is.null(nc$dim$y)) "y" else NULL
    if (is.null(lon_nm) || is.null(lat_nm)) return(list(reg = FALSE))
    lon <- try(ncdf4::ncvar_get(nc, lon_nm), silent = TRUE)
    lat <- try(ncdf4::ncvar_get(nc, lat_nm), silent = TRUE)
    if (inherits(lon, "try-error") || inherits(lat, "try-error")) return(list(reg = FALSE))
    if (length(dim(lon)) > 1 || length(dim(lat)) > 1) return(list(reg = FALSE))
    list(reg = TRUE, lon = as.vector(lon), lat = as.vector(lat), lon_name = lon_nm, lat_name = lat_nm)
  }
  
  # small utility to silence any console output (ncdf4 C layer)
  quiet_nc <- function(expr) {
    tf <- tempfile()
    con <- file(tf, open = "wt")
    on.exit({ try(close(con), TRUE); try(sink(NULL), TRUE); try(sink(type="message", NULL), TRUE) }, add = TRUE)
    sink(con); sink(con, type = "message")
    invisible(try(force(expr), silent = TRUE))
  }
  
  safe_match <- function(x, y) {
    x <- if (is.null(x)) character() else as.vector(x)
    y <- if (is.null(y)) character() else as.vector(y)
    match(x, y)
  }
  
  resolve_dim <- function(nc, nm) {
    d <- nc$dim[[nm]]
    if (!is.null(d)) return(d)
    alt <- switch(tolower(nm), "lon"="x","x"="lon","lat"="y","y"="lat", nm)
    nc$dim[[alt]]
  }
  
  # --- inspect source sig and dims ----------------------------------------
  sigv   <- src$var[["sig"]]
  dn_src <- vapply(sigv$dim, function(d) d$name, character(1))
  dl_src <- vapply(sigv$dim, function(d) d$len,  integer(1))
  logf("src sig dims (order/len): %s | %s", paste(dn_src, collapse=","), paste(dl_src, collapse=","))
  
  # destination dims must exist, but may differ in length (remap case)
  dims_dst <- lapply(dn_src, function(nm) resolve_dim(dst, nm))
  
  if (any(vapply(dims_dst, is.null, logical(1)))) {
    logf("! dst missing dims: %s", paste(dn_src[vapply(dims_dst, is.null, logical(1))], collapse=","))
    return(FALSE)
  }
  dl_dst <- vapply(dims_dst, function(d) d$len, integer(1))
  logf("dst dims (same names, len): %s | %s", paste(dn_src, collapse=","), paste(dl_dst, collapse=","))
  
  # read source data (preserve singleton time)
  sig_data_src <- try(ncdf4::ncvar_get(src, "sig", raw_datavals = TRUE, collapse_degen = FALSE), silent = TRUE)
  if (inherits(sig_data_src, "try-error")) { logf("! ncvar_get(src,'sig') failed"); return(FALSE) }
  
  # attributes we may propagate
  units_att <- get_att(src, "sig", "units")       %||% "1"
  long_att  <- get_att(src, "sig", "long_name")   %||% "significance"
  stdname   <- get_att(src, "sig", "standard_name")
  descr     <- get_att(src, "sig", "description")
  fvals     <- get_att(src, "sig", "flag_values")
  fmeans    <- get_att(src, "sig", "flag_meanings")
  vrange    <- get_att(src, "sig", "valid_range")
  fill_src  <- get_att(src, "sig", "_FillValue") %||% get_att(src, "sig", "missing_value")
  
  # quick time alignment if only time length differs but coords match
  align_time_if_needed <- function(data) {
    if (all(dl_dst == dl_src)) return(data)
    t_ix <- which(grepl("time", tolower(dn_src)))
    if (length(t_ix) != 1) return(data)
    tname <- dn_src[t_ix]
    t_src <- try(ncdf4::ncvar_get(src, tname, raw_datavals = TRUE), silent = TRUE)
    t_dst <- try(ncdf4::ncvar_get(dst, tname, raw_datavals = TRUE), silent = TRUE)
    if (inherits(t_src, "try-error") || inherits(t_dst, "try-error")) return(data)
    map <- match(as.vector(t_dst), as.vector(t_src))
    if (any(is.na(map))) return(data)
    idx <- replicate(length(dim(data)), quote(expr = ), simplify = FALSE)
    for (k in seq_along(idx)) idx[[k]] <- seq_len(dim(data)[k])
    idx[[t_ix]] <- map
    do.call(`[`, c(list(data), idx, list(drop = FALSE)))
  }
  
  # --- decide whether to copy or remap ------------------------------------
  same_xy_shape <- all(dl_dst[1:2] == dl_src[1:2])
  if (same_xy_shape) {
    # try a plain copy path (fast)
    sig_data <- align_time_if_needed(sig_data_src)
    # if shapes still mismatch (e.g. re-ordered dims), fall back to remap path
    if (!identical(as.integer(dim(sig_data)), as.integer(dl_dst))) same_xy_shape <- FALSE
  }
  
  need_remap <- !same_xy_shape && remap_sig != "off"
  # for remap we need lon/lat from both files
  if (need_remap) {
    src_grid <- get_lonlat_1d(src)
    dst_grid <- get_lonlat_1d(dst)
    if (!isTRUE(src_grid$reg) || !isTRUE(dst_grid$reg)) {
      logf("Grid(s) are not regular 1D lon/lat: falling back to nearest.")
      which_method <- "nearest"
    } else if (remap_sig == "conservative" || (remap_sig == "auto")) {
      which_method <- if (isTRUE(src_grid$reg) && isTRUE(dst_grid$reg)) "conservative" else "nearest"
    } else {
      which_method <- "nearest"
    }
    
    if (which_method == "conservative" && !requireNamespace("rainfarmr", quietly = TRUE)) {
      logf("! rainfarmr not available → falling back to nearest.")
      which_method <- "nearest"
    }
    if (which_method == "nearest" && !requireNamespace("FNN", quietly = TRUE)) {
      logf("! FNN not available → cannot remap; abort.")
      return(FALSE)
    }
    
    # Prepare remap mapping structures
    ref_lon <- src_grid$lon; ref_lat <- src_grid$lat
    tgt_lon <- dst_grid$lon; tgt_lat <- dst_grid$lat
    
    if (which_method == "nearest") {
      fnn_a <- FNN::get.knnx(ref_lon, tgt_lon, k = 1)
      fnn_b <- FNN::get.knnx(ref_lat, tgt_lat, k = 1)
    }
    
    # build sig_data with target shape
    sig_data <- array(NA_integer_, dim = c(length(tgt_lon), length(tgt_lat), dl_dst[which(grepl("time", tolower(dn_src))) %||% 3]))
    # ensure we have a time loop index
    nt <- if (length(dim(sig_data_src)) >= 3) dim(sig_data_src)[3] else 1L
    if (length(dim(sig_data)) < 3) dim(sig_data) <- c(dim(sig_data)[1:2], nt)
    
    # remap per time slice
    for (ti in seq_len(nt)) {
      # read source slice and coerce to numeric
      src_slice <- if (length(dim(sig_data_src)) >= 3) sig_data_src[ , , ti, drop = TRUE] else sig_data_src
      # treat source fill/NA as NA
      src_slice[!is.finite(src_slice)] <- NA
      if (!is.null(fill_src)) src_slice[src_slice == fill_src] <- NA
      
      if (which_method == "nearest") {
        tmp <- src_slice[fnn_a$nn.index, fnn_b$nn.index]
        # optional distance mask like in remap(): not strictly needed for sig
        sig_data[ , , ti] <- tmp
      } else {
        # conservative vote via area-weighted fraction per class
        one_if <- function(x) { y <- ifelse(x, 1, 0); y[is.na(y)] <- 0; y }
        f_pos <- rainfarmr::remapcon(ref_lon, ref_lat, one_if(src_slice ==  1L), tgt_lon, tgt_lat)
        f_nul <- rainfarmr::remapcon(ref_lon, ref_lat, one_if(src_slice ==  0L), tgt_lon, tgt_lat)
        f_neg <- rainfarmr::remapcon(ref_lon, ref_lat, one_if(src_slice == -1L), tgt_lon, tgt_lat)
        mx <- pmax(f_nul, f_pos, f_neg, na.rm = TRUE)
        out <- array(NA_integer_, dim = c(length(tgt_lon), length(tgt_lat)))
        # deterministic tie-break: 0 > +1 > -1
        out[f_neg == mx] <- -1L
        out[f_pos == mx] <-  1L
        out[f_nul == mx] <-  0L
        nothing <- (f_pos + f_nul + f_neg) == 0
        out[nothing] <- NA_integer_
        sig_data[ , , ti] <- out
      }
    }
    
    # when remapped, store as byte with fill 127 by default
    dst_prec <- if (isTRUE(cast_byte)) "byte" else sigv$prec
    fill_att <- if (isTRUE(cast_byte)) 127L else (get_att(src, "sig", "_FillValue") %||% NA_real_)
  } else {
    # copy path; preserve original precision unless cast_byte requested
    dst_prec <- if (isTRUE(cast_byte)) "byte" else sigv$prec
    fill_att <- if (isTRUE(cast_byte)) 127L else (get_att(src, "sig", "_FillValue") %||% NA_real_)
    # ensure shape matches destination dims order
    dn_dst <- vapply(dst$var[[result_varname]]$dim, function(d) d$name, character(1))
    if (!identical(dn_src, dn_dst)) {
      if (length(dn_src) && length(dn_dst) && all(nzchar(dn_src)) && all(nzchar(dn_dst))) {
        perm <- safe_match(dn_dst, dn_src)
        if (!any(is.na(perm))) sig_data <- aperm(sig_data, perm)
      }
    }
  }
  
  # --- ensure variable exists in destination and write it ------------------
  # build chunk sizes
  cs <- vapply(dims_dst, function(d) d$len, integer(1))
  cs[grepl("lon|x", names(cs), ignore.case = TRUE)] <- pmin(cs[grepl("lon|x", names(cs), ignore.case = TRUE)], chunks_xy)
  cs[grepl("lat|y", names(cs), ignore.case = TRUE)] <- pmin(cs[grepl("lat|y", names(cs), ignore.case = TRUE)], chunks_xy)
  cs[grepl("time", names(cs), ignore.case = TRUE)]  <- 1L
  chunksizes <- as.integer(cs)
  
  need_define <- !("sig" %in% names(dst$var))
  if (need_define) {
    logf("defining 'sig' in dst...")
    fmls <- names(formals(ncdf4::ncvar_def))
    args <- list(
      name     = "sig",
      units    = units_att,
      dim      = dims_dst,
      missval  = fill_att,
      longname = long_att,
      prec     = dst_prec
    )
    # Compression/chunking only if supported (NetCDF-4)
    if ("compression" %in% fmls && isTRUE(compress)) args$compression <- deflate_level
    if ("shuffle"     %in% fmls && isTRUE(compress)) args$shuffle     <- TRUE
    if ("chunksizes"  %in% fmls && isTRUE(compress)) args$chunksizes  <- chunksizes
    
    quiet_nc(ncdf4::nc_redef(dst))
    sig_def <- try(do.call(ncdf4::ncvar_def, args), silent = TRUE)
    if (inherits(sig_def, "try-error")) {
      quiet_nc(ncdf4::nc_enddef(dst))
      logf("! ncvar_def failed: %s", as.character(sig_def))
      return(FALSE)
    }
    add_res <- try(ncdf4::ncvar_add(dst, sig_def), silent = TRUE)
    if (inherits(add_res, "try-error")) {
      quiet_nc(ncdf4::nc_enddef(dst))
      logf("! ncvar_add failed: %s", as.character(add_res))
      return(FALSE)
    }
    quiet_nc(ncdf4::nc_enddef(dst))
    
    # refresh handle for safety
    try(ncdf4::nc_close(dst), silent = TRUE)
    dst <- try(ncdf4::nc_open(outfile, write = TRUE), silent = TRUE)
    if (inherits(dst, "try-error")) { logf("! re-open dst failed after add"); return(FALSE) }
    logf("...defined 'sig' and refreshed handle.")
  }
  
  # write data (cast to byte if requested)
  if (identical(dst_prec, "byte")) {
    sig_write <- ifelse(is.na(sig_data), 127L, as.integer(sig_data))
  } else {
    sig_write <- sig_data
    sig_write[is.na(sig_write)] <- fill_att
  }
  wr <- try(ncdf4::ncvar_put(dst, "sig", sig_write,
                             start = rep(1L, length(dl_dst)),
                             count = as.integer(dl_dst)),
            silent = TRUE)
  if (inherits(wr, "try-error")) { logf("! ncvar_put(sig) failed: %s", as.character(wr)); return(FALSE) }
  logf("wrote sig_data successfully.")
  
  # attributes (data mode; ncatt_put handles mode internally)
  put_att("sig", "units",          units_att)
  put_att("sig", "long_name",      long_att)
  put_att("sig", "standard_name",  stdname)
  put_att("sig", "description",    descr)
  if (identical(dst_prec, "byte")) {
    put_att("sig", "flag_values",   c(-1L, 0L, 1L))
    put_att("sig", "flag_meanings", "negative_significant not_significant positive_significant")
  } else {
    if (!is.null(fvals))  put_att("sig", "flag_values",  fvals)
    if (!is.null(fmeans)) put_att("sig", "flag_meanings", fmeans)
  }
  if (!is.null(vrange))  put_att("sig", "valid_range",  vrange)
  
  # ancillary link on the result variable
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
