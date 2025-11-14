# Keep the "sig" (trend significance) variable alongside a computed result,
# optionally remapping it to the result grid, and (optionally) masking "sig"
# wherever the result data are NA.
#
# Typical usage:
#   keep_sig_alongside(
#     infile         = "source.nc",         # contains result + sig (or at least sig)
#     outfile        = "result.nc",         # file you just wrote (result on target grid)
#     result_varname = "SIS_trend1",        # name of the result variable in outfile
#     remap_sig      = "auto",              # "auto","conservative","nearest","off"
#     dxy_factor     = 1,                   # like remap's distance filter (nearest)
#     cast_byte      = TRUE,                # store sig as int8 (-1/0/1, fill 127)
#     mask_sig_with_result_na = TRUE        # mask sig where result is NA
#   )
#
keep_sig_alongside <- function(infile, outfile, result_varname,
                               verbose = TRUE,
                               read_back_check = TRUE,
                               compress = TRUE,           # deflate/shuffle if possible
                               deflate_level = 9,         # 0–9, 2–4 is usually enough
                               chunks_xy = 256,           # chunk size for x/y
                               cast_byte = TRUE,          # store -1/0/1 as int8 when (re)mapping
                               remap_sig = c("auto","conservative","nearest","off"),
                               dxy_factor = 1,
                               mask_sig_with_result_na = TRUE) {
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
  
  # -------------------- helpers --------------------
  put_att <- function(var, name, value) {
    if (is.null(value)) return(invisible(TRUE))
    invisible(try(ncdf4::ncatt_put(dst, var, name, value), silent = TRUE))
  }
  get_att <- function(nc, var, att) {
    a <- try(ncdf4::ncatt_get(nc, var, att), silent = TRUE)
    if (!inherits(a, "try-error") && isTRUE(a$hasatt)) a$value else NULL
  }
  `%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  
  # For regular 1D lon/lat detection (used by remap path)
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
  
  # Silence C-layer chatter (used around define-mode ops)
  quiet_nc <- function(expr) {
    tf <- tempfile()
    con <- file(tf, open = "wt")
    on.exit({ try(close(con), TRUE); try(sink(NULL), TRUE); try(sink(type="message", NULL), TRUE) }, add = TRUE)
    sink(con); sink(con, type = "message")
    invisible(try(force(expr), silent = TRUE))
  }
  
  resolve_dim <- function(nc, nm) {
    d <- nc$dim[[nm]]
    if (!is.null(d)) return(d)
    alt <- switch(tolower(nm), "lon"="x","x"="lon","lat"="y","y"="lat", nm)
    nc$dim[[alt]]
  }
  
  # -------------------- inspect source sig --------------------
  sigv   <- src$var[["sig"]]
  dn_src <- vapply(sigv$dim, function(d) d$name, character(1))
  dl_src <- vapply(sigv$dim, function(d) d$len,  integer(1))
  logf("src sig dims (order/len): %s | %s", paste(dn_src, collapse=","), paste(dl_src, collapse=","))
  
  dims_dst <- lapply(dn_src, function(nm) resolve_dim(dst, nm))
  if (any(vapply(dims_dst, is.null, logical(1)))) {
    logf("! dst missing dims: %s", paste(dn_src[vapply(dims_dst, is.null, logical(1))], collapse=","))
    return(FALSE)
  }
  dl_dst <- vapply(dims_dst, function(d) d$len, integer(1))
  logf("dst dims (same names, len): %s | %s", paste(dn_src, collapse=","), paste(dl_dst, collapse=","))
  
  sig_data_src <- try(ncdf4::ncvar_get(src, "sig", raw_datavals = TRUE, collapse_degen = FALSE), silent = TRUE)
  if (inherits(sig_data_src, "try-error")) { logf("! ncvar_get(src,'sig') failed"); return(FALSE) }
  
  # Attributes to propagate
  units_att <- get_att(src, "sig", "units")       %||% "1"
  long_att  <- get_att(src, "sig", "long_name")   %||% "significance"
  stdname   <- get_att(src, "sig", "standard_name")
  descr     <- get_att(src, "sig", "description")
  fvals     <- get_att(src, "sig", "flag_values")
  fmeans    <- get_att(src, "sig", "flag_meanings")
  vrange    <- get_att(src, "sig", "valid_range")
  fill_src  <- get_att(src, "sig", "_FillValue") %||% get_att(src, "sig", "missing_value")
  
  # -------------------- copy vs. remap decision --------------------
  same_xy_shape <- all(dl_dst[1:2] == dl_src[1:2])
  
  # quick time alignment helper (if needed)
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
  
  if (same_xy_shape) {
    sig_data <- align_time_if_needed(sig_data_src)
    if (!identical(as.integer(dim(sig_data)), as.integer(dl_dst))) same_xy_shape <- FALSE
  }
  
  need_remap <- !same_xy_shape && remap_sig != "off"
  
  # Prepare sig_data (copy or remap)
  if (need_remap) {
    src_grid <- get_lonlat_1d(src)
    dst_grid <- get_lonlat_1d(dst)
    which_method <- "nearest"
    if (isTRUE(src_grid$reg) && isTRUE(dst_grid$reg) && (remap_sig %in% c("auto","conservative"))) {
      which_method <- "conservative"
    }
    if (which_method == "conservative" && !requireNamespace("rainfarmr", quietly = TRUE)) {
      logf("! rainfarmr not available → falling back to nearest.")
      which_method <- "nearest"
    }
    if (which_method == "nearest" && !requireNamespace("FNN", quietly = TRUE)) {
      logf("! FNN not available → cannot remap; abort.")
      return(FALSE)
    }
    
    ref_lon <- if (isTRUE(src_grid$reg)) src_grid$lon else stop("Source grid not regular 1D; cannot remap 'sig' conservatively.")
    ref_lat <- if (isTRUE(src_grid$reg)) src_grid$lat else stop("Source grid not regular 1D; cannot remap 'sig' conservatively.")
    tgt_lon <- if (isTRUE(dst_grid$reg)) dst_grid$lon else stop("Target grid not regular 1D; cannot remap 'sig' conservatively.")
    tgt_lat <- if (isTRUE(dst_grid$reg)) dst_grid$lat else stop("Target grid not regular 1D; cannot remap 'sig' conservatively.")
    
    if (which_method == "nearest") {
      fnn_a <- FNN::get.knnx(ref_lon, tgt_lon, k = 1)
      fnn_b <- FNN::get.knnx(ref_lat, tgt_lat, k = 1)
    }
    
    nt <- if (length(dim(sig_data_src)) >= 3) dim(sig_data_src)[3] else 1L
    sig_data <- array(NA_integer_, dim = c(length(tgt_lon), length(tgt_lat), nt))
    
    for (ti in seq_len(nt)) {
      src_slice <- if (length(dim(sig_data_src)) >= 3) sig_data_src[ , , ti, drop = TRUE] else sig_data_src
      src_slice[!is.finite(src_slice)] <- NA
      if (!is.null(fill_src)) src_slice[src_slice == fill_src] <- NA
      
      if (which_method == "nearest") {
        tmp <- src_slice[fnn_a$nn.index, fnn_b$nn.index]
        # Optional distance mask for sig is typically not needed; keep simple
        sig_data[ , , ti] <- tmp
      } else {
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
    
    dst_prec <- if (isTRUE(cast_byte)) "byte" else sigv$prec
    fill_att <- if (isTRUE(cast_byte)) 127L else (get_att(src, "sig", "_FillValue") %||% NA_real_)
  } else {
    # direct copy (maybe with time-align)
    sig_data <- sig_data_src
    dst_prec <- if (isTRUE(cast_byte)) "byte" else sigv$prec
    fill_att <- if (isTRUE(cast_byte)) 127L else (get_att(src, "sig", "_FillValue") %||% NA_real_)
    
    # reorder to destination result var's dim order if needed
    if (!missing(result_varname) && (result_varname %in% names(dst$var))) {
      dn_dst_for_res <- vapply(dst$var[[result_varname]]$dim, function(d) d$name, character(1))
      if (!identical(dn_src, dn_dst_for_res)) {
        perm <- match(dn_dst_for_res, dn_src)
        if (!any(is.na(perm))) sig_data <- aperm(sig_data, perm)
        # also update the dimension descriptors to match the new order
        dims_dst <- lapply(dn_dst_for_res, function(nm) resolve_dim(dst, nm))
        dl_dst   <- vapply(dims_dst, function(d) d$len, integer(1))
      }
    }
  }
  
  # -------------------- ensure 'sig' exists, then write --------------------
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
    
    # refresh handle after schema change
    try(ncdf4::nc_close(dst), silent = TRUE)
    dst <- try(ncdf4::nc_open(outfile, write = TRUE), silent = TRUE)
    if (inherits(dst, "try-error")) { logf("! re-open dst failed after add"); return(FALSE) }
    logf("...defined 'sig' and refreshed handle.")
  }
  
  # -------------------- optional masking by result NA --------------------
  # If requested, blank out 'sig' wherever the result variable is NA.
  if (isTRUE(mask_sig_with_result_na) && !missing(result_varname) && (result_varname %in% names(dst$var))) {
    dn_sig_out <- vapply(dst$var[["sig"]]$dim,    function(d) d$name, character(1))
    dn_res_out <- vapply(dst$var[[result_varname]]$dim, function(d) d$name, character(1))
    
    # Determine time length (if any)
    t_idx_sig <- grep("time", tolower(dn_sig_out))
    nt <- if (length(t_idx_sig) == 1) dst$var[["sig"]]$dim[[t_idx_sig]]$len else 1L
    
    # Helper to align a result slice to sig-dim order
    align_res_to_sig <- function(res_slice) {
      if (identical(dn_res_out, dn_sig_out)) return(res_slice)
      perm <- match(dn_sig_out, dn_res_out)
      if (any(is.na(perm))) return(res_slice)
      aperm(res_slice, perm)
    }
    
    # Decide fill to write for sig
    fill_to_write <- if (identical(dst_prec, "byte")) 127L else (fill_att %||% NA)
    
    for (ti in seq_len(nt)) {
      # Build start/count for res slice
      start_res <- rep(1L, length(dn_res_out))
      count_res <- vapply(dst$var[[result_varname]]$dim, function(d) d$len, integer(1))
      if (length(t_idx_res <- grep("time", tolower(dn_res_out))) == 1) {
        start_res[t_idx_res] <- ti
        count_res[t_idx_res] <- 1L
      }
      res_slice <- try(ncdf4::ncvar_get(dst, result_varname,
                                        start = start_res, count = count_res,
                                        raw_datavals = FALSE,      # interpret Fill as NA
                                        collapse_degen = FALSE),
                       silent = TRUE)
      if (inherits(res_slice, "try-error")) next
      res_slice <- align_res_to_sig(res_slice)
      
      # Extract the corresponding sig slice from sig_data (already in sig order)
      start_sig <- rep(1L, length(dn_sig_out))
      count_sig <- vapply(dst$var[["sig"]]$dim, function(d) d$len, integer(1))
      if (length(t_idx_sig) == 1) {
        start_sig[t_idx_sig] <- ti
        count_sig[t_idx_sig] <- 1L
      }
      
      # Get the current sig slice from the prepared array
      cur_sig <- if (length(dim(sig_data)) >= 3) sig_data[ , , ti, drop = TRUE] else sig_data
      
      # Apply mask: wherever result is NA, set sig to fill/NA
      mask <- is.na(res_slice)
      if (any(mask)) {
        if (is.na(fill_to_write)) cur_sig[mask] <- NA else cur_sig[mask] <- fill_to_write
      }
      
      # Write back into the prepared array
      if (length(dim(sig_data)) >= 3) {
        sig_data[ , , ti] <- cur_sig
      } else {
        sig_data <- cur_sig
      }
    }
    logf("Applied NA mask of '%s' to 'sig'.", result_varname)
  }
  
  # -------------------- write 'sig' --------------------
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
  
  # Attributes (ncatt_put handles define/data mode internally)
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
  
  # Ancillary link on the result variable
  if (!missing(result_varname) && (result_varname %in% names(dst$var))) {
    cur <- try(ncdf4::ncatt_get(dst, result_varname, "ancillary_variables"), silent = TRUE)
    cur <- if (!inherits(cur, "try-error") && isTRUE(cur$hasatt)) cur$value else ""
    new <- if (nzchar(cur) && !grepl("\\bsig\\b", cur)) paste(cur, "sig")
    else if (!nzchar(cur)) "sig" else cur
    put_att(result_varname, "ancillary_variables", new)
    logf("ancillary_variables(%s): %s", result_varname, new)
  }
  
  # Optional read-back check
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
