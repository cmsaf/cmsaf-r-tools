fld_cor_covar_wrapper <- local({
  result <- c()
  function(op, var1, infile1, var2, infile2, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE) {
    result <<- c()
    calc_time_start <- Sys.time()
    
    check_variable(var1)
    check_variable(var2)
    
    check_infile(infile1)
    check_infile(infile2)
    
    check_outfile(outfile)
    
    outfile <- correct_filename(outfile)
    check_overwrite(outfile, overwrite)
    check_nc_version(nc34)
    
    ##### extract data from two files #####
    file_data_one <- read_file(infile1, var1)
    file_data_two <- read_file(infile2, var2)
    
    file_data_one$variable$prec <- "float"
    file_data_two$variable$prec <- "float"
    
    dimension_data_1 <- file_data_one$dimension_data
    dimension_data_2 <- file_data_two$dimension_data
    var_name_1 <- var1
    var_name_2 <- var2
    
    if(length(dimension_data_1$x) == length(dimension_data_2$x) 
       & length(dimension_data_1$y) == length(dimension_data_2$y) 
       & length(dimension_data_1$t) == length(dimension_data_2$t)) {
      
      length_time <- length(dimension_data_1$t)
      file_data_missing_value <- file_data_one$variable$attributes$missing_value
      nc_in_1 <- nc_open(infile1)
      nc_in_2 <- nc_open(infile2)
    
      if (verbose) {
        pb <- progress::progress_bar$new(
          format = "Calculation [:bar] :percent eta: :eta",
          total = length(1:length(dimension_data_1$t)),
          clear = TRUE,
          callback = function(x) {message("Calculation...")},
          show_after = 0
        )
      }
      
      
      lapply(1:length(dimension_data_1$t), function(i){
        # read data from infile1
        dum_dat_1 <- ncvar_get(
          nc_in_1,
          file_data_one$variable$name,
          start = c(1, 1, i), count = c(-1, -1, 1),
          collapse_degen = FALSE
        )
        # read data from infile2
        dum_dat_2 <- ncvar_get(
          nc_in_2,
          file_data_two$variable$name,
          start = c(1, 1, i), count = c(-1, -1, 1),
          collapse_degen = FALSE
        )
        
        result <<- switch(
          op,
          fldcor = append(result, stats::cor(c(dum_dat_1[,,1]), c(dum_dat_2[,,1]), use = "na.or.complete")),
          fldcovar = append(result, stats::cov(c(dum_dat_1[,,1]), c(dum_dat_2[,,1]), use = "na.or.complete"))
        )
        if (verbose) {
          if (i == 1) {
            pb$tick(0, tokens = list(i = i))
          } else {
            pb$tick(tokens = list(i = i))
          }
        }
      })
      result_array <- array(numeric(), c(1 , 1, length(dimension_data_1$t)))
      for(i in 1:length(result)){
        result_array[1, 1, i] <- result[i]
      }
      nc_close(nc_in_1)
      nc_close(nc_in_2)
    }
    else {
      stop("The data sets do not have the same dimension. Make sure that the dimensions match. Use cmsaf.adjust.two.files first. ")
    }
    result_final <- result_array
   
    result_final[is.na(result_final)] <- file_data_one$variable$attributes$missing_value
    
    file_data_one$variable$attributes$missing_value <- -999   # change value for missing values to -999
    
    vars_data <- list(result = result_final, time_bounds = file_data_one$dimension_data$t)
    
    nc_format <- get_nc_version(nc34)
    
    cmsaf_info <- switch(op,
                         fldcor = {"cmsaf::fldcor"},
                         fldcovar = {"cmsaf::fldcovar"},
    )
    cmsaf_info <- paste0(cmsaf_info, " for variable ", file_data_one$variable$name)
    
    ##### prepare output #####
    global_att_list <- names(file_data_one$global_att)
    global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
    global_attributes <- file_data_one$global_att[global_att_list]
    
    dims <- define_dims(file_data_one$grid$is_regular,
                        1,
                        1,
                        file_data_one$dimension_data$t,
                        NB2,
                        file_data_one$time_info$units,
                        with_time_bnds = FALSE)
    
    vars <- define_vars(file_data_one$variable, dims, nc_format$compression, with_time_bnds = FALSE)
    
    write_output_file(
      outfile,
      nc_format$force_v4,
      vars,
      vars_data,
      file_data_one$variable$name,
      file_data_one$grid$vars, file_data_one$grid$vars_data,
      cmsaf_info,
      file_data_one$time_info$calendar,
      file_data_one$variable$attributes,
      global_attributes,
      with_time_bnds = FALSE)
    
    if(verbose){
      pb$update(1) # Finishes the progress bar
    }
    
    calc_time_end <- Sys.time()
    if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
  }
})
