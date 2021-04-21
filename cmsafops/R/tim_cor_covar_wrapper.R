tim_cor_covar_wrapper <- local({
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
    
    time_bnds <- get_time_bounds_1(file_data_one$dimension_data$t)
    time_bnds_two <- get_time_bounds_1(file_data_two$dimension_data$t)
    
    dimension_data_1 <- file_data_one$dimension_data
    dimension_data_2 <- file_data_two$dimension_data
    var_name_1 <- var1
    var_name_2 <- var2
    
    if(length(dimension_data_1$x) == length(dimension_data_2$x) 
       & length(dimension_data_1$y) == length(dimension_data_2$y) 
       & length(dimension_data_1$t) == length(dimension_data_2$t)){
      
      nc_in_1 <- nc_open(infile1)
      nc_in_2 <- nc_open(infile2)
      
      limit <- 2601 * 2601 * 31 # limit to avoid vector memory exhaustion, Can be adjust
      dimensionality <- as.double(length(dimension_data_1$x)) *
        as.double(length(dimension_data_1$y)) * as.double(length(dimension_data_1$t))
      
      if(limit < dimensionality){
        dum_dat_1 <- array(NA, dim = c(length(dimension_data_1$x),
                                       length(dimension_data_1$y),
                                       length(dimension_data_1$t)))
        dum_dat_2 <- array(NA, dim = c(length(dimension_data_2$x),
                                       length(dimension_data_2$y),
                                       length(dimension_data_2$t)))
        warning("The calculation takes a long time due to the large amount of data!")
        for (i in seq_along(file_data_one$dimension_data$t)) {
          dum_dat_t <- ncvar_get(
            nc_in_1,
            file_data_one$variable$name,
            start = c(1, 1, i), count = c(-1, -1, 1),
            collapse_degen = FALSE
          )
          dum_dat_1[,,i] <- dum_dat_t
          
          dum_dat_t <- ncvar_get(
            nc_in_2,
            file_data_two$variable$name,
            start = c(1, 1, i), count = c(-1, -1, 1),
            collapse_degen = FALSE
          )
          dum_dat_2[,,i] <- dum_dat_t
        }
      }else{
        dum_dat_1 <- ncvar_get(nc_in_1, var_name_1, collapse_degen = FALSE)
        dum_dat_2 <- ncvar_get(nc_in_2, var_name_2, collapse_degen = FALSE)
      }
      nc_close(nc_in_1)
      nc_close(nc_in_2)
      
      if (verbose) {
        pb <- progress::progress_bar$new(
          format = "Calculation [:bar] :percent eta: :eta",
          total = length(1:length(dimension_data_1$x)),
          clear = TRUE,
          callback = function(x) {message("Calculation...")},
          show_after = 0
        )
      }
      
      dum_dat_t_all_1 <- c()
      dum_dat_t_all_2 <- c()
      suppressWarnings({
        lapply(1:length(dimension_data_1$x), function(j){
          lapply(1:length(dimension_data_1$y), function(k){
            for(i in seq(length(dimension_data_1$t))){
              dum_dat_t_all_1 <- append(dum_dat_t_all_1, dum_dat_1[j,k,i])
              dum_dat_t_all_2 <- append(dum_dat_t_all_2, dum_dat_2[j,k,i])
            }
            result <<- switch(op,
              timcor = append(result, stats::cor(dum_dat_t_all_1, dum_dat_t_all_2, use = "na.or.complete")),
              timcovar = append(result, stats::cov(dum_dat_t_all_1, dum_dat_t_all_2, use = "na.or.complete"))
            )
            dum_dat_t_all_1 <- c()
            dum_dat_t_all_2 <- c()
            
          })
          if (verbose) {
            if (j == 1) {
              pb$tick(0, tokens = list(j = j))
            } else {
              pb$tick(tokens = list(j = j))
            }
          }
        })
      })
      result_array <- array(numeric(),c(length(dimension_data_1$x),length(dimension_data_1$y),1)) 
      for(i in 1:length(result)){
        result_array[i] <- result[i]
      }
      result_final <- aperm(result_array)
    }
    else {
      stop("The datasets do not have the same dimension. Make sure that the dimensions match. Use cmsaf.adjust.two.files first. ")
    }
    
    result_final[is.na(result_final)] <- file_data_one$variable$attributes$missing_value
    file_data_one$variable$attributes$missing_value <- -999   # change value for missing values to -999
    
    vars_data <- list(result = result_final, time_bounds = time_bnds)
    
    nc_format <- get_nc_version(nc34)
    
    cmsaf_info <- switch(op,
                         timcor = {"cmsaf::timcor"},
                         timcovar = {"cmsaf::timcovar"},
    )
    cmsaf_info <- paste0(cmsaf_info, " for variable ", file_data_one$variable$name)
    
    time_data <- time_bnds[2, ]
    
    ##### prepare output #####
    global_att_list <- names(file_data_one$global_att)
    global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
    global_attributes <- file_data_one$global_att[global_att_list]
    
    dims <- define_dims(file_data_one$grid$is_regular,
                        file_data_one$dimension_data$x,
                        file_data_one$dimension_data$y,
                        time_data,
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