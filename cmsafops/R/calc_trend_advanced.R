calc_trend_advanced <- function(var1, infile1, var2, infile2, file_data, file_data2) {
  
  length.dimension.x <- length(file_data$dimension_data$x)
  length.dimension.y <- length(file_data$dimension_data$y)
  length.dimension.t <- length(file_data$dimension_data$t)
  
  length.dimension.x2 <- length(file_data2$dimension_data$x)
  length.dimension.y2 <- length(file_data2$dimension_data$y)
  length.dimension.t2 <- length(file_data2$dimension_data$t)
  
  if(length(file_data$dimension_data$x) == length(file_data2$dimension_data$x) 
     & length(file_data$dimension_data$y) == length(file_data2$dimension_data$y) 
     & length(file_data$dimension_data$t) == length(file_data2$dimension_data$t)){

    nc_in_1 <- nc_open(infile1)
    nc_in_2 <- nc_open(infile2)
    
    dum_dat_1 <- array(NA, dim = c(length.dimension.x,
                                 length.dimension.y,
                                 length.dimension.t))
    dum_dat_2 <- array(NA, dim = c(length.dimension.x2,
                                   length.dimension.y2,
                                   length.dimension.t2))
    
    limit <- 2601 * 2601 * 31  # limit to avoid vector memory exhaustion, Can be adjust
    dimensionality <- as.double(length.dimension.x) *
      as.double(length.dimension.y) * as.double(length.dimension.t)
    
    if(limit < dimensionality){
      warning("The calculation takes a long time due to the large amount of data!")
      
      if(limit*2 < dimensionality){
        stop("Too many parameters in the model. Please select a file with less dimensions")
      }
      
      for (i in seq_along(file_data$dimension_data$t)) {
        dum_dat_t_1 <- ncvar_get(
          nc_in_1,
          var1,
          start = c(1, 1, i), count = c(-1, -1, 1),
          collapse_degen = FALSE
        )
        dum_dat_t_2 <- ncvar_get(
          nc_in_2,
          var2,
          start = c(1, 1, i), count = c(-1, -1, 1),
          collapse_degen = FALSE
        )
        dum_dat_1[,,i] <- dum_dat_t_1
        dum_dat_2[,,i] <- dum_dat_t_2
      }
    }else{
      dum_dat_1 <- ncvar_get(
        nc_in_1,
        var1,
        collapse_degen = FALSE
      )
      dum_dat_2 <- ncvar_get(
        nc_in_2,
        var2,
        collapse_degen = FALSE
      )
    }
    
    nc_close(nc_in_1)
    nc_close(nc_in_2)

    target <- array(NA, dim = c(length(file_data$dimension_data$x),
                                length(file_data$dimension_data$y), 1))
    target2 <- array(NA, dim = c(length(file_data$dimension_data$x),
                                 length(file_data$dimension_data$y), 1))
    target_p <- array(NA, dim = c(length(file_data$dimension_data$x),
                                  length(file_data$dimension_data$y), 1))
    target_p2 <- array(NA, dim = c(length(file_data$dimension_data$x),
                                  length(file_data$dimension_data$y), 1))
    target_r2 <- array(NA, dim = c(length(file_data$dimension_data$x),
                                  length(file_data$dimension_data$y), 1))
    
    dum_dat_t_all_1 <- c()
    dum_dat_t_all_2 <- c()
    result <- c()
    result_array <- array(numeric(),c(6,length(file_data$dimension_data$x),length(file_data$dimension_data$y))) 

    for(j in seq(length(file_data$dimension_data$x))){
      for(k in seq(length(file_data$dimension_data$y))){
        for(i in seq(length(file_data$dimension_data$t))){
          dum_dat_t_all_1 <- append(dum_dat_t_all_1, dum_dat_1[j,k,i])
          dum_dat_t_all_2 <- append(dum_dat_t_all_2, dum_dat_2[j,k,i])
        }
  
        result <- multiplelm(dum_dat_t_all_1, dum_dat_t_all_2)
        result_array[,j,k] <- result
        dum_dat_t_all_1 <- c()
        dum_dat_t_all_2 <- c()
      }
    }
    
    # calc sig 1
    target_p[which((result_array[3, , ] * result_array[4, , ]) < 0)] <- 0
    target_p[which(result_array[3, , ] < 0 & result_array[4, , ] < 0)] <- -1
    target_p[which(result_array[3, , ] > 0 & result_array[4, , ] > 0)] <- 1
    
    # calc sig 2
    target_p2[which((result_array[5, , ] * result_array[6, , ]) < 0)] <- 0
    target_p2[which(result_array[5, , ] < 0 & result_array[6, , ] < 0)] <- -1
    target_p2[which(result_array[5, , ] > 0 & result_array[6, , ] > 0)] <- 1

    # target time
    target  <- result_array[1, , ]
    
    # target var2
    target2 <- result_array[2, , ]

    target[is.na(target)] <- file_data$variable$attributes$missing_value
    target2[is.na(target2)] <- file_data$variable$attributes$missing_value
    target_p[is.na(target_p)] <- file_data$variable$attributes$missing_value
    target_p2[is.na(target_p)] <- file_data$variable$attributes$missing_value
    
    return(list(target = target, target2 = target2, target_p = target_p, target_p2 = target_p2))
  }
  else {
    stop("The data sets do not have the same dimension. Make sure that the dimensions match. ")
  }
}
