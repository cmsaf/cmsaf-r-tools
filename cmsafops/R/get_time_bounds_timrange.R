get_time_bounds_timrange <- function(times, nts) {
  tmp_result <- length(times)%%nts
  
  if(tmp_result != 0){
    tmp_result2 <- (nts - tmp_result)+length(times)
    result <- tmp_result2/nts
  }
  else{
    result <- length(times)/nts
  }
  
  time_bnds <- array(NA, dim = c(2, result))
  
  start <- 1
  second <- nts
  for (j in 1:(result)) {
    if((second) > length(times)){
      second <- length(times)
    }
    
    time_bnds[1, j] <- times[start]
    time_bnds[2, j] <- times[second]
    start <- start + nts
    second <- nts + (start - 1)
  }

  return(time_bnds)
}