# Function for getting number of breaks.
break_num <- function(ln,
                      bn,
                      minn,
                      maxn,
                      max_data) {
  dg <- 2
  if (abs(max_data) >= 10)
    (dg <- 1)
  if (abs(max_data) >= 100)
    (dg <- 0)
  a <- vector(mode = "character", length = bn)
  b <- round(seq(minn, maxn, length.out = bn), digits = dg)
  c <- round(seq(1, length(b), length.out = ln))
  a[c] <- b[c]
  labs <- a
  return(labs)
}
