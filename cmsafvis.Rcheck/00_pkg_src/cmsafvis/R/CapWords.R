# Function to capitalize the first letter of each word
# Taken from Andrie from Stackoverflow

CapWords <- function(x) {
  x <- as.character(x)
  w <- strsplit(x, " ")[[1]]
  a <- paste(toupper(substring(w, 1,1)), substring(w, 2),
        sep="", collapse=" ")
  cap <- a
  return(cap)
}