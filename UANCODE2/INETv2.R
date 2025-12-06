# your code goes here
adresse <- function(add, base) {
  
  base_hex <- substring(add, c(1,3,5,7), c(2,4,6,8))
  if (base == 16) {
    return(paste(base_hex, collapse = "."))
  }
  
  else if (base == 10) {
    res <- rep("", length(base_hex))
    
    for (i in 1:length(base_hex)) {
      res[i] <- strtoi(base_hex[i], base = 16)
    }
    
    return(paste(res, collapse = "."))
  }
  
  else {
    stop("Base doit être 10 ou 16.")
  }
}
## Ne pas modifier le code ci-dessous
stream <- file("stdin", "r")
x <- scan(file=stream, what=character(), quiet=TRUE)
close(stream)

cat(adresse(x[1], as.integer(x[2])))
