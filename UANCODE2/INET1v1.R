# your code goes here
adresse <- function(add, base) {
  if(base != 10 && base != 16){
    stop("Base doit être 10 ou 16.")
  }
  car <- as.character(add)
  octets <- substring(car, c(1,3,5,7), c(2,4,6,8))
  for(i in 1:4){
    if(base == 10){
      octets[i] <- as.character(strtoi(octets[i], base=16))
    } else { 
      octets[i] <- sprintf("%02X", strtoi(octets[i], base=16))
    }
  }
  paste(octets, collapse = ".")
}
## Ne pas modifier le code ci-dessous
stream <- file("stdin", "r")
x <- scan(file=stream, what=character(), quiet=TRUE)
close(stream)

cat(adresse(x[1], as.integer(x[2])))
