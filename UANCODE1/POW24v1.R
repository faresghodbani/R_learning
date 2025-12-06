Pow2 <- function(n){
  c <- 1
  while (2**c<=n){
    if(2**c==n){
      return(TRUE)
    }
    c <-c+1
  }
  return(FALSE)
}
Pow4 <- function(n){
  c <- 1
  while (4**c<=n){
    if(4**c==n){
      return(TRUE)
    }
    c <- c+1
  }
  return(FALSE)
}
Pow24 <- function(n) {
  if (n==1) {
    return(4)
  } 
  P2 <- Pow2(n)
  P4 <- Pow4(n)
  if(P2==TRUE && P4==TRUE){
    return(4)
  }else if (P2==TRUE && P4==FALSE){
    return(2)
  }else if (P2==FALSE && P4==TRUE){
    return(4)
  }else{
    return(0)
  }
}

