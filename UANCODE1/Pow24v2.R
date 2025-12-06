Pow24 <- function(n) {
  a<-n
  if (n<=0) {return(0)}
  while (a%%4==0) {
    a<-(a%/%4)
  }
  if (a==1) {return(4)}
  else if (a==2){return(2)}
  else {return(0)}
}
