ExFlot <- function(n, mode) {
  if (mode==2){
    if ( floor(n*100)%%2==0) {
      return((floor(n * 100) / 100))
    }else{
      n <- n+0.01
      return((floor(n * 100) / 100))
    }
  }
  else if (mode==0){
    if (n>0) {return(((floor(n * 100) / 100)))}
    else {return(((ceiling(n * 100) / 100)))}}
  else if (mode==1){ 
    if(n>0) {
      n <- n+0.01
      return((floor(n * 100) / 100))
    }
    else {return((ceiling(n * 100) / 100)) }}
  else if (mode==-1){
    if(n>0){
      return((floor(n * 100) / 100)) } 
    else {
      n <- n-0.01
      return((ceiling(n * 100) / 100))}
  }
  else{
    return(0)
  }
}
