ExFlot <- function(n, mode) {
  if (mode == 2) { 
    return(round(n, 2))
  }
  else if (mode == 0) { 
    return(trunc(n * 100) / 100)
  }
  else if (mode == 1) { 
    return(ceiling(n * 100) / 100)
  }
  else if (mode == -1) { 
    return(floor(n * 100) / 100)
  }
  else {
    return(0)
  }
}

