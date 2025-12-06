CDP <- function(msg, n, m) {
  if(grepl(" |'", msg)) {
    stop("Erreur : le message contient un espace ou un guillemet simple.")
  }
  x <- unlist(strsplit(msg, ""))
  tableau <- length(x)
  n_lignes <- ceiling(tableau / m)
  ligne <- rep("", n_lignes)
  acc <- 1
  
  for(i in seq_along(ligne)) {
    for(j in 1:m) {
      if(acc <= length(x)) {
        ligne[i] <- paste0(ligne[i], x[acc])
        acc <- acc + 1
      }
    }
  }
  
  cat("Tableau (", n_lignes, "x", m, "):\n", sep = "")
  for(i in seq_along(ligne)) {
    chars <- unlist(strsplit(ligne[i], ""))
    cat(chars, "\n")
  }
  
  c <- ""
  for(j in 1:m) {
    for(i in seq_along(ligne)) {
      chars <- unlist(strsplit(ligne[i], ""))
      if(j <= length(chars)) {
        c <- paste0(c, chars[j])
      }
    }
  }
  
  return(c)
}

ChiffrerMessage <- function(msg, m) {
  n <- ceiling(nchar(msg) / m)
  resultat <- CDP(msg, n, m)
  return(resultat)
}

DechiffrerMessage <- function(msg, m){
  n <- ceiling(nchar(msg) / m)
  resultat <- CDP(msg, m, n)
  return(resultat)
}
