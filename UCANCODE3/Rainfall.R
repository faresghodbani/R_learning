#your code goes here
histo <- function(n, v) {
  if (length(v) != n) return(0)
  res <- 0
  max_indice <- tail(which(v == max(v)), 1)
  
  if (max_indice > 1) {
    max_gauche <- v[1]
    for (i in 2:max_indice) {
      if (v[i] < max_gauche) res <- res + (max_gauche - v[i])
      else max_gauche <- v[i]
    }
  }
  
  if (max_indice < n) {
    max_droite <- v[n]
    for (i in (n-1):max_indice) {
      if (v[i] < max_droite) res <- res + (max_droite - v[i])
      else max_droite <- v[i]
    }
  }
  return(res)
}

Parseur1 <- function(chemin) {
  stream <- file(chemin, "r")
  x <- scan(file=stream, what=integer(), quiet=TRUE)
  close(stream)
  
  t <- x[1]
  j <- 2
  
  for (i in seq_len(t)) {
    n <- x[j]
    v <- x[(j + 1):(j + n)]
    cat(histo(n, v), "\n", sep="")
    j <- j + n + 1
  }
}
Parseur1("C:\\Users\\pc\\Documents\\fichierhisto.txt")
