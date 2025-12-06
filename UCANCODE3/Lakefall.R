#your code goes here
PlusGrandLac <- function(n, v) {
  if (length(v) != n) return(c(0,0,0))
  best_vol <- 0
  best_l <- 0
  best_r <- 0
  
  max_gauche <- v[1]
  start <- 1
  vol <- 0
  for (i in 2:n) {
    if (v[i] < max_gauche) {
      vol <- vol + (max_gauche - v[i])
    } else {
      if (vol > best_vol) {
        best_vol <- vol
        best_l <- start
        best_r <- i
      }
      max_gauche <- v[i]
      start <- i
      vol <- 0
    }
  }
  max_droite <- v[n]
  end <- n
  vol <- 0
  for (i in (n-1):1) {
    if (v[i] < max_droite) {
      vol <- vol + (max_droite - v[i])
    } else {
      if (vol > best_vol) {
        best_vol <- vol
        best_l <- i
        best_r <- end
      }
      max_droite <- v[i]
      end <- i
      vol <- 0
    }
  }
  if (best_vol == 0) return(c(0,0,0))
  c(best_l, best_r, best_vol)
}

Parseur2 <- function(chemin) {
  stream <- file(chemin, "r")
  x <- scan(file=stream, what=integer(), quiet=TRUE)
  close(stream)
  
  t <- x[1]
  j <- 2
  for (i in seq_len(t)) {
    n <- x[j]
    v <- x[(j + 1):(j + n)]
    res <- PlusGrandLac(n, v)
    cat(res[1], res[2], res[3], "\n", sep=" ")
    j <- j + n + 1
  }
}
Parseur2("C:\\Users\\pc\\Documents\\fichierhisto.txt")
