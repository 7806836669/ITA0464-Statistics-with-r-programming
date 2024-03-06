prime_numbers <- function(n) {
  if (n < 2) return(NULL)
  a <- rep(TRUE, n)
  a[1] <- FALSE
  for (i in seq(2, n)) {
    if (a[i]) {
      j <- i * i
      if (j > n) break
      a[seq(j, n, by=i)] <- FALSE
    }
  }
  return(which(a))
}

print(prime_numbers(100))