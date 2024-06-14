if (!requireNamespace("pracma", quietly = TRUE)) {
  install.packages("pracma")
}
library(pracma)


H <- function(alpha, k) {
  gamma1 <- gamma(1 + 1/k)
  gamma2 <- pgamma(gamma1^k, shape = 1/k, lower.tail = FALSE) * gamma(1/k)  
  return((2 * alpha / k) * gamma2)
}


alpha <- 1


k_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 40, 50, 100, 200, 500)

H_values <- sapply(k_values, function(k) H(alpha, k))
names(H_values) <- k_values

print(H_values)
