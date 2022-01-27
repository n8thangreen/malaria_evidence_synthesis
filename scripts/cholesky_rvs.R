
n <- 200
z <- matrix(rnorm(n*2, 0, 1), nrow = n, ncol = 2)
# covmat <- diag(2)
covmat <- matrix(c(1,   NA,
                   0.9, 1), ncol = 2, byrow = TRUE)
  
cholm <- t(chol(t(covmat)))             # lower triangle of the Cholesky decomposition

Tz <- NULL
for (i in 1:n) {
  Tz <- rbind(Tz, t(cholm %*% z[i, ]))
}

plot(Tz)

# range [0,1]
probs <- pnorm(Tz)
plot(probs)

# fed and dead positively correlated


