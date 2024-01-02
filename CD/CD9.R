n <- 10 
X <- runif(10^6)
Y <- rbinom(10^6, n, X)

mean(sqrt(Y/n) > X^2+X)
