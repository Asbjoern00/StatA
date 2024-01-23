q <- function(x, beta = 3.3){
  (x*(1-x))^beta
}

res <- c()

while(length(res) < 2000){
  samp <- runif(1)
  alpha <- q(samp)
  U <- runif(1)
  if(U < alpha){
    res <- append(res, samp)
  }
}

true_samp <- rbeta(2000, 3.3, 3.3)

qqplot(res, true_samp)
