h <- function(x,z){
  if(x == 0){
    if (z == 0){
      return(99/100 * (95/100)^2)
    }
    if (z == 1){
      return(99/100 * 95/1000)
    }
    if (z == 2){
      return(99/100*(5/100)^2)
    }
  }
  
  if(x == 1){
    if (z == 0){
      return(1/100*(5/100)^2)
    }
    if (z == 1){
      return(1/100 * 95/1000)
    }
    if (z == 2){
      return(1/100*(95/100)^2)
    }
  }
}

f <- function(x,z){
  h(x,z)/(h(0,z) + h(1,z))
}
#P(X = 1 | Z = 0)
f(1,0)
#P(X = 1 | Z = 1)
f(1,1)
#P(X = 1 | Z = 2)
f(1,2)


# Simulation. Simulate first 10^6 X from the marginal distribution
X <- rbinom(10^6, size = 1, p=0.01)

# Z is conditionally binomial with n = 2. If X is 0, Z is bin(2,0.05) and if X is 1, Z is bin(2,0.95)
prob_gen <-Vectorize( function(x){
  if (x == 0){
    return(0.05)
  }
  if (x == 1){
    return(0.95)
  }
}
)
ps <- prob_gen(X)
#Sample  Z
Z <- rbinom(10^6, size = 2, p = ps)

#Computing for instance an estimate of P(X = 1 | Z=2) amounts to filtering out the observations where Z = 2, and looking at the proportion (mean) of X's

mean(X[Z == 0]) - f(1,0)
mean(X[Z == 1]) - f(1,1)
mean(X[Z == 2]) - f(1,2)

#We see that all are very close to their theoretical values.