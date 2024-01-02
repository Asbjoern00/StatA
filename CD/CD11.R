library(ggplot2)
library(tidyr)
library(gridExtra)

inv_cdf_X <- function(U){
  U^(1/3)
}
inv_cdf_Y_x <- function(U,x){
  sqrt(x*U)
}
n <- 10^5
U <- runif(n)
U2 <- runif(n)
X <- inv_cdf_X(U)
Y <- inv_cdf_Y_x(U2,X)

bw <- 0.05

sim <- tibble(X = X, Y = Y)
#Comparte marginal desnsity of X and Y with the simulated. Looks fine except for some weird tail behavior for the simulated X's? 
grid.arrange(
sim %>% ggplot() + geom_function(fun = function(X) 3*X^2) +geom_density(aes(X)),
sim %>% ggplot() + geom_function(fun = function(Y) 3*Y*(1-Y^4)) + geom_density(aes(Y))
)

# Make scatterplot
sim %>% ggplot(aes(X,Y)) + geom_point(alpha = 0.01)

#Easily seen to represent the set A. Points more dense where both X and Y are large, which fits with how the density is defined.

#5,
mean(X < -log(Y))


#6.
#U <- runif(5000)
#X <- U^(1/3)
#Z <- sqrt(U*X)

# Z does not contain realistations of Y, since the same uniform variables are being used for the quantile transformation method.
# This introduces some weird dependence between X and Y, which is not the one specified in the problem. 