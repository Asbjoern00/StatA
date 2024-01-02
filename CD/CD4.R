library(MASS)
library(ggplot2)
library(dplyr)
library(gridExtra)
theme_set(theme_bw())
n <- 20000
xi <- c(0,1)
sigma <- matrix(c(1,1,1,4), nrow = 2)

#method 1
sim <- mvrnorm(n, xi, sigma)
X11 <- sim[,1]
X21 <- sim[,2]

#method 2 
X22 <- rnorm(n, xi[2], sqrt(sigma[2,2]))
X12 <- rnorm(n, 1/4*(X22-1), sqrt(3/4))

#method 3
X13 <- rnorm(n, xi[1], sqrt(sigma[1,1]))
X23 <- rnorm(n, 1 + X13, sqrt(3))


df_orig <- tibble(X1_m1 = X11,X1_m2 = X12, X1_m3 = X13, X2_m1 = X21,X2_m2 = X22, X2_m3 = X23)
df <- df_orig %>% mutate_all(sort)

#plots of marginals
grid.arrange(
df %>% ggplot(aes(x = X1_m1, y = X1_m2)) + geom_point() + geom_abline(),
df %>% ggplot(aes(x = X1_m1, y = X1_m3)) + geom_point() + geom_abline(),
df %>% ggplot(aes(x = X1_m2, y = X1_m3)) + geom_point() + geom_abline(),
df %>% ggplot(aes(x = X2_m1, y = X2_m2)) + geom_point() + geom_abline(),
df %>% ggplot(aes(x = X2_m1, y = X2_m3)) + geom_point() + geom_abline(),
df %>% ggplot(aes(x = X2_m2, y = X2_m3)) + geom_point() + geom_abline(),
nrow = 2
)
#Which all look correct. Now check against theoretical
grid.arrange(
  df %>% ggplot(aes(sample = X1_m1)) + geom_qq() + geom_qq_line(),
  df %>% ggplot(aes(sample = X1_m2)) + geom_qq() + geom_qq_line(),
  df %>% ggplot(aes(sample = X1_m3)) + geom_qq() + geom_qq_line(),
  df %>% ggplot(aes(sample = X2_m1)) + geom_qq() + geom_qq_line(),
  df %>% ggplot(aes(sample = X2_m2)) + geom_qq() + geom_qq_line(),
  df %>% ggplot(aes(sample = X2_m3)) + geom_qq() + geom_qq_line(),
  nrow = 2
)
#Which are also correct

#We check that the correlations are correct for the marginals:
#method 1
cov(X11,X11)
cov(X11,X21)
cov(X21,X21)

#method 2 
cov(X12,X12)
cov(X12,X22)
cov(X22,X22)

#method 3
cov(X13,X23)
cov(X13,X13)
cov(X23,X23)

#We can also do a scatter plot and overlay it with the density of the bivariate gaussian
data.grid <- expand.grid(s.1 = seq(-5, 5, length.out=200), s.2 = seq(-7, 7, length.out=200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = xi, sigma = sigma))
dense <- ggplot(q.samp, aes(x=s.1, y=s.2, z=prob)) + 
  geom_contour() +
  coord_fixed(xlim = c(-3, 3), ylim = c(-5, 5), ratio = 1) 

grid.arrange(
dense +  geom_point(data = df_orig, aes(x = X1_m1, y =X2_m1), inherit.aes = FALSE, alpha= 0.03),
dense +  geom_point(data = df_orig, aes(x = X1_m2, y =X2_m2), inherit.aes = FALSE, alpha= 0.03),
dense +  geom_point(data = df_orig, aes(x = X1_m3, y =X2_m3), inherit.aes = FALSE, alpha= 0.03),
ncol = 3
)
#All of which look very reasonable.


