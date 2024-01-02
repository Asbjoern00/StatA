# 2. 
# Draw simulated values from correct beta distributions. Since density factors, we can simulate from the joint posterior by simulating from the marginal betas
n0 <- 674
y0 <- 39
n1 <- 680
y1 <- 22
m <- 10000

p0 <- rbeta(m, y0+1, n0-y0+1)
p1 <- rbeta(m, y1+1, n1-y1+1)

# 3.
OR <- (p1/(1-p1)) / (p0/(1-p0))
mean(OR) # Posterior mean
quantile(OR, c(0.025,0.975)) # Posterior credible interval
# We see that 1 is not covered by the confidence interval, so we would say that there is less risk of death in the treatment group


#4. We saw that the marginal posteriors are Beta(y_i,n_i-y_i) hence
p0 <- rbeta(m, y0, n0-y0)
p1 <- rbeta(m, y1, n1-y1)

OR <- (p1/(1-p1)) / (p0/(1-p0))
mean(OR) # Posterior mean
quantile(OR, c(0.025,0.975)) # Posterior credible interval

#We see that essentially nothing changes here
