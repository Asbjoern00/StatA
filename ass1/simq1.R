run_experiment <- function(mu_0, mu_1, sigma_sq, n){
  
  #Get the optimal prediction rule
  pred_rule <- (mu_1 + mu_0)/2
  
  #Simulate from the marginal of Y
  Y <- rbinom(n, 1, 1/2)
  
  #Simulate from the conditional of X given Y
  mus <- Y*mu_1 + (1-Y)*mu_0
  X <- rnorm(n, mus, sqrt(sigma_sq))
  
  #Make prediction 
  pred <- (X >= pred_rule)
  
  #Return esitmated probability
  return(mean(pred == Y))
}
mu_0s <- c(1,1,1,2)
mu_1s <- c(1.5,2,1.5,2.5)
sigma_sqs <- c(1,1,2,1)
n <- 10000
estim_prob <- numeric(length(mu_0s))

for(i in seq_along(mu_0s)){
  estim_prob[i] <- run_experiment(mu_0s[i], mu_1s[i], sigma_sqs[i], n)
}
print(estim_prob)
