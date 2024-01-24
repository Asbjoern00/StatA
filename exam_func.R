####### Correlation retrieval utility

#Retrieves correlation within a single group for a model fitted with lmer
get_rho <- function(model,grp_name){
  frame <- as.data.frame(VarCorr(model)) %>% tibble()
  plank_var <- frame %>% filter(grp == grp_name) %>% select(vcov) %>% as.numeric()
  tot_var <- sum(frame$vcov)
  rho <- plank_var/tot_var
  return(rho)
}


#####GENERALISED BOOTSTRAP

#Bootstrap for lmer model. model should be a lmer model, data should be a tibble/dataframe 
#Statistic should be a fucntion taking a fitted lmer object and input.
bootstrap_lmer <- function(model, data, statistic ,B=1000){
  sims <- simulate(model, nsim = B)
  data_copy <- data
  stat_boot <- numeric(B)
  response <- all.vars(formula(model))[1]
  for (i in 1:B){
    data_copy[response] <- sims[[i]]
    mod_sim <- lmer(formula(model), data = data_copy)
    stat_boot[i] <- mod_sim %>% statistic()
  }
  return(stat_boot)
}

#######REJECTION SAMPLING

# Target/proposal should be a one-parameter function that evaluates the 'density' in a given point
# sample_dist should be a function that produces samples. 
#See example below function 
rejection_sample <- function(M, proposal, target, sample_dist, print_acceptance = TRUE){
  theta_star <- sample_dist(n = 1e6)
  U <- runif(1e6)
  alpha <- target(theta_star)/(M*proposal(theta_star))
  idx <- (U <= alpha)
  if(print_acceptance){
    cat("Acceptance rate is: ", sum(idx)/1e6)
  }
  return(theta_star[idx])
}


###### EXAMPLE
#q <- function(x, beta = 3.3){
#  (x*(1-x))^beta
#}
#res <- rejection_sample(M = 1, proposal = q, target = dunif, sample_dist = runif)
#true_samp <- rbeta(2000, 3.3, 3.3)
#qqplot(res, true_samp)


####### EXAMPLE 2
#q <- function(theta){ # proposal
#  exp(-theta^2/2)
#}
#g_2 <- function(theta){ #target
#  exp(-theta)
#}
#M <- exp(1/2) # M 
#samp_dist <- rexp()
#rejection_sample(M,q, g_2, samp_dist)
#df <- tibble(sample = res, density = sqrt(2/pi)*q(res)) # plots against actual density
#df %>% ggplot(aes(x = sample,y=density)) + geom_line() + geom_histogram(aes(sample, y =after_stat(density)), inherit.aes = FALSE) + theme_bw()

########### METROPOLIS-HASTINGS 1D
# Init value should be a scalar 
# init_dist, one dimensional density
# jump_sample, jump distribution to sample from 
# jump_dist, two parameter density. First parameter is the variable the density is evaluated in, second coordinate is the conditioning variable
metro_hast <- function(init_value, init_dist, jump_sample, jump_dist, n = 1e5){
  theta <- numeric(n)
  theta[1] <- init_value
  U <- runif(n)
  for (i in 2:n){
    theta_star <- jump_sample(theta[i-1])
    ratio <- (init_dist(theta_star)/init_dist(theta[i-1])) * (jump_dist(theta[i-1],theta_star)/jump_dist(theta_star,theta[i-1]))
    alpha <- min(c(ratio, 1))
    theta[i] <- (U[i] <= alpha)*theta_star + (U[i]>alpha)*theta[i-1]
  }
  return(theta)
}

## EXAMPLE, BS 12
#init_dist <- function(x){
#  dchisq(x, df = 10)
#}
#jump_sample <- function(x){
#  rchisq(1, df = x)
#}
#jump_dist <- function(value, condition){
#  dchisq(value, condition)
#}
#sims <- metro_hast(1,init_dist, jump_sample, jump_dist)
#qqplot(sort(tail(sims, 5000)), sort(rchisq(5000, 10)))



