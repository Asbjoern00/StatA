
#Retrieves correlation within a single group for a model fitted with lmer
get_rho <- function(model,grp_name){
  frame <- as.data.frame(VarCorr(model)) %>% tibble()
  plank_var <- frame %>% filter(grp == grp_name) %>% select(vcov) %>% as.numeric()
  tot_var <- sum(frame$vcov)
  rho <- plank_var/tot_var
  return(rho)
}

#Bootstrap. model should be a lmer model, data should be a tibble/dataframe 
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