

library(MASS)
library(AER)
library(boot)


rm(list = ls(all=TRUE))

#set up everything for the simulations
set.seed(100)
source("mediation_pleiotropy.R")
n= 5000
reps = 1000


##3. Varying total effects and proportion mediated
params <- expand.grid(
  total_effect = c(0.2, 0.5, 1),
  proportion_mediated = c(0.05, 0.25, 0.75),
  beta2 = 0.2,
  pleiotropy = c("exposure","mediator")
)


resultsA <- NULL

for(i in 1:nrow(params))
{
  o <- mediationpleiotropy(n,reps,params$total_effect[i], params$proportion_mediated[i], params$beta2[i], params$pleiotropy[i])
  AA <- cbind(params[i,], o)
  resultsA <- rbind(resultsA, AA)
}


names <- c("total_effect", "proportion_mediated", "beta2", "pleiotropy", "beta1", "gamma", "mean_ols_b_e", "mean_mr_b_e", "mean_ols_s_e", "mean_ols_s_b", "mean_ols_s_eb_e", "mean_ols_s_eb_b", 
           "mean_mr_s_e", "mean_mr_s_b", "mean_mvmr_s_eb_e", "mean_mvmr_s_eb_b",
           "mean_ols_b_e*ols_s_b", "mean_ols_s_e - ols_s_eb_e", "mean_mr_b_e*mr_s_b", "mean_mr_s_e - mvmr_s_eb_e",
           "sd_ols_b_e", "sd_mr_b_e", "sd_ols_s_e", "sd_ols_s_b", "sd_ols_s_eb_e",
           "sd_ols_s_eb_b", "sd_mr_s_e", "sd_mr_s_b", "sd_mvmr_s_eb_e", "sd_mvmr_s_eb_b",
           "sd_ols_b_e*ols_s_b", "sd_ols_s_e - ols_s_eb_e", "sd_mr_b_e*mr_s_b", "sd_mr_s_e - mvmr_s_eb_e")

colnames(resultsA) <- names


write.csv(resultsA, file = "pleiotropy_results.csv", row.names = FALSE)


