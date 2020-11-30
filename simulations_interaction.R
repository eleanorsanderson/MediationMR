

library(MASS)
library(AER)
library(boot)


rm(list = ls(all=TRUE))

#set up everything for the simulations
set.seed(100)
source("med_interaction_func.R")
n= 5000
reps = 1000


#1. varying interaction 
params <- expand.grid(
  interaction = c(0.05, 0.1, 0.2),
  continuous = TRUE
)

resultsA <- NULL

for(j in 1:nrow(params)){

o <- interactionresults(n,reps,params$interaction[j], params$continuous[j])

AA <- cbind(params[j,], o)
resultsA <- rbind(resultsA, AA)
                         
}
                         

names <- c("interaction", "continuous", "beta1", "beta3", "mean_ols_b_e", "mean_mr_b_e", "mean_ols_s_e", "mean_ols_s_b", "mean_ols_s_eb_e", "mean_ols_s_eb_b", 
           "mean_mr_s_e", "mean_mr_s_b", "mean_mvmr_s_eb_e", "mean_mvmr_s_eb_b",
           "mean_ols_b_e*ols_s_b", "mean_ols_s_e - ols_s_eb_e", "mean_mr_b_e*mr_s_b", "mean_mr_s_e - mvmr_s_eb_e",
           "sd_ols_b_e", "sd_mr_b_e", "sd_ols_s_e", "sd_ols_s_b", "sd_ols_s_eb_e",
           "sd_ols_s_eb_b", "sd_mr_s_e", "sd_mr_s_b", "sd_mvmr_s_eb_e", "sd_mvmr_s_eb_b",
           "sd_ols_b_e*ols_s_b", "sd_ols_s_e - ols_s_eb_e", "sd_mr_b_e*mr_s_b", "sd_mr_s_e - mvmr_s_eb_e")

colnames(resultsA) <- names


write.csv(resultsA, file = "continuous_interaction.csv", row.names = FALSE)

