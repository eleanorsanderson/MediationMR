

library(MASS)
library(AER)
library(boot)
library(Rfast)


rm(list = ls(all=TRUE))

#set up everything for the simulations
set.seed(100)
source("multiple_mediators.R")
n= 5000
reps = 1000


#1. independent mediators

Mediators = "independent"

o <- mediationmultiple(n,reps,Mediators)
##n, reps,



results_ind_ols <- cbind(o$ols_s_e, o$ols_s_eb_e, o$ols_b_e*o$ols_s_eb_b, o$ols_m1_e*o$ols_s_eb_m1, o$ols_m2_e*o$ols_s_eb_m2, o$ols_b_e*o$ols_s_b, o$ols_m1_e*o$ols_s_m1, o$ols_m2_e*o$ols_s_m2)
p_med_ols <- cbind((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e, (o$ols_b_e*o$ols_s_b)/o$ols_s_e, (o$ols_m1_e*o$ols_s_m1)/o$ols_s_e, (o$ols_m2_e*o$ols_s_m2)/o$ols_s_e)
results_ind_ols <- cbind(results_ind_ols, p_med_ols)

results_ind_mr <- cbind(o$mr_s_e, o$mvmr_s_eb_e, o$mr_b_e*o$mvmr_s_eb_b, o$mr_m1_e*o$mvmr_s_eb_m1, o$mr_m2_e*o$mvmr_s_eb_m2, o$mr_b_e*o$mr_s_b, o$mr_m1_e*o$mr_s_m1, o$mr_m2_e*o$mr_s_m2)
p_med_mr <- cbind((o$mr_s_e-o$mr_s_eb_e)/o$mr_s_e, (o$mr_b_e*o$mr_s_b)/o$mr_s_e, (o$mr_m1_e*o$mr_s_m1)/o$mr_s_e, (o$mr_m2_e*o$mr_s_m2)/o$mr_s_e)
results_ind_mr <- cbind(results_ind_mr, p_med_mr)

results_ind <- colMeans(results_ind_ols)
mat <- as.matrix(results_ind_ols[,1:8])
sd <- colVars(mat, suma = NULL, std = TRUE)
results_ind <- rbind(results_ind, sd, colMeans(results_ind_mr))
mat <- as.matrix(results_ind_mr[,1:8])
sd <- colVars(mat, suma = NULL, std = TRUE)
results_ind <- rbind(results_ind, sd)


#2. related mediators

Mediators = "related"

o <- mediationmultiple(n,reps,Mediators)
##n, reps,


results_rel_ols <- cbind(o$ols_s_e, o$ols_s_eb_e, o$ols_b_e*o$ols_s_eb_b, o$ols_m1_e*o$ols_s_eb_m1, o$ols_m2_e*o$ols_s_eb_m2, o$ols_b_e*o$ols_s_b, o$ols_m1_e*o$ols_s_m1, o$ols_m2_e*o$ols_s_m2)
p_med_ols <- cbind((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e, (o$ols_b_e*o$ols_s_b)/o$ols_s_e, (o$ols_m1_e*o$ols_s_m1)/o$ols_s_e, (o$ols_m2_e*o$ols_s_m2)/o$ols_s_e)
results_rel_ols <- cbind(results_rel_ols, p_med_ols)

results_rel_mr <- cbind(o$mr_s_e, o$mvmr_s_eb_e, o$mr_b_e*o$mvmr_s_eb_b, o$mr_m1_e*o$mvmr_s_eb_m1, o$mr_m2_e*o$mvmr_s_eb_m2, o$mr_b_e*o$mr_s_b, o$mr_m1_e*o$mr_s_m1, o$mr_m2_e*o$mr_s_m2)
p_med_mr <- cbind((o$mr_s_e-o$mr_s_eb_e)/o$mr_s_e, (o$mr_b_e*o$mr_s_b)/o$mr_s_e, (o$mr_m1_e*o$mr_s_m1)/o$mr_s_e, (o$mr_m2_e*o$mr_s_m2)/o$mr_s_e)
results_rel_mr <- cbind(results_rel_mr, p_med_mr)


results_related <- colMeans(results_rel_ols)
mat <- as.matrix(results_rel_ols[,1:8])
sd <- colVars(mat, suma = NULL, std = TRUE)
results_related <- rbind(results_related, sd, colMeans(results_rel_mr))
mat <- as.matrix(results_rel_mr[,1:8])
sd <- colVars(mat, suma = NULL, std = TRUE)
results_related <- rbind(results_related, sd)


##output the results

names = c("total", "direct", "indirect_1_MVMR", "indirect_2_MVMR", "indirect_3_MVMR", "indirect_1_TS", "indirect_2_TS", "indirect_3_TS", "pm_difference", "pm_med1TS", "pm_med2TS", "pm_med3TS")
rows = c("ols", "ols_sd", "MR", "MR_sd")

colnames(results_ind) <- names
rownames(results_ind) <- rows

colnames(results_related) <- names
rownames(results_related) <- rows


write.csv(results_ind, file = "multiplemediators_independent.csv")
write.csv(results_related, file = "multiplemediators_related.csv")
