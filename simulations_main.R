

library(MASS)
library(AER)
library(boot)


rm(list = ls(all=TRUE))

#set up everything for the simulations
set.seed(100)
source("mediation_function.R")
n= 5000
reps = 1000


#1. No mediaton 
scenario <- 1
params <- expand.grid(
  total_effect = 0.5,
  proportion_mediated =0,
  beta2 = 0.2
)
Meas_error = "NONE"
weakinstruments = "NONE"

o <- mediationresults(n,reps,params$total_effect, params$proportion_mediated, params$beta2, Meas_error, weakinstruments)
##n, reps, Tot, prop_med, beta2, M_error, weak
#M_error takes "exposure" or "mediator" otherwise gives no measurement error
#Weak takes "exposure" or "mediator" otherwise both instruments are strong

cont_results <- cbind(scenario, n, params$total_effect, params$proportion_mediated,  Meas_error, weakinstruments, mean(o$ols_s_e), sd(o$ols_s_e), mean(o$ols_s_eb_e), sd(o$ols_s_eb_e),  mean(o$ols_s_e-o$ols_s_eb_e), sd(o$ols_s_e-o$ols_s_eb_e), mean((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), sd((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), mean(o$ols_s_eb_b*o$ols_b_e), sd(o$ols_s_eb_b*o$ols_b_e),  mean((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e),  sd((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e), mean(o$mr_s_e), sd(o$mr_s_e), mean(o$mvmr_s_eb_e), sd(o$mvmr_s_eb_e), mean(o$mr_s_e-o$mvmr_s_eb_e), sd(o$mr_s_e - o$mvmr_s_eb_e), mean((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), sd((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), mean(o$mvmr_s_eb_b*o$mr_b_e), sd(o$mvmr_s_eb_b*o$mr_b_e), mean((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e), sd((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e))
highprev_results <- cbind(scenario, n, params$total_effect, params$proportion_mediated, Meas_error, weakinstruments, mean(o$ols_h_e), sd(o$ols_h_e), mean(o$ols_h_eb_e), sd(o$ols_h_eb_e),  mean(o$ols_h_e-o$ols_h_eb_e), sd(o$ols_h_e-o$ols_h_eb_e), mean((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), sd((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), mean(o$ols_h_eb_b*o$ols_b_e), sd(o$ols_h_eb_b*o$ols_b_e),  mean((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), sd((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), mean(o$mr_h_e), sd(o$mr_h_e), mean(o$mvmr_h_eb_e), sd(o$mvmr_h_eb_e), mean(o$mr_h_e-o$mvmr_h_eb_e), sd(o$mr_h_e - o$mvmr_h_eb_e), mean((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), sd((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), mean(o$mvmr_h_eb_b*o$mr_b_e), sd(o$mvmr_h_eb_b*o$mr_b_e), mean((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), sd((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), mean(o$mr_h_e_lor), sd(o$mr_h_e_lor), mean(o$mr_h_eb_e_lor), sd(o$mr_h_eb_e_lor), mean(o$mr_h_e_lor-o$mr_h_eb_e_lor),  sd(o$mr_h_e_lor - o$mr_h_eb_e_lor), mean((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor),  sd((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor), mean(o$mr_h_eb_b_lor*o$mr_b_e), sd(o$mr_h_eb_b_lor*o$mr_b_e), mean((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), sd((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), mean(o$mr_h_e_or), sd(o$mr_h_e_or), mean(o$mr_h_eb_e_or), sd(o$mr_h_eb_e_or), mean(o$mr_h_e_or-o$mr_h_eb_e_or), sd(o$mr_h_e_or - o$mr_h_eb_e_or), mean((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), sd((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), mean(o$mr_h_eb_b_or*o$mr_b_e), sd(o$mr_h_eb_b_or*o$mr_b_e), mean((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or), sd((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or))
lowprev_results <-  cbind(scenario, n, params$total_effect, params$proportion_mediated, Meas_error, weakinstruments,  mean(o$ols_c_e), sd(o$ols_c_e), mean(o$ols_c_eb_e), sd(o$ols_c_eb_e),  mean(o$ols_c_e-o$ols_c_eb_e), sd(o$ols_c_e-o$ols_c_eb_e), mean((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), sd((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), mean(o$ols_c_eb_b*o$ols_b_e), sd(o$ols_c_eb_b*o$ols_b_e),  mean((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e),  sd((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e), mean(o$mr_c_e), sd(o$mr_c_e), mean(o$mvmr_c_eb_e), sd(o$mvmr_c_eb_e), mean(o$mr_c_e-o$mvmr_c_eb_e), sd(o$mr_c_e - o$mvmr_c_eb_e), mean((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), sd((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), mean(o$mvmr_c_eb_b*o$mr_b_e), sd(o$mvmr_c_eb_b*o$mr_b_e), mean((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), sd((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), mean(o$mr_c_e_lor), sd(o$mr_c_e_lor), mean(o$mr_c_eb_e_lor), sd(o$mr_c_eb_e_lor), mean(o$mr_c_e_lor-o$mr_c_eb_e_lor), sd(o$mr_c_e_lor - o$mr_c_eb_e_lor), mean((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), sd((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), mean(o$mr_c_eb_b_lor*o$mr_b_e), sd(o$mr_c_eb_b_lor*o$mr_b_e), mean((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), sd((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), mean(o$mr_c_e_or), sd(o$mr_c_e_or), mean(o$mr_c_eb_e_or), sd(o$mr_c_eb_e_or), mean(o$mr_c_e_or-o$mr_c_eb_e_or), sd(o$mr_c_e_or - o$mr_c_eb_e_or), mean((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), sd((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), mean(o$mr_c_eb_b_or*o$mr_b_e), sd(o$mr_c_eb_b_or*o$mr_b_e), mean((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or), sd((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or))



#2. inconsistent mediation
scenario <- 2
params <- expand.grid(
  total_effect = 0.5,
  proportion_mediated = -0.5,
  beta2 = 0.2
)
Meas_error = "NONE"
weakinstruments = "NONE"

o <- mediationresults(n,reps,params$total_effect, params$proportion_mediated, params$beta2, Meas_error, weakinstruments)
##n, reps, Tot, prop_med, beta2, M_error, weak
#M_error takes "exposure" or "mediator" otherwise gives no measurement error
#Weak takes "exposure" or "mediator" otherwise both instruments are strong
cont_results <-  rbind(cont_results, cbind(scenario, n, params$total_effect, params$proportion_mediated,  Meas_error, weakinstruments, mean(o$ols_s_e), sd(o$ols_s_e), mean(o$ols_s_eb_e), sd(o$ols_s_eb_e),  mean(o$ols_s_e-o$ols_s_eb_e), sd(o$ols_s_e-o$ols_s_eb_e), mean((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), sd((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), mean(o$ols_s_eb_b*o$ols_b_e), sd(o$ols_s_eb_b*o$ols_b_e),  mean((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e),  sd((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e), mean(o$mr_s_e), sd(o$mr_s_e), mean(o$mvmr_s_eb_e), sd(o$mvmr_s_eb_e), mean(o$mr_s_e-o$mvmr_s_eb_e), sd(o$mr_s_e - o$mvmr_s_eb_e), mean((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), sd((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), mean(o$mvmr_s_eb_b*o$mr_b_e), sd(o$mvmr_s_eb_b*o$mr_b_e), mean((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e), sd((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e)))
highprev_results <-  rbind(highprev_results, cbind(scenario, n, params$total_effect, params$proportion_mediated, Meas_error, weakinstruments, mean(o$ols_h_e), sd(o$ols_h_e), mean(o$ols_h_eb_e), sd(o$ols_h_eb_e),  mean(o$ols_h_e-o$ols_h_eb_e), sd(o$ols_h_e-o$ols_h_eb_e), mean((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), sd((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), mean(o$ols_h_eb_b*o$ols_b_e), sd(o$ols_h_eb_b*o$ols_b_e),  mean((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), sd((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), mean(o$mr_h_e), sd(o$mr_h_e), mean(o$mvmr_h_eb_e), sd(o$mvmr_h_eb_e), mean(o$mr_h_e-o$mvmr_h_eb_e), sd(o$mr_h_e - o$mvmr_h_eb_e), mean((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), sd((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), mean(o$mvmr_h_eb_b*o$mr_b_e), sd(o$mvmr_h_eb_b*o$mr_b_e), mean((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), sd((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), mean(o$mr_h_e_lor), sd(o$mr_h_e_lor), mean(o$mr_h_eb_e_lor), sd(o$mr_h_eb_e_lor), mean(o$mr_h_e_lor-o$mr_h_eb_e_lor),  sd(o$mr_h_e_lor - o$mr_h_eb_e_lor), mean((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor),  sd((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor), mean(o$mr_h_eb_b_lor*o$mr_b_e), sd(o$mr_h_eb_b_lor*o$mr_b_e), mean((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), sd((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), mean(o$mr_h_e_or), sd(o$mr_h_e_or), mean(o$mr_h_eb_e_or), sd(o$mr_h_eb_e_or), mean(o$mr_h_e_or-o$mr_h_eb_e_or), sd(o$mr_h_e_or - o$mr_h_eb_e_or), mean((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), sd((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), mean(o$mr_h_eb_b_or*o$mr_b_e), sd(o$mr_h_eb_b_or*o$mr_b_e), mean((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or), sd((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or)))
lowprev_results <-   rbind(lowprev_results, cbind(scenario, n, params$total_effect, params$proportion_mediated, Meas_error, weakinstruments,  mean(o$ols_c_e), sd(o$ols_c_e), mean(o$ols_c_eb_e), sd(o$ols_c_eb_e),  mean(o$ols_c_e-o$ols_c_eb_e), sd(o$ols_c_e-o$ols_c_eb_e), mean((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), sd((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), mean(o$ols_c_eb_b*o$ols_b_e), sd(o$ols_c_eb_b*o$ols_b_e),  mean((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e),  sd((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e), mean(o$mr_c_e), sd(o$mr_c_e), mean(o$mvmr_c_eb_e), sd(o$mvmr_c_eb_e), mean(o$mr_c_e-o$mvmr_c_eb_e), sd(o$mr_c_e - o$mvmr_c_eb_e), mean((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), sd((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), mean(o$mvmr_c_eb_b*o$mr_b_e), sd(o$mvmr_c_eb_b*o$mr_b_e), mean((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), sd((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), mean(o$mr_c_e_lor), sd(o$mr_c_e_lor), mean(o$mr_c_eb_e_lor), sd(o$mr_c_eb_e_lor), mean(o$mr_c_e_lor-o$mr_c_eb_e_lor), sd(o$mr_c_e_lor - o$mr_c_eb_e_lor), mean((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), sd((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), mean(o$mr_c_eb_b_lor*o$mr_b_e), sd(o$mr_c_eb_b_lor*o$mr_b_e), mean((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), sd((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), mean(o$mr_c_e_or), sd(o$mr_c_e_or), mean(o$mr_c_eb_e_or), sd(o$mr_c_eb_e_or), mean(o$mr_c_e_or-o$mr_c_eb_e_or), sd(o$mr_c_e_or - o$mr_c_eb_e_or), mean((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), sd((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), mean(o$mr_c_eb_b_or*o$mr_b_e), sd(o$mr_c_eb_b_or*o$mr_b_e), mean((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or), sd((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or)))



##3. Varying total effects and proportion mediated
scenario <- 3
params <- expand.grid(
  total_effect = c(0, 0.2, 0.5, 1),
  proportion_mediated = c(0.05, 0.25, 0.75),
  beta2 = 0.2
)

for(i in 1:nrow(params))
{
  
  o <- mediationresults(n,reps,params$total_effect[i], params$proportion_mediated[i], params$beta2[i],  Meas_error, weakinstruments)
  ##n, reps, Tot, prop_med, beta2, M_error, weak
  #M_error takes "exposure" or "mediator" otherwise gives no measurement error
  #Weak takes "exposure" or "mediator" otherwise both instruments are strong
  cont_results <- rbind(cont_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i],  Meas_error, weakinstruments, mean(o$ols_s_e), sd(o$ols_s_e), mean(o$ols_s_eb_e), sd(o$ols_s_eb_e),  mean(o$ols_s_e-o$ols_s_eb_e), sd(o$ols_s_e-o$ols_s_eb_e), mean((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), sd((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), mean(o$ols_s_eb_b*o$ols_b_e), sd(o$ols_s_eb_b*o$ols_b_e),  mean((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e),  sd((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e), mean(o$mr_s_e), sd(o$mr_s_e), mean(o$mvmr_s_eb_e), sd(o$mvmr_s_eb_e), mean(o$mr_s_e-o$mvmr_s_eb_e), sd(o$mr_s_e - o$mvmr_s_eb_e), mean((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), sd((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), mean(o$mvmr_s_eb_b*o$mr_b_e), sd(o$mvmr_s_eb_b*o$mr_b_e), mean((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e), sd((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e)))
  highprev_results <- rbind(highprev_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i], Meas_error, weakinstruments, mean(o$ols_h_e), sd(o$ols_h_e), mean(o$ols_h_eb_e), sd(o$ols_h_eb_e),  mean(o$ols_h_e-o$ols_h_eb_e), sd(o$ols_h_e-o$ols_h_eb_e), mean((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), sd((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), mean(o$ols_h_eb_b*o$ols_b_e), sd(o$ols_h_eb_b*o$ols_b_e),  mean((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), sd((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), mean(o$mr_h_e), sd(o$mr_h_e), mean(o$mvmr_h_eb_e), sd(o$mvmr_h_eb_e), mean(o$mr_h_e-o$mvmr_h_eb_e), sd(o$mr_h_e - o$mvmr_h_eb_e), mean((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), sd((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), mean(o$mvmr_h_eb_b*o$mr_b_e), sd(o$mvmr_h_eb_b*o$mr_b_e), mean((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), sd((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), mean(o$mr_h_e_lor), sd(o$mr_h_e_lor), mean(o$mr_h_eb_e_lor), sd(o$mr_h_eb_e_lor), mean(o$mr_h_e_lor-o$mr_h_eb_e_lor),  sd(o$mr_h_e_lor - o$mr_h_eb_e_lor), mean((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor),  sd((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor), mean(o$mr_h_eb_b_lor*o$mr_b_e), sd(o$mr_h_eb_b_lor*o$mr_b_e), mean((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), sd((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), mean(o$mr_h_e_or), sd(o$mr_h_e_or), mean(o$mr_h_eb_e_or), sd(o$mr_h_eb_e_or), mean(o$mr_h_e_or-o$mr_h_eb_e_or), sd(o$mr_h_e_or - o$mr_h_eb_e_or), mean((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), sd((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), mean(o$mr_h_eb_b_or*o$mr_b_e), sd(o$mr_h_eb_b_or*o$mr_b_e), mean((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or), sd((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or)))
  lowprev_results <- rbind(lowprev_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i], Meas_error, weakinstruments,  mean(o$ols_c_e), sd(o$ols_c_e), mean(o$ols_c_eb_e), sd(o$ols_c_eb_e),  mean(o$ols_c_e-o$ols_c_eb_e), sd(o$ols_c_e-o$ols_c_eb_e), mean((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), sd((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), mean(o$ols_c_eb_b*o$ols_b_e), sd(o$ols_c_eb_b*o$ols_b_e),  mean((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e),  sd((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e), mean(o$mr_c_e), sd(o$mr_c_e), mean(o$mvmr_c_eb_e), sd(o$mvmr_c_eb_e), mean(o$mr_c_e-o$mvmr_c_eb_e), sd(o$mr_c_e - o$mvmr_c_eb_e), mean((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), sd((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), mean(o$mvmr_c_eb_b*o$mr_b_e), sd(o$mvmr_c_eb_b*o$mr_b_e), mean((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), sd((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), mean(o$mr_c_e_lor), sd(o$mr_c_e_lor), mean(o$mr_c_eb_e_lor), sd(o$mr_c_eb_e_lor), mean(o$mr_c_e_lor-o$mr_c_eb_e_lor), sd(o$mr_c_e_lor - o$mr_c_eb_e_lor), mean((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), sd((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), mean(o$mr_c_eb_b_lor*o$mr_b_e), sd(o$mr_c_eb_b_lor*o$mr_b_e), mean((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), sd((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), mean(o$mr_c_e_or), sd(o$mr_c_e_or), mean(o$mr_c_eb_e_or), sd(o$mr_c_eb_e_or), mean(o$mr_c_e_or-o$mr_c_eb_e_or), sd(o$mr_c_e_or - o$mr_c_eb_e_or), mean((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), sd((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), mean(o$mr_c_eb_b_or*o$mr_b_e), sd(o$mr_c_eb_b_or*o$mr_b_e), mean((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or), sd((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or)))
  
}


##4. small total effect that still crosses the null
scenario <- 4
params <- expand.grid(
  total_effect = c(0.01, 0.05, 0.1),
  proportion_mediated = c(0.05, 0.25, 0.75),
  beta2 = 0.2
)

for(i in 1:nrow(params))
{
  
  o <- mediationresults(n,reps,params$total_effect[i], params$proportion_mediated[i], params$beta2[i],  Meas_error, weakinstruments)
  ##n, reps, Tot, prop_med, beta2, M_error, weak
  #M_error takes "exposure" or "mediator" otherwise gives no measurement error
  #Weak takes "exposure" or "mediator" otherwise both instruments are strong
  cont_results <- rbind(cont_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i],  Meas_error, weakinstruments, mean(o$ols_s_e), sd(o$ols_s_e), mean(o$ols_s_eb_e), sd(o$ols_s_eb_e),  mean(o$ols_s_e-o$ols_s_eb_e), sd(o$ols_s_e-o$ols_s_eb_e), mean((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), sd((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), mean(o$ols_s_eb_b*o$ols_b_e), sd(o$ols_s_eb_b*o$ols_b_e),  mean((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e),  sd((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e), mean(o$mr_s_e), sd(o$mr_s_e), mean(o$mvmr_s_eb_e), sd(o$mvmr_s_eb_e), mean(o$mr_s_e-o$mvmr_s_eb_e), sd(o$mr_s_e - o$mvmr_s_eb_e), mean((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), sd((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), mean(o$mvmr_s_eb_b*o$mr_b_e), sd(o$mvmr_s_eb_b*o$mr_b_e), mean((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e), sd((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e)))
  highprev_results <- rbind(highprev_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i], Meas_error, weakinstruments, mean(o$ols_h_e), sd(o$ols_h_e), mean(o$ols_h_eb_e), sd(o$ols_h_eb_e),  mean(o$ols_h_e-o$ols_h_eb_e), sd(o$ols_h_e-o$ols_h_eb_e), mean((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), sd((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), mean(o$ols_h_eb_b*o$ols_b_e), sd(o$ols_h_eb_b*o$ols_b_e),  mean((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), sd((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), mean(o$mr_h_e), sd(o$mr_h_e), mean(o$mvmr_h_eb_e), sd(o$mvmr_h_eb_e), mean(o$mr_h_e-o$mvmr_h_eb_e), sd(o$mr_h_e - o$mvmr_h_eb_e), mean((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), sd((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), mean(o$mvmr_h_eb_b*o$mr_b_e), sd(o$mvmr_h_eb_b*o$mr_b_e), mean((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), sd((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), mean(o$mr_h_e_lor), sd(o$mr_h_e_lor), mean(o$mr_h_eb_e_lor), sd(o$mr_h_eb_e_lor), mean(o$mr_h_e_lor-o$mr_h_eb_e_lor),  sd(o$mr_h_e_lor - o$mr_h_eb_e_lor), mean((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor),  sd((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor), mean(o$mr_h_eb_b_lor*o$mr_b_e), sd(o$mr_h_eb_b_lor*o$mr_b_e), mean((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), sd((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), mean(o$mr_h_e_or), sd(o$mr_h_e_or), mean(o$mr_h_eb_e_or), sd(o$mr_h_eb_e_or), mean(o$mr_h_e_or-o$mr_h_eb_e_or), sd(o$mr_h_e_or - o$mr_h_eb_e_or), mean((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), sd((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), mean(o$mr_h_eb_b_or*o$mr_b_e), sd(o$mr_h_eb_b_or*o$mr_b_e), mean((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or), sd((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or)))
  lowprev_results <- rbind(lowprev_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i], Meas_error, weakinstruments,  mean(o$ols_c_e), sd(o$ols_c_e), mean(o$ols_c_eb_e), sd(o$ols_c_eb_e),  mean(o$ols_c_e-o$ols_c_eb_e), sd(o$ols_c_e-o$ols_c_eb_e), mean((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), sd((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), mean(o$ols_c_eb_b*o$ols_b_e), sd(o$ols_c_eb_b*o$ols_b_e),  mean((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e),  sd((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e), mean(o$mr_c_e), sd(o$mr_c_e), mean(o$mvmr_c_eb_e), sd(o$mvmr_c_eb_e), mean(o$mr_c_e-o$mvmr_c_eb_e), sd(o$mr_c_e - o$mvmr_c_eb_e), mean((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), sd((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), mean(o$mvmr_c_eb_b*o$mr_b_e), sd(o$mvmr_c_eb_b*o$mr_b_e), mean((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), sd((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), mean(o$mr_c_e_lor), sd(o$mr_c_e_lor), mean(o$mr_c_eb_e_lor), sd(o$mr_c_eb_e_lor), mean(o$mr_c_e_lor-o$mr_c_eb_e_lor), sd(o$mr_c_e_lor - o$mr_c_eb_e_lor), mean((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), sd((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), mean(o$mr_c_eb_b_lor*o$mr_b_e), sd(o$mr_c_eb_b_lor*o$mr_b_e), mean((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), sd((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), mean(o$mr_c_e_or), sd(o$mr_c_e_or), mean(o$mr_c_eb_e_or), sd(o$mr_c_eb_e_or), mean(o$mr_c_e_or-o$mr_c_eb_e_or), sd(o$mr_c_e_or - o$mr_c_eb_e_or), mean((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), sd((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), mean(o$mr_c_eb_b_or*o$mr_b_e), sd(o$mr_c_eb_b_or*o$mr_b_e), mean((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or), sd((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or)))
  
  
}


#5. smaller sample size and wider confidence intervals
n=1000
source("mediation_function_CI.r")
scenario <- 5
params <- expand.grid(
  total_effect = 0.2,
  proportion_mediated = c(0.05, 0.25, 0.75),
  beta2 = 0.2
)


for(i in 1:nrow(params))
{
  
  o <- mediationresults(n,reps,params$total_effect[i], params$proportion_mediated[i], params$beta2[i],  Meas_error, weakinstruments)
  ##n, reps, Tot, prop_med, beta2, M_error, weak
  #M_error takes "exposure" or "mediator" otherwise gives no measurement error
  #Weak takes "exposure" or "mediator" otherwise both instruments are strong
  cont_results <- rbind(cont_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i],  Meas_error, weakinstruments, mean(o$ols_s_e), sd(o$ols_s_e), mean(o$ols_s_eb_e), sd(o$ols_s_eb_e),  mean(o$ols_s_e-o$ols_s_eb_e), sd(o$ols_s_e-o$ols_s_eb_e), mean((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), sd((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), mean(o$ols_s_eb_b*o$ols_b_e), sd(o$ols_s_eb_b*o$ols_b_e),  mean((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e),  sd((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e), mean(o$mr_s_e), sd(o$mr_s_e), mean(o$mvmr_s_eb_e), sd(o$mvmr_s_eb_e), mean(o$mr_s_e-o$mvmr_s_eb_e), sd(o$mr_s_e - o$mvmr_s_eb_e), mean((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), sd((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), mean(o$mvmr_s_eb_b*o$mr_b_e), sd(o$mvmr_s_eb_b*o$mr_b_e), mean((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e), sd((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e)))
  highprev_results <- rbind(highprev_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i], Meas_error, weakinstruments, mean(o$ols_h_e), sd(o$ols_h_e), mean(o$ols_h_eb_e), sd(o$ols_h_eb_e),  mean(o$ols_h_e-o$ols_h_eb_e), sd(o$ols_h_e-o$ols_h_eb_e), mean((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), sd((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), mean(o$ols_h_eb_b*o$ols_b_e), sd(o$ols_h_eb_b*o$ols_b_e),  mean((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), sd((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), mean(o$mr_h_e), sd(o$mr_h_e), mean(o$mvmr_h_eb_e), sd(o$mvmr_h_eb_e), mean(o$mr_h_e-o$mvmr_h_eb_e), sd(o$mr_h_e - o$mvmr_h_eb_e), mean((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), sd((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), mean(o$mvmr_h_eb_b*o$mr_b_e), sd(o$mvmr_h_eb_b*o$mr_b_e), mean((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), sd((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), mean(o$mr_h_e_lor), sd(o$mr_h_e_lor), mean(o$mr_h_eb_e_lor), sd(o$mr_h_eb_e_lor), mean(o$mr_h_e_lor-o$mr_h_eb_e_lor),  sd(o$mr_h_e_lor - o$mr_h_eb_e_lor), mean((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor),  sd((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor), mean(o$mr_h_eb_b_lor*o$mr_b_e), sd(o$mr_h_eb_b_lor*o$mr_b_e), mean((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), sd((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), mean(o$mr_h_e_or), sd(o$mr_h_e_or), mean(o$mr_h_eb_e_or), sd(o$mr_h_eb_e_or), mean(o$mr_h_e_or-o$mr_h_eb_e_or), sd(o$mr_h_e_or - o$mr_h_eb_e_or), mean((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), sd((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), mean(o$mr_h_eb_b_or*o$mr_b_e), sd(o$mr_h_eb_b_or*o$mr_b_e), mean((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or), sd((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or)))
  lowprev_results <- rbind(lowprev_results, cbind(scenario, n, params$total_effect[i], params$proportion_mediated[i], Meas_error, weakinstruments,  mean(o$ols_c_e), sd(o$ols_c_e), mean(o$ols_c_eb_e), sd(o$ols_c_eb_e),  mean(o$ols_c_e-o$ols_c_eb_e), sd(o$ols_c_e-o$ols_c_eb_e), mean((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), sd((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), mean(o$ols_c_eb_b*o$ols_b_e), sd(o$ols_c_eb_b*o$ols_b_e),  mean((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e),  sd((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e), mean(o$mr_c_e), sd(o$mr_c_e), mean(o$mvmr_c_eb_e), sd(o$mvmr_c_eb_e), mean(o$mr_c_e-o$mvmr_c_eb_e), sd(o$mr_c_e - o$mvmr_c_eb_e), mean((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), sd((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), mean(o$mvmr_c_eb_b*o$mr_b_e), sd(o$mvmr_c_eb_b*o$mr_b_e), mean((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), sd((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), mean(o$mr_c_e_lor), sd(o$mr_c_e_lor), mean(o$mr_c_eb_e_lor), sd(o$mr_c_eb_e_lor), mean(o$mr_c_e_lor-o$mr_c_eb_e_lor), sd(o$mr_c_e_lor - o$mr_c_eb_e_lor), mean((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), sd((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), mean(o$mr_c_eb_b_lor*o$mr_b_e), sd(o$mr_c_eb_b_lor*o$mr_b_e), mean((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), sd((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), mean(o$mr_c_e_or), sd(o$mr_c_e_or), mean(o$mr_c_eb_e_or), sd(o$mr_c_eb_e_or), mean(o$mr_c_e_or-o$mr_c_eb_e_or), sd(o$mr_c_e_or - o$mr_c_eb_e_or), mean((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), sd((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), mean(o$mr_c_eb_b_or*o$mr_b_e), sd(o$mr_c_eb_b_or*o$mr_b_e), mean((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or), sd((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or)))
  
}



##6. Measurement error
scenario <- 6
n=5000
total_effect = 0.5
proportion_mediated = 0.25
beta2 = 0.2
weakinstruments = "NONE"

Meas_error = c("exposure", "mediator")

for(i in 1:2)
{
  o <- mediationresults(n,reps, total_effect ,  proportion_mediated ,  beta2 ,  Meas_error[i], weakinstruments)
  ##n, reps, Tot, prop_med, beta2, M_error, weak
  #M_error takes "exposure" or "mediator" otherwise gives no measurement error
  #Weak takes "exposure" or "mediator" otherwise both instruments are strong
  cont_results <- rbind(cont_results, cbind(scenario, n, total_effect, proportion_mediated,  Meas_error[i], weakinstruments, mean(o$ols_s_e), sd(o$ols_s_e), mean(o$ols_s_eb_e), sd(o$ols_s_eb_e),  mean(o$ols_s_e-o$ols_s_eb_e), sd(o$ols_s_e-o$ols_s_eb_e), mean((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), sd((o$ols_s_e-o$ols_s_eb_e)/o$ols_s_e), mean(o$ols_s_eb_b*o$ols_b_e), sd(o$ols_s_eb_b*o$ols_b_e),  mean((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e),  sd((o$ols_s_eb_b*o$ols_b_e)/o$ols_s_e), mean(o$mr_s_e), sd(o$mr_s_e), mean(o$mvmr_s_eb_e), sd(o$mvmr_s_eb_e), mean(o$mr_s_e-o$mvmr_s_eb_e), sd(o$mr_s_e - o$mvmr_s_eb_e), mean((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), sd((o$mr_s_e-o$mvmr_s_eb_e)/o$mr_s_e), mean(o$mvmr_s_eb_b*o$mr_b_e), sd(o$mvmr_s_eb_b*o$mr_b_e), mean((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e), sd((o$mvmr_s_eb_b*o$mr_b_e)/o$mr_s_e)))
  highprev_results <- rbind(highprev_results, cbind(scenario, n, total_effect, proportion_mediated, Meas_error[i], weakinstruments, mean(o$ols_h_e), sd(o$ols_h_e), mean(o$ols_h_eb_e), sd(o$ols_h_eb_e),  mean(o$ols_h_e-o$ols_h_eb_e), sd(o$ols_h_e-o$ols_h_eb_e), mean((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), sd((o$ols_h_e-o$ols_h_eb_e)/o$ols_h_e), mean(o$ols_h_eb_b*o$ols_b_e), sd(o$ols_h_eb_b*o$ols_b_e),  mean((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), sd((o$ols_h_eb_b*o$ols_b_e)/o$ols_h_e), mean(o$mr_h_e), sd(o$mr_h_e), mean(o$mvmr_h_eb_e), sd(o$mvmr_h_eb_e), mean(o$mr_h_e-o$mvmr_h_eb_e), sd(o$mr_h_e - o$mvmr_h_eb_e), mean((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), sd((o$mr_h_e-o$mvmr_h_eb_e)/o$mr_h_e), mean(o$mvmr_h_eb_b*o$mr_b_e), sd(o$mvmr_h_eb_b*o$mr_b_e), mean((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), sd((o$mvmr_h_eb_b*o$mr_b_e)/o$mr_h_e), mean(o$mr_h_e_lor), sd(o$mr_h_e_lor), mean(o$mr_h_eb_e_lor), sd(o$mr_h_eb_e_lor), mean(o$mr_h_e_lor-o$mr_h_eb_e_lor),  sd(o$mr_h_e_lor - o$mr_h_eb_e_lor), mean((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor),  sd((o$mr_h_e_lor-o$mr_h_eb_e_lor)/o$mr_h_e_lor), mean(o$mr_h_eb_b_lor*o$mr_b_e), sd(o$mr_h_eb_b_lor*o$mr_b_e), mean((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), sd((o$mr_h_eb_b_lor*o$mr_b_e)/o$mr_h_e_lor), mean(o$mr_h_e_or), sd(o$mr_h_e_or), mean(o$mr_h_eb_e_or), sd(o$mr_h_eb_e_or), mean(o$mr_h_e_or-o$mr_h_eb_e_or), sd(o$mr_h_e_or - o$mr_h_eb_e_or), mean((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), sd((o$mr_h_e_or-o$mr_h_eb_e_or)/o$mr_h_e_or), mean(o$mr_h_eb_b_or*o$mr_b_e), sd(o$mr_h_eb_b_or*o$mr_b_e), mean((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or), sd((o$mr_h_eb_b_or*o$mr_b_e)/o$mr_h_e_or)))
  lowprev_results <- rbind(lowprev_results, cbind(scenario, n, total_effect, proportion_mediated, Meas_error[i], weakinstruments,  mean(o$ols_c_e), sd(o$ols_c_e), mean(o$ols_c_eb_e), sd(o$ols_c_eb_e),  mean(o$ols_c_e-o$ols_c_eb_e), sd(o$ols_c_e-o$ols_c_eb_e), mean((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), sd((o$ols_c_e-o$ols_c_eb_e)/o$ols_c_e), mean(o$ols_c_eb_b*o$ols_b_e), sd(o$ols_c_eb_b*o$ols_b_e),  mean((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e),  sd((o$ols_c_eb_b*o$ols_b_e)/o$ols_c_e), mean(o$mr_c_e), sd(o$mr_c_e), mean(o$mvmr_c_eb_e), sd(o$mvmr_c_eb_e), mean(o$mr_c_e-o$mvmr_c_eb_e), sd(o$mr_c_e - o$mvmr_c_eb_e), mean((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), sd((o$mr_c_e-o$mvmr_c_eb_e)/o$mr_c_e), mean(o$mvmr_c_eb_b*o$mr_b_e), sd(o$mvmr_c_eb_b*o$mr_b_e), mean((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), sd((o$mvmr_c_eb_b*o$mr_b_e)/o$mr_c_e), mean(o$mr_c_e_lor), sd(o$mr_c_e_lor), mean(o$mr_c_eb_e_lor), sd(o$mr_c_eb_e_lor), mean(o$mr_c_e_lor-o$mr_c_eb_e_lor), sd(o$mr_c_e_lor - o$mr_c_eb_e_lor), mean((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), sd((o$mr_c_e_lor-o$mr_c_eb_e_lor)/o$mr_c_e_lor), mean(o$mr_c_eb_b_lor*o$mr_b_e), sd(o$mr_c_eb_b_lor*o$mr_b_e), mean((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), sd((o$mr_c_eb_b_lor*o$mr_b_e)/o$mr_c_e_lor), mean(o$mr_c_e_or), sd(o$mr_c_e_or), mean(o$mr_c_eb_e_or), sd(o$mr_c_eb_e_or), mean(o$mr_c_e_or-o$mr_c_eb_e_or), sd(o$mr_c_e_or - o$mr_c_eb_e_or), mean((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), sd((o$mr_c_e_or-o$mr_c_eb_e_or)/o$mr_c_e_or), mean(o$mr_c_eb_b_or*o$mr_b_e), sd(o$mr_c_eb_b_or*o$mr_b_e), mean((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or), sd((o$mr_c_eb_b_or*o$mr_b_e)/o$mr_c_e_or)))
}


continuous <- data.frame(cont_results)
highprevelance <- data.frame(highprev_results)
lowprevelance <- data.frame(lowprev_results)
names <- c("scenario", "sample_size", "total_effect",  "proportion_mediated" ," Meas_error", "weakinstruments"," total", "sd_total", "direct", "sd_direct",  "indirect_diff", "sd_indirect_diff", "prop_med_diff","sd_prop_med_diff", "indirect_prod", "sd_indirect_prod",  "prop_med_prod", "sd_prop_med_prod", "mr_total", "sd_mr_total", "mr_direct", "sd_mr_direct", "mr_indirect_diff", "sd_mr_indirect_diff", "mr_prop_mediated_diff",  "sdmr_prop_mediated_diff", "mr_indirect_prod", "sd_mr_indirect_prod", "mr_prop_mediated_prod", "sdmr_prop_mediated_prod")

names_lor <- c("mr_total_lor", "sd_mr_total_lor", "mr_direct_lor", "sd_mr_direct_lor", "mr_indirect_diff_lor", "sd_mr_indirect_diff_lor", "mr_prop_mediated_diff_lor", "sdmr_prop_mediated_diff_lor", "mr_indirect_prod_lor", "sd_mr_indirect_prod_lor", "mr_prop_mediated_prod_lor", "sdmr_prop_mediated_prod_lor")
names_or <- c("mr_total_or", "sd_mr_total_or", "mr_direct_or", "sd_mr_direct_or", "mr_indirect_diff_or", "sd_mr_indirect_diff_or", "mr_prop_mediated_diff_or", "sdmr_prop_mediated_diff_or", "mr_indirect_prod_or", "sd_mr_indirect_prod_or", "mr_prop_mediated_prod_or", "sdmr_prop_mediated_prod_or")

colnames(continuous) <- names
colnames(highprevelance) <- c(names, names_lor, names_or)
colnames(lowprevelance) <-  c(names, names_lor, names_or)

write.csv(continuous, file = "continuous_results.csv", row.names = FALSE)
write.csv(highprevelance, file = "highprevelance_results.csv", row.names = FALSE)
write.csv(lowprevelance, file = "lowprevelance_results.csv", row.names = FALSE)

