

mediationpleiotropy <- function(n, reps, Tot, prop_med, beta2, pleio)
{

  
    pi1 = 1.0
    pi2 = 1.0
  
  
  gamma = (Tot*prop_med)/beta2
  beta1 = Tot - gamma*beta2
  

##Data set up

s2_bp <- 1
s2_e <- 1
s2_bmi <- 1
s_bp_e <- 1
s_bp_b <- 1
s_e_b <- 1

ols_b_e = mr_b_e = NULL
ols_s_e = ols_s_b = ols_s_eb_e = ols_s_eb_b = mr_s_e = mr_s_b = mvmr_s_eb_e = mvmr_s_eb_b = NULL
ols_h_e = ols_h_b = ols_h_eb_e = ols_h_eb_b = mr_h_e = mr_h_b = mvmr_h_eb_e = mvmr_h_eb_b = NULL
ols_c_e = ols_c_b = ols_c_eb_e = ols_c_eb_b = mr_c_e = mr_c_b = mvmr_c_eb_e = mvmr_c_eb_b = NULL
mr_h_e_lor = mr_h_b_lor = mr_h_eb_e_lor = mr_h_eb_b_lor = NULL
mr_c_e_lor = mr_c_b_lor = mr_c_eb_e_lor = mr_c_eb_b_lor = NULL
mr_h_e_or = mr_h_b_or = mr_h_eb_e_or = mr_h_eb_b_or = NULL
mr_c_e_or = mr_c_b_or = mr_c_eb_e_or = mr_c_eb_b_or = NULL



for (i in 1:reps) {

PGRS1 <- rnorm(n,0,1)
PGRS1b <- rnorm(n,0,1)
PGRS2 <- rnorm(n,0,1)
PGRS2b <- rnorm(n,0,1)


mu = c(0,0,0)
s = matrix(c(s2_bp, s_bp_e, s_bp_b, s_bp_e, s2_e, s_e_b, s_bp_b, s_e_b, s2_bmi), ncol = 3)
v = mvrnorm(n, mu, s)


Edu_true <-  pi1*PGRS1 + v[,2]
Edu <- Edu_true
BMI_true <-  gamma*Edu_true + pi2*PGRS2 + v[,3]
BMI <- BMI_true

if(pleio == "exposure"){
sbp <- (beta1)*Edu_true + beta2*BMI_true + 0.2*PGRS1 + v[,1]
}
if(pleio == "mediator"){
  sbp <- (beta1)*Edu_true + beta2*BMI_true + 0.2*PGRS2 + v[,1]
}

cutoff <- quantile(sbp,0.75)
hyp <- as.numeric(sbp>cutoff)

cutoff_l <- quantile(sbp, 0.95)
low <- as.numeric(sbp>cutoff_l)

##Get the regression output

ols_b_e[i] <- lm(BMI~Edu)$coef[2]
mr_b_e[i] <- ivreg(BMI~Edu|PGRS1)$coef[2]

ols_s_e[i] <- lm(sbp ~ Edu)$coef[2]

ols_s_b[i] <- lm(sbp ~ BMI)$coef[2]
ols_s_eb_e[i] <- lm(sbp ~ Edu + BMI)$coef[2]
ols_s_eb_b[i] <- lm(sbp ~ Edu + BMI)$coef[3]

mr_s_e[i] <- ivreg(sbp ~ Edu|PGRS1)$coef[2]

mr_s_b[i] <- ivreg(sbp ~ BMI|PGRS2)$coef[2]
mvmr_s_eb_e[i] <- ivreg(sbp ~ Edu+BMI|PGRS1+PGRS2)$coef[2]
mvmr_s_eb_b[i] <- ivreg(sbp ~ Edu+BMI|PGRS1+PGRS2)$coef[3]


}

resultsa <- cbind(ols_b_e, mr_b_e, ols_s_e, ols_s_b, ols_s_eb_e, ols_s_eb_b, mr_s_e, mr_s_b, mvmr_s_eb_e, mvmr_s_eb_b)


resultsb <- cbind(ols_b_e*ols_s_b, ols_s_e - ols_s_eb_e, mr_b_e*mr_s_b, mr_s_e - mvmr_s_eb_e)

cont_results <- rbind(apply(resultsa,2,mean))
sd_cont_results <- rbind(apply(resultsa,2,sd))

cont_results_ind <- rbind(apply(resultsb,2,mean))
sd_cont_results_ind <- rbind(apply(resultsb,2,sd))

results <- data.frame(beta1, gamma, cont_results, cont_results_ind, sd_cont_results, sd_cont_results_ind)


return(results)

}