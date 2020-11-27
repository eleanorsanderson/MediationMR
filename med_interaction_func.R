

interactionresults <- function(n, reps, int_eff, cont)
{


  
    pi1 = 1.0
    pi2 = 1.0
  
  ###need to work out this bit so that the total effect and proportion mediated make sense with the interaction
    
  beta1 = 0.4
  beta2 = 0.5
  gamma = 0.2
  beta3 = int_eff

##Data set up

s2_bp <- 1
s2_e <- 1
s2_bmi <- 1
s_bp_e <- 1
s_bp_b <- 1
s_e_b <- 1

ols_b_e = mr_b_e = NULL
ols_s_e = ols_s_b = ols_s_eb_e = ols_s_eb_b = mr_s_e = mr_s_b = mvmr_s_eb_e = mvmr_s_eb_b = NULL



for (i in 1:reps) {

PGRS1 <- rnorm(n,0,1)
PGRS1b <- rnorm(n,0,1)
PGRS2 <- rnorm(n,0,1)
PGRS2b <- rnorm(n,0,1)


mu = c(0,0,0)
s = matrix(c(s2_bp, s_bp_e, s_bp_b, s_bp_e, s2_e, s_e_b, s_bp_b, s_e_b, s2_bmi), ncol = 3)
v = mvrnorm(n, mu, s)


if(cont == TRUE){

Edu_true <- 2 + pi1*PGRS1 + v[,2]
Edu <- Edu_true 
BMI_true <- 5 + gamma*Edu_true + pi2*PGRS2 + v[,3]
BMI <- BMI_true 

sbp <- (beta1)*Edu_true + beta2*BMI_true + beta3*Edu_true*BMI_true + v[,1]

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


if(cont == FALSE){
  
  Edu_true <-  pi1*PGRS1 + v[,2]
  Edu <- Edu_true 
  BMI_true <-  gamma*Edu_true + pi2*PGRS2 + v[,3]
  BMI <- as.numeric(BMI_true > pctile(BMI_true,0.75))
  
  sbp <- (beta1)*Edu_true + beta2*BMI + beta3*Edu_true*BMI + v[,1]
  
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



}

resultsa <- cbind(ols_b_e, mr_b_e, ols_s_e, ols_s_b, ols_s_eb_e, ols_s_eb_b, mr_s_e, mr_s_b, mvmr_s_eb_e, mvmr_s_eb_b)

cont_results <- rbind(apply(resultsa,2,mean))
sd_cont_results <- rbind(apply(resultsa,2,sd))

results <- data.frame(beta1, beta3, cont_results, sd_cont_results)

return(results)

}