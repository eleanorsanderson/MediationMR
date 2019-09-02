

mediationresultsci <- function(n, reps, Tot, prop_med, beta2, M_error)
{

  if (M_error == "exposure"){
    me_var_exp = 2
    me_var_med = 0
  } else if (M_error == "mediator"){
    me_var_exp = 0
    me_var_med = 2
  }  else {
    me_var_exp = 0
    me_var_med = 0
  }
  
  
    pi1 = 0.7
    pi1b = 0.3
    pi2 = 1.0
  
  
  gamma = (Tot*prop_med)/beta2
  beta1 = Tot - gamma*beta2
  

##Data set up
##variances 

  #wider confidence interval
  
s2_bp <- 10
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



mu = c(0,0,0)
s = matrix(c(s2_bp, s_bp_e, s_bp_b, s_bp_e, s2_e, s_e_b, s_bp_b, s_e_b, s2_bmi), ncol = 3)
v = mvrnorm(n, mu, s)


Edu_true <-  pi1*PGRS1 + pi1b*PGRS1b + v[,2]
Edu <- Edu_true + rnorm(n, 0, me_var_exp)
BMI_true <-  gamma*Edu_true + pi2*PGRS2 + v[,3]
BMI <- BMI_true + rnorm(n, 0, me_var_med)

sbp <- (beta1)*Edu_true + beta2*BMI_true + v[,1]
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

ols_h_e[i] <- lm(hyp ~ Edu)$coef[2]
ols_h_b[i] <- lm(hyp ~ BMI)$coef[2]
ols_h_eb_e[i] <- lm(hyp ~ Edu + BMI)$coef[2]
ols_h_eb_b[i] <- lm(hyp ~ Edu + BMI)$coef[3]
mr_h_e[i] <- ivreg(hyp ~ Edu|PGRS1)$coef[2]
mr_h_b[i] <- ivreg(hyp ~ BMI|PGRS2)$coef[2]
mvmr_h_eb_e[i] <- ivreg(hyp ~ Edu+BMI|PGRS1+PGRS2)$coef[2]
mvmr_h_eb_b[i] <- ivreg(hyp ~ Edu+BMI|PGRS1+PGRS2)$coef[3]



##Estimating the odds ratios
#predict the exposures first

prd_e <- lm(Edu ~ PGRS1)$fitted
prd_b <- lm(BMI ~ PGRS2)$fitted
prd_e_b <- lm(Edu ~ PGRS1 + PGRS2)$fitted
prd_b_e <- lm(BMI ~ PGRS1 + PGRS2)$fitted

mr_h_e_lor[i] <- (glm(hyp~prd_e,family=binomial())$coef[2])
mr_h_b_lor[i] <- (glm(hyp~prd_b,family=binomial())$coef[2])
mr_h_eb_e_lor[i] <- (glm(hyp~prd_e_b + prd_b_e,family=binomial())$coef[2])
mr_h_eb_b_lor[i] <- (glm(hyp~prd_e_b + prd_b_e,family=binomial())$coef[3])

mr_h_e_or[i] <- exp(mr_h_e_lor[i])
mr_h_b_or[i] <- exp(mr_h_b_lor[i])
mr_h_eb_e_or[i] <- exp(mr_h_eb_e_lor[i])
mr_h_eb_b_or[i] <- exp(mr_h_eb_b_lor[i])


ols_c_e[i] <- lm(low ~ Edu)$coef[2]
ols_c_b[i] <- lm(low ~ BMI)$coef[2]
ols_c_eb_e[i] <- lm(low ~ Edu + BMI)$coef[2]
ols_c_eb_b[i] <- lm(low ~ Edu + BMI)$coef[3]
mr_c_e[i] <- ivreg(low ~ Edu|PGRS1)$coef[2]
mr_c_b[i] <- ivreg(low ~ BMI|PGRS2)$coef[2]
mvmr_c_eb_e[i] <- ivreg(low ~ Edu+BMI|PGRS1+PGRS2)$coef[2]
mvmr_c_eb_b[i] <- ivreg(low ~ Edu+BMI|PGRS1+PGRS2)$coef[3]


mr_c_e_lor[i] <- (glm(low~prd_e,family=binomial())$coef[2])
mr_c_b_lor[i] <- (glm(low~prd_b,family=binomial())$coef[2])
mr_c_eb_e_lor[i] <- (glm(low~prd_e_b + prd_b_e,family=binomial())$coef[2])
mr_c_eb_b_lor[i] <- (glm(low~prd_e_b + prd_b_e,family=binomial())$coef[3])


mr_c_e_or[i] <- exp(mr_c_e_lor[i])
mr_c_b_or[i] <- exp(mr_c_b_lor[i])
mr_c_eb_e_or[i] <- exp(mr_c_eb_e_lor[i])
mr_c_eb_b_or[i] <- exp(mr_c_eb_b_lor[i])



}

resultsa <- cbind(ols_b_e, mr_b_e)
resultsb <- cbind(ols_s_e, ols_s_b, ols_s_eb_e, ols_s_eb_b, mr_s_e, mr_s_b, mvmr_s_eb_e, mvmr_s_eb_b)
resultsc <- cbind(ols_h_e, ols_h_b, ols_h_eb_e, ols_h_eb_b, mr_h_e, mr_h_b, mvmr_h_eb_e, mvmr_h_eb_b)
resultsd <- cbind(mr_h_e_lor, mr_h_b_lor, mr_h_eb_e_lor, mr_h_eb_b_lor)
resultsd2 <- cbind(mr_h_e_or, mr_h_b_or, mr_h_eb_e_or, mr_h_eb_b_or)
resultse <- cbind(ols_c_e, ols_c_b, ols_c_eb_e, ols_c_eb_b, mr_c_e, mr_c_b, mvmr_c_eb_e, mvmr_c_eb_b)
resultsf <- cbind(mr_c_e_lor, mr_c_b_lor, mr_c_eb_e_lor, mr_c_eb_b_lor)
resultsf2 <- cbind(mr_c_e_or, mr_c_b_or, mr_c_eb_e_or, mr_c_eb_b_or)


results <- data.frame(beta1, gamma, resultsa, resultsb, resultsc, resultsd, resultsd2, resultse, resultsf, resultsf2)

return(results)

}