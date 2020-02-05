

mediationmultiple <- function(n, reps, mediators)
{

 
    pi1 = 1.0
    pi2 = 1.0
    pi3 = 1.0
    pi4 = 1.0
  
  
##Data set up


if (mediators == "independent"){
  betaE = 0.2
  gamma1 = 0.1
  beta1 = 0.5
  gamma2 = 0.2
  beta2 = 0.4
  gamma3 = 0.4
  beta3 = 0.3
  delta23 = 0
} else if (mediators == "related"){
  betaE = 0.2
  gamma1 = 0.1
  beta1 = 0.5
  gamma2 = 0.2
  beta2 = 0.25
  gamma3 = 0.4
  beta3 = 0.3
  delta23 = 0.5
}  

    
    ols_b_e = mr_b_e = ols_m1_e = mr_m1_e = ols_m2_e =  mr_m2_e = NULL
    ols_s_e = ols_s_b = ols_s_m1 = ols_s_m2 = ols_s_eb_e = ols_s_eb_b = ols_s_eb_m1 = ols_s_eb_m2 = NULL
    mr_s_e = mr_s_b = mr_s_m1 = mr_s_m2 = NULL
    mvmr_s_eb_e = mvmr_s_eb_b = mvmr_s_eb_m1 = mvmr_s_eb_m2 = NULL
    
    
for (i in 1:reps) {

PGRS1 <- rnorm(n,0,1)
PGRS2 <- rnorm(n,0,1)
PGRS3 <- rnorm(n,0,1)
PGRS4 <- rnorm(n,0,1)

mu = c(0,0,0,0,0)
s = matrix(c(rep(1,25)), ncol = 5)
v = mvrnorm(n, mu, s)


Edu <-  pi1*PGRS1 + v[,2]
BMI <-  gamma1*Edu + pi2*PGRS2 + v[,3]
Med1 <- gamma2*Edu + pi3*PGRS3 + v[,4]
Med2 <- gamma3*Edu + delta23*Med1 + pi4*PGRS4 + v[,5]

sbp <- betaE*Edu + beta1*BMI + beta2*Med1 + beta3*Med2 + v[,1]
cutoff <- quantile(sbp,0.75)
hyp <- as.numeric(sbp>cutoff)

cutoff_l <- quantile(sbp, 0.95)
low <- as.numeric(sbp>cutoff_l)

##Get the regression output for the continuous outcome

ols_b_e[i] <- lm(BMI~Edu)$coef[2]
mr_b_e[i] <- ivreg(BMI~Edu|PGRS1)$coef[2]

ols_m1_e[i] <- lm(Med1~Edu)$coef[2]
mr_m1_e[i] <- ivreg(Med1~Edu|PGRS1)$coef[2]

ols_m2_e[i] <- lm(Med2~Edu)$coef[2]
mr_m2_e[i] <- ivreg(Med2~Edu|PGRS1)$coef[2]

ols_s_e[i] <- lm(sbp ~ Edu)$coef[2]
ols_s_b[i] <- lm(sbp ~ BMI)$coef[2]
ols_s_m1[i] <- lm(sbp ~ Med1)$coef[2]
ols_s_m2[i] <- lm(sbp ~ Med2)$coef[2]
ols_s_eb_e[i] <- lm(sbp ~ Edu + BMI +Med1 + Med2)$coef[2]
ols_s_eb_b[i] <- lm(sbp ~ Edu + BMI +Med1 + Med2)$coef[3]
ols_s_eb_m1[i] <- lm(sbp ~ Edu + BMI +Med1 + Med2 )$coef[4]
ols_s_eb_m2[i] <- lm(sbp ~ Edu + BMI +Med1 + Med2)$coef[5]

mr_s_e[i] <- ivreg(sbp ~ Edu|PGRS1)$coef[2]
mr_s_b[i] <- ivreg(sbp ~ BMI|PGRS2)$coef[2]
mr_s_m1[i] <- ivreg(sbp ~ Med1|PGRS3)$coef[2]
mr_s_m2[i] <- ivreg(sbp ~ Med2|PGRS4)$coef[2]

mvmr_s_eb_e[i] <- ivreg(sbp ~ Edu+BMI+Med1+Med2|PGRS1+PGRS2+PGRS3+PGRS4)$coef[2]
mvmr_s_eb_b[i] <- ivreg(sbp ~ Edu+BMI+Med1+Med2|PGRS1+PGRS2+PGRS3+PGRS4)$coef[3]
mvmr_s_eb_m1[i] <- ivreg(sbp ~ Edu+BMI+Med1+Med2|PGRS1+PGRS2+PGRS3+PGRS4)$coef[4]
mvmr_s_eb_m2[i] <- ivreg(sbp ~ Edu+BMI+Med1+Med2|PGRS1+PGRS2+PGRS3+PGRS4)$coef[5]



}

resultsa <- cbind(ols_b_e, mr_b_e, ols_m1_e, mr_m1_e, ols_m2_e, mr_m2_e)
resultsb <- cbind(ols_s_e, ols_s_b, ols_s_m1, ols_s_m2,  mr_s_e, mr_s_b, mr_s_m1, mr_s_m2)
resultsc <- cbind(ols_s_eb_e, ols_s_eb_b, ols_s_eb_m1, ols_s_eb_m2, mvmr_s_eb_e, mvmr_s_eb_b, mvmr_s_eb_m1, mvmr_s_eb_m2)



results <- data.frame(n, reps, mediators, resultsa, resultsb, resultsc)

return(results)

}