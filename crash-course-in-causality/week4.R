#install packages (if needed)
install.packages("tableone")
install.packages("ipw")
install.packages("sandwich")
install.packages("survey")

library(tableone)
library(ipw)
library(sandwich) #for robust variance estimation
library(survey)
library(MatchIt)
library(summarytools)

data(lalonde)
summary(lalonde)
str(lalonde)
dfSummary(lalonde)
descr(lalonde)

#create a data set with just these variables, for simplicity
age <- as.numeric(lalonde$age)
black <- as.numeric(lalonde$black)
educ <- as.numeric(lalonde$educ)
hispan <- as.numeric(lalonde$hispan)
married <- as.numeric(lalonde$married)   
nodegree <- as.numeric(lalonde$nodegree)
re74 <- as.numeric(lalonde$re74)
re75 <- as.numeric(lalonde$re75)
re78 <- as.numeric(lalonde$re78)
treat <- as.numeric(lalonde$treat)

#new dataset
mydata <- cbind(age,black,educ,hispan,married, nodegree, 
              re74, re75, treat, re78)
mydata <- data.frame(mydata)

#covariates we will use (shorter list than you would use in practice)
xvars <- c("age","black","educ","hispan","married","nodegree","re74","re75")

##########################
#propensity score matching
#########################

#fit a propensity score model. logistic regression

psmodel<-glm(treat~age+black+educ+hispan+married+
               nodegree+re74+re75,
             family=binomial(link = "logit"),
             data=mydata)

#show coefficients etc
summary(psmodel)

#create propensity score
ps <-predict(psmodel, type = "response")

#create weights
weight<-ifelse(treat==1,1/(ps),1/(1-ps))

#apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =mydata, weights = ~ weight)

#weighted table 1
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat", 
                                  data = weighteddata, test = FALSE)
## Show table with SMD
print(weightedtable, smd = TRUE)

## Min e Max weight
min(weight); max(weight)

## Estimate and 95% confidence interval for the average causal effect
ate_fit <- svyglm(re78~treat, design=weighteddata)
summary(ate_fit)
confint(ate_fit, level = 0.95)

# fit propensity score model to get weights, but truncated
weightmodeltrunc <- ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~ age+black+educ+hispan+married+
                        nodegree+re74+re75, data=mydata,trunc=0.01)

#numeric summary of weights
summary(weightmodeltrunc$weights.trun)

mydata$wt<-weightmodeltrunc$weights.trun

#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt,
                                                    data =mydata)))
summary(msm)
confint(msm, level = 0.95)
