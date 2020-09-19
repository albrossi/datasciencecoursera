install.packages("tableone")
install.packages("Matching")
install.packages("MatchIt")

library(tableone)
library(Matching)
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
xvars <- c("age","black","educ","hispan","married","nodegree","re74","re75", "re78")

#look at a table 1
table1 <- CreateTableOne(vars=xvars,strata="treat", data=mydata, test=FALSE)
## include standardized mean difference (SMD)
print(table1, smd=TRUE)
6349.14 - 6984.17

##########################
#propensity score matching
#########################

#fit a propensity score model. logistic regression

psmodel<-glm(treat~age+black+educ+hispan+married+
               nodegree+re74+re75,
             family=binomial(link = "logit"),data=mydata)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values

pscore <- predict(psmodel, newdata = mydata, type="response")

min(pscore); max(pscore)

descr(pscore, round.digits = 3)
plot(pscore)
hist(pscore)

############################################
#do greedy matching on Mahalanobis distance
############################################

set.seed(931139)

xvars <- c("age","black","educ","hispan","married","nodegree","re74","re75")

greedymatch<-Match(Tr=treat,M=1,X=pscore,replace=FALSE,caliper = 0.1)
matched<-mydata[unlist(greedymatch[c("index.treated","index.control")]), ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)


#outcome analysis
y_trt<-matched$re78[matched$treat==1]
y_con<-matched$re78[matched$treat==0]

#pairwise difference
diffy<-mean(y_trt)-mean(y_con)
diffy

#paired t-test
diffy<-y_trt-y_con
t.test(diffy)




