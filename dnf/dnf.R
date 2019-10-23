source('http://grimshawville.byu.edu/Tennis2018.R')


tennis$Gender = factor(tennis$Gender)
tennis$Gender = relevel(tennis$Gender, "M")
tennis$Tournament = factor(tennis$Tournament)
tennis$Tournament = relevel(tennis$Tournament, "Masters")
tennis$Surface = factor(tennis$Surface)
tennis$Surface = relevel(tennis$Surface, "Hard")
tennis$Round = factor(tennis$Round)
tennis$Round = relevel(tennis$Round, "1st Round")
tennis$Best.of = factor(tennis$Best.of)
tennis$Best.of = relevel(tennis$Best.of, "3")




#quantitative explanatory varaiables
par(mfrow=c(1,2))
par(mar=c(2,2,2,2))
boxplot(WRank~DNF,data = tennis,xlab="DNF",ylab="WRank",main= "WRank")
boxplot(LRank~DNF,data = tennis,xlab="DNF",ylab="LRank",main= "LRank")


#categorical explanatory varaiables
prop.table(table(tennis$Gender,tennis$DNF), margin =1)
prop.table(table(tennis$Surface,tennis$DNF), margin =1)
prop.table(table(tennis$Round,tennis$DNF), margin =1)
prop.table(table(tennis$Best.of,tennis$DNF), margin =1)


set.seed(27)
dim(tennis)
train.ind = sample(12914,10000)
tennis.train = tennis[train.ind,]
tennis.test = tennis[-train.ind,]


#Analysis
# response variable: DNF = 1 if match ended with loser retiring or walkover
# explanatory varaiable:
#     Gender, Tournament, Surface, Round, Best.of
#     WRank, LRank

#Model:
# log( P(DNP) / P(not DNP))
# = beta0 + beta1 * Men + beta2 * GrandSlam + beta3 Clay + beta4 Grass + beta5
#   2nd Round + beta6 3rd Round + beta7 4th Round + beta8 Quarterfinals + beta9 
#   Semifinals + beta10 The Final + beta11 Best of 3 + beta12 WRank +beta13 LRank




#is there a difference in Gender
tennis.out = glm(DNF~ Gender+Tournament+Surface+Round+Best.of+WRank+LRank,data=tennis.train, family=binomial)
summary(tennis.out)

coef(tennis.out)
exp(coef(tennis.out)[-1])

confint(tennis.out)
exp(confint(tennis.out)[-1,])


#there is no statistical significant difference between women and men
#p-value = 0.3913
#in log-odds: men are more likely to DNF than women
#odds-ratio:
# holding all else constant mean have a 19% (95%CI: -20,78%)
#increase in the odds of DNF compared to women

summary(tennis.out)
#we willl only report the p-value from the summary

tennis.red = glm(DNF~Tournament+Surface+Round+Best.of+WRank+LRank,data=tennis.train, family=binomial)
anova(tennis.red,tennis.out, test="Chisq")




#there is a statistical significant difference in Tournament
# log-odds: Grand Slam have a smaller DNF than Masters
# holding all else constant mean have a 43% (95%CI: 15%,63%)
#decrease in the odds of DNF compared to Masters



#graphic of effect
plot(c(0,1),c(0,-0.57676),
     pch=19, cex=3,
     xlim= c(-0.5,1.5),
     ylim=c(-0.7,0.1),
     ylab = "Partial log(DNF)",
     axis=FALSE)
axis(2)
axis(1,c(0,1),c("Masters","Grand Slam"))




#is there a difference in Gender
tennis.out = glm(DNF~ Gender+Tournament+Surface+Round+Best.of+WRank+LRank,data=tennis.train, family=binomial)
summary(tennis.out)

coef(tennis.out)
exp(coef(tennis.out)[-1])

confint(tennis.out)
exp(confint(tennis.out)[-1,])


#there is no statistical significant difference between best.of
#p-value < 0.001
#in log-odds: best of 5 are more likely to DNF than best of 3
#odds-ratio:
# holding all else constant mean have a 371% (95%CI: 217,641%)
#increase in the odds of DNF compared to best of 3


#test round has no effect on DNF
summary(tennis.out)
#we willl only report the p-value from the summary
tennis.red1 = glm(DNF~Gender+Tournament+Surface+Best.of+WRank+LRank,data=tennis.train, family=binomial)
anova(tennis.red1,tennis.out, test="Chisq")


#test surface has no effect on DNF
summary(tennis.out)
#we willl only report the p-value from the summary
tennis.red2 = glm(DNF~Gender+Tournament+Round+Best.of+WRank+LRank,data=tennis.train, family=binomial)
anova(tennis.red2,tennis.out, test="Chisq")



#Wrank,
#for one unit increase in WRank holding all other constant, we estimate 0.19% increase in DNF




#prediction
predict(tennis.out, newdata = data.frame(Gender = "M", Tournament="GrandSlam",
                                         Round= "1st Round", Surface = "Hard",
                                         Best.of = "5",WRank=50,
                                         LRank = 500), 
        type = "response")

logit.hat = predict(tennis.out, newdata = data.frame(Gender = "M", Tournament="GrandSlam",
                                         Round= "1st Round", Surface = "Hard",
                                         Best.of = "5",WRank=50,
                                         LRank = 500), 
        type = "link", se=TRUE)
logit.L = logit.hat$fit - 1.96*logit.hat$se
logit.U = logit.hat$fit + 1.96*logit.hat$se
phat.L = exp(logit.L)/(1+exp(logit.L))
phat.U = exp(logit.U)/(1+exp(logit.U))

#ROC curve
library(ROCR)
#train
tennis.pred = prediction(predict(tennis.out, type="response"), tennis.train$DNF)
tennis.perf = performance(tennis.pred,measure= "tpr", x.measure= "fpr")
#test
tennis.test.pred = prediction(predict(tennis.out, newdata= tennis.test, type = "response"), tennis.test$DNF)
tennis.test.perf = performance(tennis.test.pred,measure= "tpr", x.measure= "fpr")

plot(tennis.perf, xlab = '1 - specify', ylab = 'sensitivity', main = "Roc curve")
abline(0,1,col="gray")
plot(tennis.test.perf, add=TRUE, col = "red")

performance(tennis.pred,measure= "auc")
performance(tennis.test.pred,measure= "auc")

