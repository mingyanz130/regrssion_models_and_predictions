library(httr)
library(jsonlite)
get1<-GET("https://s3-us-west-1.amazonaws.com/riot-developer-portal/seed-data/matches10.json")
alljson1<-fromJSON(content(get1, "text", encoding = "UTF-8"))
# get the 12th chunk (out of 100)

LOL.big =NULL
for (i in c(1:100)){
this.row.i<-data.frame(Win=ifelse(alljson1$matches$participants[[i]]$stats$win==TRUE,1,0),
                       Kills=alljson1$matches$participants[[i]]$stats$kills,
                       Deaths=alljson1$matches$participants[[i]]$stats$deaths,
                       LargestMultiKill=alljson1$matches$participants[[i]]$stats$largestMultiKill,
                       Assists=alljson1$matches$participants[[i]]$stats$assists,
                       GoldEarned=alljson1$matches$participants[[i]]$stats$goldEarned,
                       LongestTimeSpentLiving=alljson1$matches$participants[[i]]$stats$longestTimeSpentLiving)
LOL.big = rbind(LOL.big,this.row.i)
}


#EDA:
#par(mfrow=c(3,2))
boxplot(Kills~Win,data = LOL.big,xlab="Wins",ylab="Kills")
boxplot(Deaths~Win,data = LOL.big,xlab="Wins",ylab="Deaths")
boxplot(LargestMultiKill~Win, data = LOL.big,xlab="Wins",ylab="largestMultiKill")
boxplot(Assists~Win, data = LOL.big,xlab="Wins",ylab="assists")
boxplot(GoldEarned~Win, data = LOL.big,xlab="Wins",ylab="goldEarned")
boxplot(LongestTimeSpentLiving~Win, data = LOL.big,xlab="Wins",ylab="longestTimeSpentLiving")
#par(mfrow=c(1,1))

#look for anything unusual
summary(LOL.big)

plot(~Kills+Deaths+Assists+LargestMultiKill+GoldEarned+LongestTimeSpentLiving,data=LOL.big)
#looking closer: Kills >= 20 & Death >= 15 are bad influential
plot(Kills~Deaths, data = LOL.big)
plot(jitter(Kills)~jitter(Deaths), data = LOL.big)
plot(jitter(Kills)~jitter(Assists), data = LOL.big)
plot(jitter(Deaths)~jitter(Assists), data = LOL.big)


#look at obs 175 374 322 526 (they lost!)
# obs 792 amazing won!


#removing obs 175, 322, 374, 526, 792 and any obs with kills >= 20 and deaths >=15
LOL.big = LOL.big[-c(175,322,374,526,792),]
LOL.big = subset(LOL.big, Kills < 20 & Deaths < 15)


dim(LOL.big)






#Response Variable: Win = 1 if win, =0 if lost
#explanatory varaiable:
#   Offense: Kills, GoldEarned
#   Errors: Death
#   Team Play: Assists
#   Risk/Reward: LongestTimeSpentLiving
#   Hot hand: LargestMultiKill


#create train and test
set.seed(58)

train.ind = sample(989,700)

LOL.train = LOL.big[train.ind,]
LOL.test = LOL.big[-train.ind,]

#model:
#log(P(win)/P(loss)) = beta0 + beta1*kills + beta2*GoldEarned +
#                      beta3*Death+beta4*Assists+beta5*LongestTimeSpentLiving+beta6*LargestMultiKill

LOL.out = glm(Win ~ Kills+GoldEarned +Deaths+Assists+
                LongestTimeSpentLiving+LargestMultiKill,
                data = LOL.train,
                family = "binomial")

#table of coeffcients and std errors
summary(LOL.out)

# a more interpretable version
exp( coef(LOL.out)[-1])


# 95% CI on exp(beta_j)
exp( confint(LOL.out)[-1,])


#graphic of effects

#partial logg_odds
x.star = seq(0,10,length=100)
plot(x.star, coef(LOL.out)[2]*x.star,type="l",
     xlab = "Kills", ylab= "Partial Log-Odds Winning")


#From a Probability Perspective (by setting explanatory variables to median)
x.star = data.frame(Kills = seq(0,10,length=100),GoldEarned = 11000, Deaths=5,Assists=7,
                      LongestTimeSpentLiving=600,LargestMultiKill=1)
plot(x.star$Kills, predict(LOL.out,newdata=x.star,type= "response"), type="l",
     xlab = "Kills", ylab= "P(Win)", ylim= c(0,1))


#For GoldEarned
x.star = data.frame(GoldEarned = seq(5000,17500,length=100),Kills=5, Deaths=5,Assists=7,
                    LongestTimeSpentLiving=600,LargestMultiKill=1)
plot(x.star$GoldEarned, predict(LOL.out,newdata=x.star,type= "response"), type="l",
     xlab = "Kills", ylab= "P(Win)", ylim= c(0,1))

#summarize effect on Winning

#Dose playing aggresively having a statiscally significant
# H0: Kills, Assists, LargestKill have no effect

LOL.reduced = glm(Win~GoldEarned+Deaths+LongestTimeSpentLiving, data = LOL.train, family = "binomial")
anova(LOL.reduced, LOL.out, test="Chisq")



#Prediction P(Win) for a player with Faker-like skills
predict(LOL.out, newdata= data.frame(Kills = 2,GoldEarned=15000, Deaths=3, Assists=8, LongestTimeSpentLiving=600, LargestMultiKill=2), type = "response")


#95% CI on P(Win)
#logit
Faker.logit = predict(LOL.out, newdata= data.frame(Kills = 2,GoldEarned=15000, Deaths=3, Assists=8, LongestTimeSpentLiving=600, LargestMultiKill=2),
                      type = "link", se.fit=TRUE)
#95% CI on logit
logit.L = Faker.logit$fit - 1.96* Faker.logit$se.fit
logit.U = Faker.logit$fit + 1.96* Faker.logit$se.fit

Faker.phat.L = exp(logit.L)/(1+exp(logit.L))
Faker.phat.U = exp(logit.U)/(1+exp(logit.U))



# ROC curve
library(ROCR)
train.pred<-prediction(predict(LOL.out, type='response'), LOL.train$Win)
train.perf<-performance(train.pred, measure='tpr', x.measure='fpr')
plot(train.perf, xlab='1-specificity', ylab='sensitivity', main='ROC curve')
abline(0, 1, col='gray')

# AUC
performance(train.pred, measure='auc')




        