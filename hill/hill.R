
library(ggplot2)

#Scottish Hill Races(1984 Records)


#Data:
hills = read.csv(header=TRUE,sep="","http://www.statsci.org/data/general/hills.txt")

#EDA
plot(Climb~Time, data=hills)
identify(x=hills$Time, y = hills$Climb,cex = 0.5, label = hills$Race)
#which ones are outliers? (vertical distance): Knock Hill,Bens of Jura
#which races are influencial obs? (horizontal distance) Two Breweries, Moffat Chase, Lairing Ghru



plot(Distance~Time, data=hills)

#identify(x=hills$Time, y = hills$Distance,cex = 0.5, label = hills$Race)
#which ones are outliers? (vertical distance):  Knock Hill, Seven Hills, Lairing Ghru
#which races are influencial obs? (horizontal distance) Bens of Jura,Two Breweries, Moffat Chase, Ben Nevis 


#Analysis

# Model:
#   Time = beta0 +beta1*Distance+beta2*Climb+ε ε is in (0,δ^2)  

hills.out= lm(Time~Distance + Climb, data = hills)

#before doing any inference, look at the data diagnosis

#Residual = Actual - Prediction = y_i - y_i_

#first, look at the normality assumption
#(shape, outliers)

#compute R-studentized residuals
hills.R = rstudent(hills.out)
hills.R

#what is the R-studentized residuals for Cairn Hill Race
subset(hills.R, hills$Race=="CairnTable")

hist(hills.R)
# prettier plot
hist(hills.R, freq=FALSE)
my.z = seq(-3,3,length=50)
lines(my.z, dnorm(my.z,0,1), col = "royalblue")


plot(density(hills.R))
my.z = seq(-3,3,length=50)
lines(my.z, dnorm(my.z,0,1), col = "royalblue")

#Test for normality
shapiro.test(hills.R)

"Normality Test"
# H0: normally distributed
# Ha: not normally distributed


#see two outliers, what races are they

#degree of freedom is (35-1-3) = 31
#is it ...Kildcon Hill?
subset(hills.R, hills$Race=="KildconHill")

#what if KildconHill is an outlier
#Ho: KildconHill is not an outlier
#Ha: KildconHill is an outlier
#p-value
2*(1 - pt(0.2054782, 31))
# KildconHill is not an outlier

#is it ...KnockHill?
subset(hills.R, hills$Race=="KnockHill")

#what if KnockHill is an outlier
#Ho: KnockHill is not an outlier
#Ha: KnockHill is an outlier
#p-value
2*(1 - pt(7.610845, 31))
# KnockHill is an outlier




#use a rule of thumb to find outliers
subset(hills, abs(hills.R)> 2)




#Data Diagonosis
#outliers/ Normality assumptions
#R- standardized residuals

#Influential obs

#leverage
#weight on obs has in predicting itself
hills.leverage <- lm.influence(hills.out)$hat

subset(hills.leverage, hills$Race == "BensofJura")

#Rule of Thumb: 2*(p+1)/n (n is total number of rows)
subset(hills, hills.leverage >2*3/35)


#Cook's Distance
#change in parameter estimates with and without obs

hills.cd = cooks.distance(hills.out)
subset(hills.cd, hills$Race == "MoffatChase")

#Rule of Thumb: 4/(N-(P+1))
subset(hills, hills.cd >4/(35-3))


#Look at Moffat Chase
par(mfrow=c(1,2))
plot(hills$Distance, hills$Time)
points(hills$Distance[35], hills$Time[35], col="Red", pch =19)
plot(hills$Climb, hills$Time)
points(hills$Climb[35], hills$Time[35], col="Red", pch =19)
par(mfrow=c(1,1))


#Look at Lairig Ghru(row11)
par(mfrow=c(1,2))
plot(hills$Distance, hills$Time)
points(hills$Distance[11], hills$Time[11], col="Red", pch =19)
plot(hills$Climb, hills$Time)
points(hills$Climb[11], hills$Time[11], col="Red", pch =19)
par(mfrow=c(1,1))



#Analysis

#from diagnostics, we remove the following races
#Knock Hill (we believe it is recorded incorrectly)
#Lairig Ghru(long flat race and most races are shorter with correlation between distance and climb)
#Bens of Jura (this is hard due to terrain.. terrain is not in the model)

#model: Time = beta0 +beta1*Distance + Climb + epsilon~N(0, sigma2)
hills1 = hills[-c(18,11,7)]
hills1.out = lm(Time~Distance + Climb, data = hills1)

summary(hills.out)


#impact of filtering obs on the analysis:
#only use the model for short small climb races (distance < 20, climb<6000)
#prediction follows where we belived from plos
#p-value a bi biased toward significance
#95% CI will be biased smaller
