library(XML)
mlb.webpage = htmlParse("http://www.espn.com/mlb/standings/_/season/2018")
mlb= readHTMLTable(mlb.webpage,
                        which = 1,
                        skip=c(1:21,27,33),
                        header=FALSE,
                        colClasses = c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character","character"),
                        stringsAsFactors = FALSE
                        )

mlb = subset(mlb[,c(1,9)])
colnames(mlb) = c("wins", "run.diff")
mlb$division = c("east","east","east","east","east","central","central","central","central","central","west","west","west","west","west")
mlb$division = as.factor(mlb$division)
west = subset(mlb, division == "west")
central = subset(mlb, division == "central")
east = subset(mlb, division == "east")

summary(west$wins)
summary(central$wins)
summary(east$wins)

summary(west$run.diff)
summary(central$run.diff)
summary(east$run.diff)

#EDA
library(ggplot2)

ggplot() + 
  geom_point(data = west, aes(x=run.diff,y=wins,color ="west"))+ 
  geom_point(data = east, aes(x=run.diff,y=wins,color ="east"))+ 
  geom_point(data = central, aes(x=run.diff,y=wins,color ="central"))+
  labs(title = "run differential and wins", y = "wins",
     x = "run differential")


#Analysis:

#Model:
#if ingore division, SLR
#wins = beta0 + beta1* runn.diff + ε~N(0,σ^2) 

#if ingnore run.diff
#wins_i,j = μ + α_i + ε_i,j
#i = 1, 2, 3


#division is a factor
mlb$division = factor(mlb$division)
mlb$division = relevel(mlb$division, "east")

mlb.out = lm(wins~division+run.diff, data = mlb, y = TRUE, x = TRUE)
summary(mlb.out)

mlb.out1 = lm(wins~division, data = mlb, y = TRUE, x = TRUE)
summary(mlb.out1)

#interpret regression coefficient for run.diff:
#for each run scored or prevented we estimated a 0.10
#increase in the number of wins keeping
# teams in the same division

# what makes the division explanatory variable hard to interprete
# 3 levels (East, Central, West)
# "division main effect"

mlb.out$x

#graphic of the different models (showing the effect of divisions)

plot(wins~run.diff, data = mlb, type="n")
points(wins~run.diff, data = subset(mlb, division == "east"), pch = 19, col="orange")
points(wins~run.diff, data = subset(mlb, division == "central"), pch = 19, col="black")
points(wins~run.diff, data = subset(mlb, division == "west"), pch = 19, col="red")
#overlay estimated line for each division
abline(81.840678, 0.099961, col = "orange")
abline(81.840678 -2.843916, 0.099961, col = "black")
abline(81.840678 + 0.241295, 0.099961, col = "red")

#it appears from the graph that there is no statiscally significant
# difference between divison after adjusting for run differential

# Test ho : no difference between division (no division effect)
#     beta1 = 0 and beta2 = 0


#assume Ho is true
mlb.reduced = lm(wins~run.diff, data=mlb)

anova(mlb.reduced, mlb.out)

# there is no statistically significant different in divisions (pvalue = 0.6172)
# after adjusting for run differential









X = rbind(
  c(1,3,1,8,1,1),
  c(1,6,3,3,1,2),
  c(1,4,1,4,1,5),
  c(1,5,1,3,1,1),
  c(1,7,1,3,3,1),
  c(1,6,1,3,5,2),
  c(1,6,1,8,5,3),
  c(1,5,1,8,3,2),
  c(1,1,3,4,2,1),
  c(1,3,5,2,2,4),
  c(1,2,3,3,1,3),
  c(1,1,4,3,2,3),
  c(1,6,5,3,1,5),
  c(1,2,1,5,2,2),
  c(1,5,1,4,2,1))
Y = cbind(c(303.3,467.1,422.8,391.6,
            403.8,373.8,263.5,226.8,
            183.9,208.4,208.2,168.8,
            245.4,160.0,173.2))

l.out = lm(Y~X[,2]+X[,3]+X[,4]+X[,5]+X[,6], y = TRUE, x = TRUE)
l.out1 = lm(Y~1, y = TRUE, x = TRUE)
l.out2 = lm(Y~X[,3]+X[,4]+X[,5]+X[,6], y = TRUE, x = TRUE)
l.out3 = lm(Y~X[,2], y = TRUE, x = TRUE)
l.out4 = lm(Y~X[,2]+X[,2]+X[,2]+X[,2]+X[,2], y = TRUE, x = TRUE)

summary(l.out)
summary(l.out1)

anova(l.out, l.out1)
anova(l.out, l.out2)
anova(l.out,l.out3)
anova(l.out,l.out4)

X_R = X[,1] 
beta_ = solve(t(X) %*% X, t(X) %*% Y)
beta_R = solve(t(X_R) %*% X_R, t(X_R) %*% Y)

t(Y - X %*% beta_) %*% (Y- X%*%beta_ )
t(Y - X_R %*% beta_R) %*% (Y- X_R%*%beta_R )


X = rbind(
  c(0,0,0,1),
  c(0,1,0,0),
  c(0,0,1,0),
  c(0,0,0,1),
  c(0,1,0,0),
  c(1,0,0,0),
  c(0,0,1,0),
  c(1,0,0,0))
Y = cbind(c(10,8,9,10,6,5,7,7))
beta_  = solve(t(X) %*% X, t(X) %*% Y)
t(Y - X %*% beta_) %*% (Y- X%*%beta_ )






