examp = read.csv("http://grimshawville.byu.edu/ExamP.csv", header = TRUE, sep = ",")



#EDA
table(examp$GPA>=3.5, examp$ExamP)
#gives joint proportions:
prop.table(table(examp$GPA>=3.5, examp$ExamP))
#gives conditional on GPA row proportions:
prop.table(table(examp$GPA>=3.5, examp$ExamP), margin=1)

boxplot(GPA~ExamP, data = examp)

#Analysis:

#Response Variable:
# Pass, No pass
# Y=1, if Pass
#  = 0, if No Pass

examp$Passed = ifelse(examp$ExamP=="Passed", 1, 0)

#Explanatory Variable:
# GPA(quantitative)

#Model:
# log(P(pass)/ P(No Pass)) = beta0 + beta1*GPA

examp.out = glm(Passed ~ GPA, data = examp, family= "binomial")
summary(examp.out)

#intercept GPA effects
exp(2.256)

#estimated increase in odds of passing Examp by 9.54
#(almost 10 times) for increasing GPA by 1 
#(student increasing from B to A)


#is GPA statistically significant
#z-test
summary(examp.out)
# LRT X^2 test
examp.reduced = glm(Passed ~ +1, data = examp, family= "binomial")
anova(examp.reduced, examp.out, test="Chisq")

# 95% CI on beta1
confint(examp.out)

#95% CI on exp(beta1)
exp(confint(examp.out)[-1,])

#predict prob of passing for students with 3.25 and 3.85
predict(examp.out, newdata= data.frame(GPA=c(3.25,3.85)), type = "response")


#graphic showing the effect of GPA on Passing Exam P
#data
plot(Passed~GPA, data = examp, pch=19, xlab="GPA", ylab="P(Pass Exam P | GPA)")
#curve
xstar = seq(2,4,length=100)
phat= predict(examp.out, newdata = data.frame(GPA=xstar),type="response")
lines(xstar,phat,col="gray",lwd=2)

#Ci on curve
logit.hat = predict(examp.out, newdata = data.frame(GPA=xstar), type="link", se=TRUE)
logit.L = logit.hat$fit -1.96*logit.hat$se
logit.U = logit.hat$fit +1.96*logit.hat$se
phat.L = exp(logit.L)/(1+exp(logit.L))
phat.U = exp(logit.U)/(1+exp(logit.U))

lines(xstar,phat.L,col="gray",lty=3)
lines(xstar,phat.U,col="gray",lty=3)

#log odd ratio
plot(xstar, logit.hat$fit,type="l",col="gray",lwd=2)
lines(xstar,logit.L, col="gray", lty=3)
lines(xstar,logit.U, col="gray", lty=3)






x1 = seq(0,3,length=100)
x2 = seq(-1,1,length=100)

lmfig1fun = function(x1,x2){2+3*x1-4*x2}

persp(x1,x2, outer(x1,x2, FUN = lmfig1fun),theta=330,phi=45,r=20,xlab="X1",ylab="X2", zlab="E(Y|X1,X2)",ticktype = "detailed")

x1 = seq(0,1,length=100)
lmfig2fun = function(x1,x2){14-8*x1+2*x2}

plot(x1,lmfig2fun(x1,0),type="l",ylim=c(6,16),xlab="X",ylab="Y")
lines(x1, lmfig2fun(x1,0),type="l",xlab="X",ylab="Y",ylim=c(6,16))

text(0.4,10,"Z=0")
lines(x1, lmfig2fun(x1,1),type="l",xlab="X",ylab="Y")
text(0.6,12,"Z=1")


X = rbind(c(1,3,1,8,1,1),
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
dim(X)
dim(Y)
beta_ = solve(t(X) %*% X, t(X) %*% Y)
beta_
