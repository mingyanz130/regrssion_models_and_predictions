pic = read.csv(file = "http://grimshawville.byu.edu/bestpicture.txt", header=FALSE,sep=",")
colnames(pic) <- c("Name","Win","Review","Rating") 

pic$Rating = as.factor(pic$Rating)
pic$Rating = relevel(pic$Rating, " PG-13")



#model:
#log(P(win)/P(loss)) = beta0 + beta1*Review + beta2*PG-13 +beta3*R

pic.out = glm(Win ~ Review+Rating,
              data = pic,
              family = "binomial")
#table of coeffcients and std errors
summary(pic.out)

# a more interpretable version
exp( coef(pic.out)[-1])


# 95% CI on exp(beta_j)
exp( confint(pic.out)[-1,])


#is there a difference in PG-13 and R
pic.red = glm(Win ~ Review,
              data = pic,
              family = "binomial")
summary(pic.red)

anova(pic.red,pic.out,test="Chisq")
#there is no statistical significant difference between women and men
#p-value = 0.3913
#in log-odds: men are more likely to DNF than women
#odds-ratio:
# holding all else constant mean have a 19% (95%CI: -20,78%)
#increase in the odds of DNF compared to women




#prediction
predict(pic.out, newdata = data.frame(Rating = " PG-13", Review=97), 
        type = "response")

logit.hat = predict(pic.out, newdata = data.frame(Rating = " PG-13", Review=97), 
                    type = "link", se=TRUE)
logit.L = logit.hat$fit - 1.96*logit.hat$se
logit.U = logit.hat$fit + 1.96*logit.hat$se

exp(logit.hat$fit)
exp(logit.L)/(1+exp(logit.L))
exp(logit.U)/(1+exp(logit.U))


#ROC curve
library(ROCR)
#train
pic.pred = prediction(predict(pic.out, type="response"), pic$Win)
pic.perf = performance(pic.pred,measure= "tpr", x.measure= "fpr")
#test
pic.test.pred = prediction(predict(pic.out, newdata= pic, type = "response"), pic$Win)
pic.test.perf = performance(pic.test.pred,measure= "tpr", x.measure= "fpr")

plot(pic.perf, xlab = '1 - specify', ylab = 'sensitivity', main = "Roc curve")
abline(0,1,col="gray")
plot(pic.test.perf, add=TRUE, col = "red")

performance(pic.pred,measure= "auc")
performance(pic.test.pred,measure= "auc")
