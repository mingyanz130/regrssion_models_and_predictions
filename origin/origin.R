
#Data
peas = read.csv(header=TRUE,text='
                parent, offspring,
                0.21, 0.1726
                0.20, 0.1707
                0.19, 0.1637
                0.18, 0.1640
                0.17, 0.1613
                0.16, 0.1617
                0.15, 0.1598')


#EDA
cor(peas$parent,peas$offspring)

library(ggplot2)
qplot(parent, offspring, data = peas,
      geom="point",
      xlab = "Doameter of Parent Pea",
      ylab = "Doameter of Offspring Pea")
#Analysis:

#Response Variavle; offspring pea diameter (in)
#Explanatory Variable: parent pea diameter (in)

#Model offspring = beta0 + beta1 parent + epsilon, epsilon~N(0, sigma2)

out.peas = lm(offspring ~parent, data = peas)

#create a table of estimates and std errors
summary(out.peas)

#t-test
# Is there a statistically significant inheritance effect?
# t-test
# H0: beta1 = 0
# Ha: beta1 != 0
# t = 5.438
# pvalue = 0.00285

# we reject H0 in favor of Ha: beta1 != 0 since pvalue=0.0028 < alpha = 0.05
# There is a statically significant inheritance effect (pvalue = 0.0029)
#For a one inch increase in parent pea diameter we estimate a 0.21 increase 
#in offspring pea diameter on average.




#ANOVA F-test

#H0: no parent affect on offspring <==> H0:beta1 = 0
#Ha: is a parent affect on offspring <==> Ha:beta1 != 0

anova(out.peas)
#F = 29.577
#pvalue = 0.002852 
# we reject H0 in favor of Ha: beta1 != 0 since pvalue=0.0028 < alpha = 0.05
# There is a statically significant inheritance effect (pvalue = 0.0029)
#For a one inch increase in parent pea diameter we estimate a 0.21 increase 
#in offspring pea diameter on average.




#95% confidence Interval on beta1
confint(out.peas)
#(0.111 0.309)
#  we reject H0 in favor of Ha: beta1 != 0 since the 95% confidence interval on beta1 is (0.11, 0.309) which 
#dose not contain zero
# There is a statically significant inheritance effect (pvalue = 0.0029)
#For a one inch increase in parent pea diameter we estimate a 0.210 (95%CI: 0.111 0.309) increase 
#in offspring pea diameter on average.


#graphic showing the uncertainty in estimating the model
#graphic of uncertainty
library(ggplot2)

qplot(parent, offspring, data = peas,
      geom = "smooth", formula = y~x, 
      method = "lm", se = TRUE,
      xlab = "Doameter of Parent Pea",
      ylab = "Doameter of Offspring Pea" )

# 95% CI for E(Offspring | Parent=0.20)
predict(out.peas,newdata=data.frame(parent=0.20),interval="confidence")


# 95% PI for Parent=0.18
predict(out.peas,newdata=data.frame(parent=0.18),interval="prediction")


# graphic of uncertainty with predictions
plot.df<-cbind(peas,predict(out.peas,interval="prediction"))

ggplot(plot.df,aes(parent,offspring))+
  xlab("Diameter of Parent Pea") +
  ylab("Diameter of Offspring Pea") +
  geom_point() +
  geom_line(aes(y=fit),color="royalblue") +
  geom_line(aes(y=lwr),color="red",linetype="dashed") +
  geom_line(aes(y=upr),color="red",linetype="dashed")

# report R^2
summary(out.peas)
# % of variation in Y explained by X

