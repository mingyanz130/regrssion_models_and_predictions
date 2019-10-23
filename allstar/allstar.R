allstar = read.csv(file = "http://grimshawville.byu.edu/MLBbostonASG.txt", header=TRUE,sep="")

cor(allstar$aud,allstar$BOS.total)
plot(allstar$BOS.total,allstar$aud)
cor(allstar)


#is it feasible to thoughtfully examine all 546 correlation / plots?
#general rule is that we need tall data ... not short wide

#what variables would you include
#BOS.total, BOS.atBreak, NYY.total,

#stepwise selection

#backward elimination isn't possible because we have wide 

#forward selection(best possible two variable model)
min.model = lm(aud/10^3 ~ +1, data = allstar)
biggest.model = formula(lm(aud/10^3 ~ ., data=allstar))
alg.out2 = step(min.model, scope= biggest.model, direction="forward", steps=2)
summary(alg.out2)

library(car)
crPlots(alg.out2)

#forward selection(best possible five variable model)
min.model = lm(aud/10^3 ~ +1, data = allstar)
biggest.model = formula(lm(aud/10^3 ~ ., data=allstar))
alg.out5 = step(min.model, scope= biggest.model, direction="forward", steps=5)
summary(alg.out5)
# R square is 0.988
crPlots(alg.out5)


#signs of over fit the model: 
#really good r square, variables does not make sense


