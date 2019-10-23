
summary(speed.last$speed)
hist(speed.last$speed)

#Analysis 

#Create train and test
set.seed(12)
n.speed.last=dim(speed.last)[1]
train.rows = sample(n.speed.last, 8000)
speed.train = speed.last[train.rows,]
speed.test = speed.last[-train.rows,]

#Model: Random Forest

library(randomForest)

out.speed = randomForest(x = speed.train[,-18], y = speed.train$speed,
                         xtest = speed.test[,-18], ytest = speed.test$speed,
                         replace = TRUE, #use bootstrap
                         keep.forest= TRUE, # store trees for future prediction
                         ntree = 50, # number of trees
                         mtry= 5, # rule of thumb is p/3
                         nodesize= 25)

#Model Summary
out.speed


#notice the results give MSE and we want RMSE
sqrt(48.71651)
sqrt(61.93)

new.obs

#predict for a new observation
predict(out.speed, newdata = new.obs)

# to understand a little about the model
round(importance(out.speed), 0)
varImpPlot(out.speed)


