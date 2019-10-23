#Ticket or warning
ticket.last
summary(ticket.last@Ticket)
plot(ticket.last@Ticket)
#Data
#mONTGOMERY cOUNTY TRAFFIC sTOPS dATA
source('http://grimshawville.byu.edu/TrafficStops2018.R')

#Response Variables: Citation or Warning (binary)
#Collection of quantitative and categorical explanatory variable

#EDA:
#TRUE = Citation, False = Warning
table(ticket.last$Ticket)
prop.table(table(ticket.last$Ticket))

#Analysis

#Create a dataset with half goods(warning, FALSE)
#   and half bads (ticket, TRUE)
# All bads, SRS #/O replacement of goods

#Bads
all.bad = subset(ticket.last, Ticket == "TRUE")
n.bads = dim(all.bad)[1]
#Goods
all.good = subset(ticket.last, Ticket == "FALSE")
n.good = dim(all.good)[1]
set.seed(42)
rows.good = sample(n.good, n.bads)
sample.good = all.good[rows.good,]
ticket.model = rbind(all.bad, sample.good)

#verify correct
dim(ticket.model)
table(ticket.model$Ticket)

#create train and test
n.ticket.model = dim(ticket.model)[1]
train.rows = sample(n.ticket.model, 150000)
ticket.train = ticket.model[train.rows,]
ticket.test = ticket.model[-train.rows,]


#confirm similar
table(ticket.train$Ticket)
table(ticket.test$Ticket)

#Random Forest
#install.package("randomForest")
library(randomForest)

#find out the column with the response variable
names(ticket.train)
out.ticket = randomForest(x = ticket.train[,-17], y = ticket.train$Ticket,
                         xtest = ticket.test[,-17], ytest = ticket.test$Ticket,
                         replace = TRUE, #use bootstrap
                         keep.forest= TRUE, # store trees for future prediction
                         ntree = 50, # number of trees
                         mtry= 5, # rule of thumb is p/3
                         nodesize= 25)
out.ticket

#demostrate a new prediction
new.obs
predict(out.ticket, newdata = new.obs)

#variable importance236
# to understand a little about the model
round(importance(out.ticket), 0)
varImpPlot(out.ticket)





