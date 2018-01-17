library(dplyr)
library(MASS)
library(nnet)
library(caret)
library(doParallel)

#MLP
s1.train1 = read.csv("data/train_subject1_raw01.asc", header = FALSE, sep="")
s1.train2 = read.csv("data/train_subject1_raw02.asc", header = FALSE, sep="")
s1.train3 = read.csv("data/train_subject1_raw03.asc", header = FALSE, sep="")

s1.train = rbind(s1.train1, s1.train2)
s1.train = rbind(s1.train, s1.train3)

colnames(s1.train) <- c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 'S11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20', 's21', 's22', 's23', 's24', 's25', 's26', 's27', 's28', 's29', 's30', 's31', 's32', 'C')
colnames(s1.train) <- make.names(colnames(s1.train))

s1.train$C <- as.factor(s1.train$C)

train<-sample_frac(s1.train, 0.7)
sid<-as.numeric(rownames(train))
test<-s1.train[-sid,]

#decays <- c(0, 0.001, 0.01, 0.1, 0.5)
#trc <- trainControl (method="repeatedcv", number=10, repeats=5)

for (i in 1:32)
{
  train[,i] <- scale(train[,i])
  test[,i] <- scale(test[,i])
}

#For find best parameters
#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
#model <- train(C~., data = train, method = 'nnet', maxit = 100, trace = FALSE, 
#tuneGrid = expand.grid(.size=2, .decay=decays), trControl = trc)
#stopCluster(cl)
#model$bestTune
#model$results

model <- nnet(C ~., data=train, size=15, maxit=5000, decay=0.5)

#load("models/mlp_15_0.5_s1.model")
#save(model, file="models/mlp_15_0.5_s1.model")

pred.train <- as.factor(predict (model, type="class"))
t1 <- table(pred.train,train$C)
t1
(error <- 100*(1-sum(t1[row(t1)==col(t1)])/sum(t1)))


pred <- as.factor(predict (model, newdata=test, type = "class"))
tab <- table(pred,test$C)
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))
