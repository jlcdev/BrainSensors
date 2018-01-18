library(dplyr)
library(randomForest)
set.seed(13)

s1.train1 = read.csv("data/train_subject2_raw01.asc", header = FALSE, sep="")
s1.train2 = read.csv("data/train_subject2_raw02.asc", header = FALSE, sep="")
s1.train3 = read.csv("data/train_subject2_raw03.asc", header = FALSE, sep="")

s1.train = rbind(s1.train1, s1.train2)
s1.train = rbind(s1.train, s1.train3)

colnames(s1.train) <- c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 'S11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20', 's21', 's22', 's23', 's24', 's25', 's26', 's27', 's28', 's29', 's30', 's31', 's32', 'C')
colnames(s1.train) <- make.names(colnames(s1.train))

s1.train$C <- as.factor(s1.train$C)

train<-sample_frac(s1.train, 0.7)
sid<-as.numeric(rownames(train))
test<-s1.train[-sid,]

#Get best Random Forest model computing all between 10 and 1000 and select 
#Posible trees: 10   16   25   40   63  100  158  251  398  631 1000
computeRandomForest <- function(data,sid, trees=round(10^seq(1,3,by=0.2))){
  train <- data[sid,]
  m <- matrix (rep(0,2*length(trees)), nrow=length(trees))
  m[,1] <- trees
  m[,2] <- 0
  
  for(t in trees){
    model <- randomForest(C ~.,data=train,ntree=t,proximity=F)
    m[1,2] <- model$err.rate[t,1]
  }
  
  le <- as.integer(which.min(m[,2]))
  modelResult <- randomForest(C ~ ., data = train, ntree=m[le,1], proximity=F)
  return(modelResult)
}

#get best Random Forest model using loaded data.
model = computeRandomForest(s1.train, sid)
save(model, file="models/randomForest_16_5_s2.model")

#show obtained model
print(model)

#get important columns
round(importance(model), 2)


pred <- as.factor(predict (model, newdata=test, type = "class"))
tab <- table(pred,test$C)
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))