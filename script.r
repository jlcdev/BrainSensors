s1.train1 = read.csv("train_subject1_raw01.asc", header = FALSE, sep="")
s1.train2 = read.csv("train_subject1_raw02.asc", header = FALSE, sep="")
s1.train3 = read.csv("train_subject1_raw03.asc", header = FALSE, sep="")

s2.train1 = read.csv("train_subject2_raw01.asc", header = FALSE, sep="")
s2.train2 = read.csv("train_subject2_raw02.asc", header = FALSE, sep="")
s2.train3 = read.csv("train_subject2_raw03.asc", header = FALSE, sep="")

s3.train1 = read.csv("train_subject3_raw01.asc", header = FALSE, sep="")
s3.train2 = read.csv("train_subject3_raw02.asc", header = FALSE, sep="")
s3.train3 = read.csv("train_subject3_raw03.asc", header = FALSE, sep="")

s1.test = read.csv("test_subject1_raw04.asc", header = FALSE, sep="")
s3.test = read.csv("test_subject3_raw04.asc", header = FALSE, sep="")
s2.test = read.csv("test_subject2_raw04.asc", header = FALSE, sep="")

s1.train = rbind(s1.train1, s1.train2)
s1.train = rbind(s1.train, s1.train3)

s2.train = rbind(s2.train1, s2.train2)
s2.train = rbind(s2.train, s2.train3)

s3.train = rbind(s3.train1, s3.train2)
s3.train = rbind(s3.train, s3.train3)

library(MASS)

colnames(s1.train) <- c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 'S11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20', 's21', 's22', 's23', 's24', 's25', 's26', 's27', 's28', 's29', 's30', 's31', 's32', 'C')
colnames(s2.train) <- c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 'S11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20', 's21', 's22', 's23', 's24', 's25', 's26', 's27', 's28', 's29', 's30', 's31', 's32', 'C')
colnames(s3.train) <- c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 'S11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20', 's21', 's22', 's23', 's24', 's25', 's26', 's27', 's28', 's29', 's30', 's31', 's32', 'C')

colnames(s1.train) <- make.names(colnames(s1.train))
colnames(s2.train) <- make.names(colnames(s2.train))
colnames(s3.train) <- make.names(colnames(s3.train))

s1.train$C <- as.factor(s1.train$C)
s2.train$C <- as.factor(s2.train$C)
s3.train$C <- as.factor(s3.train$C)

lda.s1.model <- lda (C ~ ., data = s1.train)
s1.lda.pred <- predict(lda.s1.model)
plot(s1.lda.pred$x,type="n")
text(s1.lda.pred$x,labels=as.character(rownames(s1.lda.pred$x)),col=as.integer(s1.train$C))

plot.mean <- function (class)
{
  m1 <- mean(subset(s1.lda.pred$x[,1],s1.train$C==class))
  m2 <- mean(subset(s1.lda.pred$x[,2],s1.train$C==class))
  print(c(m1,m2))
  points(m1,m2,pch=16,cex=2,col=as.integer(class))
}
plot.mean ('1')
plot.mean ('2')
plot.mean ('3')

s1.qda.model <- qda(C ~ ., data = s1.train)
s1.qda.pred <- predict(s1.qda.model)
table(s1.train$C, s1.qda.pred$class)
s1.qda.predcv <- update(s1.qda.model, CV=TRUE)

library(klaR)

s1.rda.model <- rda (C ~ ., data = s1.train)

library(e1071)

set.seed(1111)
N <- nrow(s1.train)
s1.nb.learn <- sample(1:N, round(2*N/3))
nlearn <- length(s1.nb.learn)
ntest <- N - nlearn
s1.nb.model <- naiveBayes(C ~ ., data = s1.train[s1.nb.learn,])
s1.nb.pred <- predict(s1.nb.model, s1.train[s1.nb.learn,-1])
table(s1.nb.pred, s1.train[s1.nb.learn,]$C) 

