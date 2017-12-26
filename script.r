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


#Lecturas de psd

s1.psd.train1 = read.csv("train_subject1_psd01.asc", header = FALSE, sep="")
s1.psd.train2 = read.csv("train_subject1_psd02.asc", header = FALSE, sep="")
s1.psd.train3 = read.csv("train_subject1_psd03.asc", header = FALSE, sep="")

s2.psd.train1 = read.csv("train_subject2_psd01.asc", header = FALSE, sep="")
s2.psd.train2 = read.csv("train_subject2_psd02.asc", header = FALSE, sep="")
s2.psd.train3 = read.csv("train_subject2_psd03.asc", header = FALSE, sep="")

s3.psd.train1 = read.csv("train_subject3_psd01.asc", header = FALSE, sep="")
s3.psd.train2 = read.csv("train_subject3_psd02.asc", header = FALSE, sep="")
s3.psd.train3 = read.csv("train_subject3_psd03.asc", header = FALSE, sep="")

s1.psd.test = read.csv("test_subject1_psd04.asc", header = FALSE, sep="")
s3.psd.test = read.csv("test_subject3_psd04.asc", header = FALSE, sep="")
s2.psd.test = read.csv("test_subject2_psd04.asc", header = FALSE, sep="")

s1.psd.train12 = rbind(s1.psd.train1, s1.psd.train2)
s1.psd.train13 = rbind(s1.psd.train1, s1.psd.train3)
s1.psd.train23 = rbind(s1.psd.train2, s1.psd.train3)

library(MASS)

colnames(s1.psd.train12) <- c('s1.8', 's1.10', 's1.12', 's1.14', 's1.16', 's1.18', 's1.20', 's1.22', 's1.24', 's1.26', 's1.28', 's1.30', 's2.8', 's2.10', 's2.12', 's2.14', 's2.16', 's2.18', 's2.20', 's2.22', 's2.24', 's2.26', 's2.28', 's2.30', 's3.8', 's3.10', 's3.12', 's3.14', 's3.16', 's3.18', 's3.20', 's3.22', 's3.24', 's3.26', 's3.28', 's3.30', 's4.8', 's4.10', 's4.12', 's4.14', 's4.16', 's4.18', 's4.20', 's4.22', 's4.24', 's4.26', 's4.28', 's4.30', 's5.8', 's5.10', 's5.12', 's5.14', 's5.16', 's5.18', 's5.20', 's5.22', 's5.24', 's5.26', 's5.28', 's5.30', 's6.8', 's6.10', 's6.12', 's6.14', 's6.16', 's6.18', 's6.20', 's6.22', 's6.24', 's6.26', 's6.28', 's6.30', 's7.8', 's7.10', 's7.12', 's7.14', 's7.16', 's7.18', 's7.20', 's7.22', 's7.24', 's7.26', 's7.28', 's7.30', 's8.8', 's8.10', 's8.12', 's8.14', 's8.16', 's8.18', 's8.20', 's8.22', 's8.24', 's8.26', 's8.28', 's8.30', 'C')
colnames(s1.psd.train13) <- c('s1.8', 's1.10', 's1.12', 's1.14', 's1.16', 's1.18', 's1.20', 's1.22', 's1.24', 's1.26', 's1.28', 's1.30', 's2.8', 's2.10', 's2.12', 's2.14', 's2.16', 's2.18', 's2.20', 's2.22', 's2.24', 's2.26', 's2.28', 's2.30', 's3.8', 's3.10', 's3.12', 's3.14', 's3.16', 's3.18', 's3.20', 's3.22', 's3.24', 's3.26', 's3.28', 's3.30', 's4.8', 's4.10', 's4.12', 's4.14', 's4.16', 's4.18', 's4.20', 's4.22', 's4.24', 's4.26', 's4.28', 's4.30', 's5.8', 's5.10', 's5.12', 's5.14', 's5.16', 's5.18', 's5.20', 's5.22', 's5.24', 's5.26', 's5.28', 's5.30', 's6.8', 's6.10', 's6.12', 's6.14', 's6.16', 's6.18', 's6.20', 's6.22', 's6.24', 's6.26', 's6.28', 's6.30', 's7.8', 's7.10', 's7.12', 's7.14', 's7.16', 's7.18', 's7.20', 's7.22', 's7.24', 's7.26', 's7.28', 's7.30', 's8.8', 's8.10', 's8.12', 's8.14', 's8.16', 's8.18', 's8.20', 's8.22', 's8.24', 's8.26', 's8.28', 's8.30', 'C')
colnames(s1.psd.train23) <- c('s1.8', 's1.10', 's1.12', 's1.14', 's1.16', 's1.18', 's1.20', 's1.22', 's1.24', 's1.26', 's1.28', 's1.30', 's2.8', 's2.10', 's2.12', 's2.14', 's2.16', 's2.18', 's2.20', 's2.22', 's2.24', 's2.26', 's2.28', 's2.30', 's3.8', 's3.10', 's3.12', 's3.14', 's3.16', 's3.18', 's3.20', 's3.22', 's3.24', 's3.26', 's3.28', 's3.30', 's4.8', 's4.10', 's4.12', 's4.14', 's4.16', 's4.18', 's4.20', 's4.22', 's4.24', 's4.26', 's4.28', 's4.30', 's5.8', 's5.10', 's5.12', 's5.14', 's5.16', 's5.18', 's5.20', 's5.22', 's5.24', 's5.26', 's5.28', 's5.30', 's6.8', 's6.10', 's6.12', 's6.14', 's6.16', 's6.18', 's6.20', 's6.22', 's6.24', 's6.26', 's6.28', 's6.30', 's7.8', 's7.10', 's7.12', 's7.14', 's7.16', 's7.18', 's7.20', 's7.22', 's7.24', 's7.26', 's7.28', 's7.30', 's8.8', 's8.10', 's8.12', 's8.14', 's8.16', 's8.18', 's8.20', 's8.22', 's8.24', 's8.26', 's8.28', 's8.30', 'C')

colnames(s1.psd.train12) <- make.names(colnames(s1.psd.train12))
colnames(s1.psd.train13) <- make.names(colnames(s1.psd.train13))
colnames(s1.psd.train23) <- make.names(colnames(s1.psd.train23))

colnames(s1.psd.train1) <- c('s1.8', 's1.10', 's1.12', 's1.14', 's1.16', 's1.18', 's1.20', 's1.22', 's1.24', 's1.26', 's1.28', 's1.30', 's2.8', 's2.10', 's2.12', 's2.14', 's2.16', 's2.18', 's2.20', 's2.22', 's2.24', 's2.26', 's2.28', 's2.30', 's3.8', 's3.10', 's3.12', 's3.14', 's3.16', 's3.18', 's3.20', 's3.22', 's3.24', 's3.26', 's3.28', 's3.30', 's4.8', 's4.10', 's4.12', 's4.14', 's4.16', 's4.18', 's4.20', 's4.22', 's4.24', 's4.26', 's4.28', 's4.30', 's5.8', 's5.10', 's5.12', 's5.14', 's5.16', 's5.18', 's5.20', 's5.22', 's5.24', 's5.26', 's5.28', 's5.30', 's6.8', 's6.10', 's6.12', 's6.14', 's6.16', 's6.18', 's6.20', 's6.22', 's6.24', 's6.26', 's6.28', 's6.30', 's7.8', 's7.10', 's7.12', 's7.14', 's7.16', 's7.18', 's7.20', 's7.22', 's7.24', 's7.26', 's7.28', 's7.30', 's8.8', 's8.10', 's8.12', 's8.14', 's8.16', 's8.18', 's8.20', 's8.22', 's8.24', 's8.26', 's8.28', 's8.30', 'C')
colnames(s1.psd.train2) <- c('s1.8', 's1.10', 's1.12', 's1.14', 's1.16', 's1.18', 's1.20', 's1.22', 's1.24', 's1.26', 's1.28', 's1.30', 's2.8', 's2.10', 's2.12', 's2.14', 's2.16', 's2.18', 's2.20', 's2.22', 's2.24', 's2.26', 's2.28', 's2.30', 's3.8', 's3.10', 's3.12', 's3.14', 's3.16', 's3.18', 's3.20', 's3.22', 's3.24', 's3.26', 's3.28', 's3.30', 's4.8', 's4.10', 's4.12', 's4.14', 's4.16', 's4.18', 's4.20', 's4.22', 's4.24', 's4.26', 's4.28', 's4.30', 's5.8', 's5.10', 's5.12', 's5.14', 's5.16', 's5.18', 's5.20', 's5.22', 's5.24', 's5.26', 's5.28', 's5.30', 's6.8', 's6.10', 's6.12', 's6.14', 's6.16', 's6.18', 's6.20', 's6.22', 's6.24', 's6.26', 's6.28', 's6.30', 's7.8', 's7.10', 's7.12', 's7.14', 's7.16', 's7.18', 's7.20', 's7.22', 's7.24', 's7.26', 's7.28', 's7.30', 's8.8', 's8.10', 's8.12', 's8.14', 's8.16', 's8.18', 's8.20', 's8.22', 's8.24', 's8.26', 's8.28', 's8.30', 'C')
colnames(s1.psd.train3) <- c('s1.8', 's1.10', 's1.12', 's1.14', 's1.16', 's1.18', 's1.20', 's1.22', 's1.24', 's1.26', 's1.28', 's1.30', 's2.8', 's2.10', 's2.12', 's2.14', 's2.16', 's2.18', 's2.20', 's2.22', 's2.24', 's2.26', 's2.28', 's2.30', 's3.8', 's3.10', 's3.12', 's3.14', 's3.16', 's3.18', 's3.20', 's3.22', 's3.24', 's3.26', 's3.28', 's3.30', 's4.8', 's4.10', 's4.12', 's4.14', 's4.16', 's4.18', 's4.20', 's4.22', 's4.24', 's4.26', 's4.28', 's4.30', 's5.8', 's5.10', 's5.12', 's5.14', 's5.16', 's5.18', 's5.20', 's5.22', 's5.24', 's5.26', 's5.28', 's5.30', 's6.8', 's6.10', 's6.12', 's6.14', 's6.16', 's6.18', 's6.20', 's6.22', 's6.24', 's6.26', 's6.28', 's6.30', 's7.8', 's7.10', 's7.12', 's7.14', 's7.16', 's7.18', 's7.20', 's7.22', 's7.24', 's7.26', 's7.28', 's7.30', 's8.8', 's8.10', 's8.12', 's8.14', 's8.16', 's8.18', 's8.20', 's8.22', 's8.24', 's8.26', 's8.28', 's8.30', 'C')

colnames(s1.psd.train1) <- make.names(colnames(s1.psd.train1))
colnames(s1.psd.train2) <- make.names(colnames(s1.psd.train2))
colnames(s1.psd.train3) <- make.names(colnames(s1.psd.train3))

s1.psd.train12$C <- as.factor(s1.psd.train12$C)
s1.psd.train13$C <- as.factor(s1.psd.train13$C)
s1.psd.train23$C <- as.factor(s1.psd.train23$C)

qda.psd12.model <- qda (C ~ ., data = s1.psd.train12)
qda.psd13.model <- qda (C ~ ., data = s1.psd.train13)
qda.psd23.model <- qda (C ~ ., data = s1.psd.train23)

qda.psd12.pred <- predict(qda.psd12.model)
qda.psd13.pred <- predict(qda.psd13.model)
qda.psd23.pred <- predict(qda.psd23.model)

qda.pred3 <- predict(qda.psd12.model, s1.psd.train3)
qda.pred2 <- predict(qda.psd13.model, s1.psd.train2)
qda.pred1 <- predict(qda.psd23.model, s1.psd.train1)

#tablas
tab <- table(s1.psd.train1$C, qda.pred1$class)
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))
table(s1.psd.train2$C, qda.pred2$class)
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))
table(s1.psd.train3$C, qda.pred3$class)
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))


#plot(qda.psd12.pred$x,type="n")
#points(qda.psd12.pred$x, cex=0.01 ,col=as.integer(s1.psd.train12$C))

#plot(qda.psd13.pred$x,type="n")
#points(qda.psd13.pred$x, cex=0.01 ,col=as.integer(s1.psd.train13$C))

#plot(qda.psd23.pred$x,type="n")
#points(qda.psd23.pred$x, cex=0.01 ,col=as.integer(s1.psd.train23$C))




