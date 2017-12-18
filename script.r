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
