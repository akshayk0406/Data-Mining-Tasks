#Problem Statement:- http://www2.research.att.com/~volinsky/DataMining/Columbia2011/HW/HW4.html

#Problem 1
setwd("/Users/akshaykulkarni/Documents/Data-Mining-Tasks")
spam = read.csv("data/spam.csv")
del_cols = c("isuid","id","domain","spampct","category")
spam = spam[,!(names(spam) %in% del_cols)] 

library(caTools)
library(rpart)
library(rpart.plot)
set.seed(786)

split = sample.split(spam$spam,SplitRatio = 0.8)
train = subset(spam,split==TRUE)
test  = subset(spam,split==FALSE)

cart_spam = rpart(spam~.,data=train,method="class")
prp(cart_spam)
print(cart_spam)
predict_spam = predict(cart_spam,newdata=test,type="prob")
table(test$spam,predict_spam[,2]>0.5)

library(ROCR)
pred = prediction(predict_spam[,2],test$spam)
perf = performance(pred,"tpr","fpr")
plot(perf)
auc = as.numeric(performance(pred,"auc")@y.values)

#Problem 2
music = read.csv("data/music-full.csv")
del_cols = c("X","X.1","Artist","Type")
music_std = music[,!(names(music) %in% del_cols)]

for(i in 1:ncol(music_std))
  music_std[,i] = (music_std[,i] - mean(music_std[,i]))/std(music_std[,i])

par(mfrow=c(3,1))
distances = dist(music_std,method="euclidean")
cluster_movies_single = hclust(distances,method="single")
cluster_movies_complete = hclust(distances,method="complete")
cluster_movies_ward = hclust(distances,method="ward.D")

plot(cluster_movies_single,labels=music$Artist,main="Clustering method-Single. By - Artist",cex=0.8)
plot(cluster_movies_complete,labels=music$Artist,main="Clustering method-complete By - Artist",cex=0.8)
plot(cluster_movies_ward,labels=music$Artist,main="Clustering method-Ward By - Artist",cex=0.8)

par(mfrow=c(3,1))
plot(cluster_movies_single,labels=music$Type,main="Clustering method-Single. By - Type",cex=0.8)
plot(cluster_movies_complete,labels=music$Type,main="Clustering method-complete By - Type",cex=0.8)
plot(cluster_movies_ward,labels=music$Type,main="Clustering method-Ward By - Type",cex=0.8)