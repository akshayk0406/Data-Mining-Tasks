#Problem statement - http://www2.research.att.com/~volinsky/DataMining/Columbia2011/HW/HW3.html
#Problem 2
setwd("/Users/akshaykulkarni/Documents/Data-Mining-Tasks")
boston = read.table("data/BostonHousing.txt",sep=" ")
random_pts = sample(1:nrow(boston),nrow(boston))
split_length = floor(nrow(boston)/4)

#Splitting observations in 4 groups
g1=boston[random_pts[(1:split_length)],]
g2=boston[random_pts[(split_length+1):(2*split_length)],]
g3=boston[random_pts[(2*split_length+1):(3*split_length)],]
g4=boston[random_pts[(3*split_length+1):(nrow(boston))],]

#Part A:
train = rbind(g1,g2,g3)
model = lm(medv~.,data=train)
predict_g4 = predict(model,g4)
mse = (sum((predict_g4-g4$medv)^2)/nrow(g4))

#Part B,C,D:
get_best_model <- function(train)
{
  best_model = NULL
  min_mse = 1e10
  for (i in 1:5)
  {
    model = lm(medv~.,data=train)
    predict_g3 = predict(model,g3)
    median_validation = median(predict_g3)
    mse = (sum((predict_g3-g3$medv)^2)/nrow(g3))
    
    if(mse < min_mse)
    {
      min_mse = mse
      best_model = model
    }
    min_t_state_col = colnames(train)[which.min(abs(summary(model)$coef[,"t value"][-1]))]
    train = train=train[,!(colnames(train) %in% c(min_t_state_col))]
  }
  list(min_mse,best_model)
}

best_model_g3 = get_best_model(rbind(g1,g2))
best_model_g2 = get_best_model(rbind(g1,g3))
best_model_g1 = get_best_model(rbind(g2,g3))

final_mse = best_model_g3[[1]]
final_best_model = best_model_g3[[2]]

if(best_model_g2[[1]] < final_mse)
{
  final_mse = best_model_g2[[1]]
  final_best_model = best_model_g2[[2]]
}

if(best_model_g1[[1]] < final_mse)
{
  final_mse = best_model_g1[[1]]
  final_best_model = best_model_g1[[2]]
}

final_predict_g4 = predict(final_best_model,newdata=g4)
final_mse = sum((final_predict_g4-g4$medv)^2)/nrow(g4)

#Problem 3
library(caTools)
set.seed(123)
spam = read.csv("data/spam.csv")
split = sample.split(spam$spam,SplitRatio=0.8)
train = subset(spam,split==TRUE)
test = subset(spam,split==FALSE)
model = glm(spam~log(size.kb+1)+digits+cappct+special,data=train,family="binomial")

predict_test = predict(model,newdata=test,type="response")
table(test$spam,predict_test>0.5)

#Plotting ROC curve
library(ROCR)
ROCR_pred = prediction(predict_test,test$spam)
ROCR_perf = performance(ROCR_pred,"tpr","fpr")
plot(ROCR_perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

model_fact = glm(spam~log(size.kb+1)+digits+cappct+special+box+local+credit+sucker
                 +porn+chain+large.text+username,data=train,family="binomial")
predict_test = predict(model_fact,newdata=test,type="response")
table(test$spam,predict_test>0.5)

ROCR_pred = prediction(predict_test,test$spam)
ROCR_perf = performance(ROCR_pred,"tpr","fpr")
plot(ROCR_perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))