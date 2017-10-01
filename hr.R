hr <- read.csv("D:/kaggle/HR/HR_comma_sep.csv", header=TRUE)
View(hr)
summary(hr)
library(corrplot)
library(ggplot2)
library(dplyr)
library(e1071)
library(caret)
library(rpart.plot)
library(rpart)
#visualisations

hr_cor = hr %>% select(satisfaction_level:promotion_last_5years)
cor(hr_cor)
corrplot(cor(hr_cor), method="circle")#On average people who leave have a low satisfaction level, they work more and didn't get promoted within the past five years.

# data of people who left
hr_left = hr%>% filter(left==1)
nrow(hr_left) #people left

#why we dont retain everyone
hist(hr_left$last_evaluation,col="green", main = "Last evaluation")
hist(hr_left$time_spend_company,col="blue", main = "time spend company")
hist(hr_left$number_project,col="red", main = "number of projects")

#people the company should have retained.
hr_left_retained = hr_left %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
nrow(hr_left_retained)

#why valuable people leave

hr_valuable_people =  hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
cor(hr_valuable_people[,1:8])
corrplot(cor(hr_valuable_people[,1:8]),method="circle")
nrow(hr_valuable_people) #number of valuable people


#to find which valuable people will leave
#modelling 
 hr_model = hr_valuable_people
 #cross-validation(k-fold)
 hr_model$left <- as.factor(hr_model$left)
 train_control<- trainControl(method="cv", number=5, repeats=3)
 
 #tree model
 
 # train the model 
 rpartmodel<- train(left~., data=hr_model, trControl=train_control, method="rpart")
 # make predictions
 predictions<- predict(rpartmodel,hr_model)
 table(predictions)
 # summarize results
 confusionMatrix<- confusionMatrix(predictions,hr_model$left)
 confusionMatrix
#ROC
 predictions = as.numeric(predictions)
 pred = prediction(predictions,hr_model$left)
 perf = performance(pred,"tpr", "fpr")
 plot(perf, main=" ROC Curves",
       xlab="1 - Specificity: False Positive Rate",
       ylab="Sensitivity: True Positive Rate",
       col="blue")
  abline(0,1,col="black")
  as.numeric(performance(pred, "auc")@y.values) #auc value
  
  #naives bayes
  
    # train the model 
  nb_model <- train(left~., data=hr_model, trControl=train_control, method="nb")
  # make predictions
  predictions_nb<- predict(nb_model,hr_model)
  # summarize results
  confusionMatrix<- confusionMatrix(predictions_nb,hr_model$left)
  confusionMatrix
  
  #ROC
  predictions_nb = as.numeric(predictions_nb)
  pred_nb = prediction(predictions_nb,hr_model$left)
  perf_nb = performance(pred_nb,"tpr", "fpr")
  plot(perf_nb, main=" ROC Curves-NB",
       xlab="1 - Specificity: False Positive Rate",
       ylab="Sensitivity: True Positive Rate",
       col="blue")
  abline(0,1,col="black")
  as.numeric(performance(pred_nb, "auc")@y.values) #auc value
  
  
  ## logistic regression
  # train the model
  lg_model <- train(left~., data=hr_model, trControl=train_control, method="LogitBoost")
  
  # make predictions
  predictions_lg<- predict(lg_model,hr_model)
  # summarize results
  confusionMatrix<- confusionMatrix(predictions_lg,hr_model$left)
  confusionMatrix
  
  #ROC
  predictions_lg = as.numeric(predictions_lg)
  pred_lg = prediction(predictions_lg,hr_model$left)
  perf_lg = performance(pred_lg,"tpr", "fpr")
  plot(perf_lg, main=" ROC Curves-LG",
       xlab="1 - Specificity: False Positive Rate",
       ylab="Sensitivity: True Positive Rate",
       col="blue")
  abline(0,1,col="black")
  as.numeric(performance(pred_lg, "auc")@y.values) #auc value
   
  #probablity to leave
  #splitting the data
  
  library(caTools)
  set.seed(123)
  split = sample.split(hr_model$left, SplitRatio = 0.75)
  train = subset(hr_model, split==TRUE)
  test = subset(hr_model, split ==FALSE)
  
  lgmodel <- glm(left~ .,data = train,family="binomial")
  summary(lgmodel)
  people_leave = predict(lgmodel,newdata = test,type="response")
  df = data.frame(people_leave)
  df$satisfaction = test$satisfaction_level
  df$perf = test$last_evaluation
  plot(df$people_leave,df$perf)
  plot(df$people_leave,df$satisfaction)