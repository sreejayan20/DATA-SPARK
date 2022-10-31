library(rpart)
getwd()
cd<-read.csv("D:/churn.csv")
View(cd)
#Preprocessing
colnames(cd)

#Delete attributes
library(dplyr)
cm<-cd%>%select(-RowNumber)%>%select(-CustomerId)%>%select(-Surname)
colnames(cm)
unique(cm$NumOfProducts)

#Factorization

cm 
cm$IsActiveMember<-factor(cm$IsActiveMember,levels=c(0,1),labels=c("NO","YES"))
cm$EstimatedSalary<-cut(cm$EstimatedSalary, breaks=c(-Inf, 5000, 100000, 150000, 200000), labels=c("Low", "Middle", "High", "Upper"))
clm<-cm
View(clm)
cm$Exited<-factor(cm$Exited,levels=c(0,1),labels=c("NO","YES"))

summary(cd$EstimatedSalary)



View(cm)
library(ggplot2)
ggplot(data=cm, aes(x=cm$IsActiveMember, fill= cm$HasCrCard))+geom_bar(position="dodge")
ggplot(data=cm, aes(x=cm$EstimatedSalary, fill= cm$HasCrCard))+geom_bar(position="dodge")

ggplot(data=cm, aes(x=Age,y=Exited, fill=Exited))+geom_boxplot()

c<-unique(cm$Geography)
c
x<-table(cm$Geography)/100
x
pie(x, labels=c, main="Geography", col=c("purple", "blue", "pink"))
#ggplot(data=cm, aes(x=cm$Age ,y=cm$Tenure, shape=cm$Gender, color=cm$NumOfProducts))+geom_point()

ggplot(data=cm, aes(x=cm$Geography,fill=cm$Exited))+geom_bar(position="dodge")


#Spliting
library(caTools)
split=sample.split(cm,SplitRatio = 0.6)
test_dt<-subset(cm,split==TRUE)
train_dt<-subset(cm,split==FALSE)
#View(train_dt)
#View(test_dt)

#model & prediction

#Decision Tree
library(rpart)
library(rpart.plot)
dt<-rpart(Exited~.,data=train_dt,method="class")
dt
rpart.plot(dt)
pred=predict(dt,test_dt,type="class")
ct<-table(pred,test_dt$Exited)
ct

#RandomForest
library(randomForest)
rf_model<-randomForest(Exited~.,data=train_dt,ntree=50,type="classification")
rf_model
pred=predict(rf_model,test_dt,type="class")
rf<-table(pred,test_dt$Exited)
rf


#Logistic Regression
library(caret)
View(clm)
split=sample.split(clm,SplitRatio = 0.6)
testdt<-subset(clm,split==TRUE)
traindt<-subset(clm,split==FALSE)

lml <- glm(Exited ~., data = traindt, family = "binomial")
lml
predict_dt <- predict(lml, newdata=testdt, type = "response")
predict_dt 
crf<-table( testdt$Exited, predict_dt)
crf
r_sq_3 <- summary(lml)$r.squared
r_sq_3

#Naive Bayyes
library(e1071)
cl <- naiveBayes(Exited ~ ., data = train_dt)
cl
y_pred <- predict(cl, newdata = test_dt)
cm <- table(test_dt$Exited, y_pred)
cm


# New Values
#Traing and Testing
set.seed(222)
split=sample(2,nrow(Exited),replace = T,prob=c(0.8,0.2))
train<-cm[split==1,]
test<-cm[split==2,]


h<-read.csv("C:/Users/Admin/Documents/r/predex.csv")
#View(h)
#Random Forest
set.seed(333)
library(randomForest)
rfp<-randomForest(Exited~.,data=train)
rfp
pred_dt <- predict(rfp, h)
pred_dt


library(Boruta)
#feature_selection
feature_select <- Boruta(Exited ~ ., data = cm)
feature_select$finalDecision





#CONFUSION MATRICES
#RANDOM FOREST
confusionMatrix(rf)

#DECISION TREE
confusionMatrix(ct)

#LOGISTIC REFRESSION
confusionMatrix(crf)

#NAIVE BAYES
confusionMatrix(cm)
