# Housing prediction
#importing the csv
df=read.csv("Housing 31.csv")
df=read.csv("Housing.csv")
getwd()

colnames(df)
dim(df)
#importing packages
library(caret)
library(car)
library(corrplot)
library(corrgram)
library(lmtest)
#Data partitioning
x=createDataPartition(df$medv,p=0.7,list = FALSE)
x=createDataPartition(df$medv,p=0.8,list = FALSE)
dim(x)
#train and test dataset
train<-df[x,]
test<-df[-x,]
dim(test)

#univariate analysis
a<-lm(train$medv~crim, data = train)
summary(a)
plot()
# feature selection
#training data fit
fit1<-lm(train$log(medv)~nox+rm+age+dis+tax+ptratio+black, data = train)
summary(fit1)
fit8<-lm(train$medv~crim+nox+rm+dis+ptratio+lstat, data = train)
summary(fit8)
#testing data fit
fit1<-lm(test$medv~lstat+ptratio, data = test)
summary(fit1)
#other multivariate analysis
fit7<-lm(train$medv~rm+age+dis+tax+ptratio+black, data = train)
summary(fit7)
fit2<-lm(train$medv~nox+rm+age+dis+tax+ptratio+black, data = train)
summary(fit2)
fit3<-lm(train$medv~zn+chas+nox+rm+rad+dis+tax+ptratio+black+lstat, data = train)
summary(fit3)
fit4<-lm(train$medv~rm+tax+ptratio+black+lstat, data = train)
summary(fit4)
#correlation plot
y=cor(df)
corrplot(y)

#VIF
vif(fit1)

 #plotting
plot(fit1)
View(train)

#K fold validation
data(fit1)
train_control<-trainControl(method='cv',number=10)
model<-train(train$medv~crim+nox+rm+age+dis+rad+ptratio+black,df, trControl=train_control(method="lm",number = 10)
print(model)

#prediction
#training set prediction+tax
train$pred<-predict(fit1,newdata = train)
View(train)
#test set prediction
test$pred<-predict(fit1,newdata = test)
View(test)

#error 
#train
train$err<-(train$medv-train$pred)
train$err2<-(train$err)^2
sum(train$err2)
mean(train$err2)  
#test
test$err<-(test$medv-test$pred)
test$err2<-(test$err)^2
sum(test$err2)
mean(test$err2) 

#RMSE
#train
actual<-train$medv
predicted<-train$pred
RMSE(actual,predicted)
#test
act<-test$medv
pred<-test$pred
RMSE(act,pred)

lines(train$medv, col='blue')
lines(train$pred,col='red')

plot(test$medv,col='blue')
lines(test$pred,col='red')

#linearity
crPlots(fit1)

#normality
qqPlot(fit1)

#heteroscasdiscity
library(lmtest)
bptest("fit3")

#autocorrelation
durbinWatsonTest(fit1)
library(base)

#cooks distance plot
plot(cooks.distance(lm(train$medv~crim+nox+rm+age+dis+rad+tax+ptratio+black, data = train)))


library(rms)
install.packages("rms")
robcov(fit1)



train_control <- trainControl(method="cv", number=10)
# train the model
model <- train(fit, data=df, trControl=train_control, method="lm")


# summarize results
print(model
      
      
colnames(train$rad)<-c("radi")
outlier.test(fit1,cutoff=inf,n.max=15)
