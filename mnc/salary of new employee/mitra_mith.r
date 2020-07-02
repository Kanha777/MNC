rm(list=ls(all=TRUE))


#reading
#Loading data
data<-read.csv(file="train.csv",header=T,sep=",")

dim(data)
str(data)
summary(data)


#Removing unique column
data$Candidate.ID=NULL
data$Date.Of.Birth=NULL
data$State=NULL
data<-data[!duplicated(data),]


#Checking missing values
data$School.Board.in.Tenth[data$School.Board.in.Tenth==0]=NA
data$Board.in.Twelth[data$Board.in.Twelth==0]=NA
data$Score.in.Domain[data$Score.in.Domain==-1]=NA
sum(is.na(data))
#11018

#factor
data$CityTier=as.factor(data$CityTier)
data$CollegeTier=as.factor(data$CollegeTier)
data$School.Board.in.Tenth=as.numeric(data$School.Board.in.Tenth)
data$Board.in.Twelth=as.numeric(data$Board.in.Twelth)
str(data)
summary(data)

#Handling Missing Values
library(DMwR)
data<-centralImputation(data)
summary(data)
sum(is.na(data))
#0
#partition
library(caret)
set.seed(123)
trainrows<-createDataPartition(data$Pay_in_INR,p=0.7,list = F)
train<-data[trainrows,]
test<-data[-trainrows,]

#random forest

library(randomForest)
model_rf <- randomForest(Pay_in_INR~ ., data= train, ntree=10,mtry = 5)
plot(model_rf)
print(model_rf)
pred1=predict(model_rf,test)
pred2=predict(model_rf,train)

library(DMwR)
regr.eval(train$Pay_in_INR,pred2)
#mae          mse         rmse         mape 
#5.504661e+04 1.185770e+10 1.088931e+05 1.166739e-01 

regr.eval(test$Pay_in_INR,pred1)
#mae          mse         rmse         mape 
#1.139253e+05 4.138393e+10 2.034304e+05 2.616111e-01 

regr.eval(train$Pay_in_INR,data2)



#decision tree

library(rpart)
library(rpart.plot)
DT_rpart_Reg<-rpart(Pay_in_INR~.,data=train,method="anova")
DT_rpart_Reg<-rpart(Pay_in_INR~.,data=train,method="anova",control =rpart.control(cp = 0.001))
printcp(DT_rpart_Reg)
rpart.plot(DT_rpart_Reg)
predCartTrain=predict(DT_rpart_Reg, newdata=train, type="vector")
predCartTest=predict(DT_rpart_Reg, newdata=test, type="vector")
regr.eval(train[,"Pay_in_INR"], predCartTrain)

#mae          mse         rmse         mape 
#1.318162e+05 5.066062e+10 2.250791e+05 2.858330e-01 

regr.eval(test[,"Pay_in_INR"], predCartTest)
#mae          mse         rmse         mape 
#1.442336e+05 6.661448e+10 2.580978e+05 3.190960e-01


################test.csv############
dat=read.csv("test.csv")
dim(dat)
str(dat)
summary(dat)


#Check for missing values
ssc.tenth=dat[,5]
ssc.tenth=data.frame(ssc.tenth)
ssc.tenth[ssc.tenth==0]=NA
sum(is.na(ssc.tenth))
#2138

library(DMwR)
t1=centralImputation(ssc.tenth)
sum(is.na(t1))
#0

#12 mark
Board.in.Twelth=dat[,8]
Board.in.Twelth=data.frame(Board.in.Twelth)
Board.in.Twelth[Board.in.Twelth==0]=NA
sum(is.na(Board.in.Twelth))
#2155

library(DMwR)
t2=centralImputation(Board.in.Twelth)
sum(is.na(t2))

#unique
dat$Candidate.ID=NULL
dat$School.Board.in.Tenth=NULL
dat$Board.in.Twelth=NULL

#databind
finaldat=cbind(dat,t1,t2)
anyDuplicated(finaldat)#0
str(finaldat)

#Type conversions
finaldat$Year.of.Graduation.Completion=as.factor(finaldat$Year.of.Graduation.Completion)
finaldat$Year.Of.Twelth.Completion=as.factor(finaldat$Year.Of.Twelth.Completion)
finaldat$Graduation=as.factor(finaldat$Graduation)
finaldat$CollegeTier=as.factor(finaldat$CollegeTier)
finaldat$CityTier=as.factor(finaldat$CityTier)
finaldat$Date.Of.Birth=as.character.Date(finaldat$Date.Of.Birth)
Date.Of.Birth=ifelse(finaldat$Date.Of.Birth >="1992-02-01 00:00",1,0)
D.O.B=data.frame(Date.Of.Birth)
t4=ifelse(finaldat[,18:25] <="-100",0,1)
t4=data.frame(t4)
finaldat[,18:25]=NULL
finaldat$Date.Of.Birth=NULL

finaldatatest=cbind(finaldat,D.O.B)
str(finaldatatest)
names=c(26:32)
finaldatatest[,names]=lapply(finaldatatest[,names],factor)
str(finaldatatest)

finaldatatest$pay<-0


#rpart
library(rpart)
model_test <- rpart(pay ~ . ,data =  finaldatatest,method = "anova") 
pred=predict(model_test,newdata = finaldatatest)

regr.eval(finaldatatest$pay,predCartTrain)
#mae          mse         rmse         mape 
#1.092158e+06 8.041689e+11 8.967546e+05          Inf 

regr.eval(finaldatatest$pay,predCartTest)
#mae          mse         rmse         mape 
#6.369291e+05 4.608780e+11 6.788799e+05          Inf 

#Warning messages:
#  1: In trues - preds :
#  longer object length is not a multiple of shorter object length
#2: In trues - preds :
#  longer object length is not a multiple of shorter object length
#3: In trues - preds :
  #longer object length is not a multiple of shorter object length

