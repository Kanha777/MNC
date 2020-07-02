rm(list=ls(all=TRUE))


library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library("dplyr")
library(randomForest)


###set working directory###
setwd("C:/Users/User/Desktop/data science project/mnc/churn analysis")


#load data##

data<-read.csv(file="churn.csv",header=T,sep=",")
dim(data)
str(data)
summary(data)



#checking missin vaalue##
sum(is.na(data))
#0#

##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)


library(esquisse)
esquisse::esquisser(data)

library(ggplot2)

ggplot(data) +
 aes(x = churn, fill = state) +
 geom_bar() +
 scale_fill_hue() +
 theme_minimal()

ggplot(data) +
 aes(x = churn, y = total.day.minutes) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()



library(earth)
marsModel <- earth(churn ~ ., data=data) # build model
ev <- evimp(marsModel)
plot(ev)




data=data[,5:21]

#after feature selection##
data=data[,c(1,3,4,6,7,10,13:17)]
view(data)

data$international.plan=as.factor(data$international.plan)
data$churn=as.factor(data$churn)

#splitdata

set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




#buildmodel#

library(caret)
library(ada)

model = ada(churn~ ., iter = 20,data = train, loss="logistic")

pred = predict(model, test)

pred

confusionMatrix(test$churn,pred)

#95%#


#randomforest


library(randomForest)

model_rf <- randomForest(churn~ ., data= train, ntree=10,mtry = 5)

pred1=predict(model_rf,test)

confusionMatrix(test$churn,pred1,positive = "1")

#error#
data$international.plan=NULL

#rpart##

library(rpart)

model_rf <- rpart(churn~.,data=train,method="anova",control =rpart.control(cp = 0.001))

pred1=predict(model_rf,test)
plot(model_rf)
print(model_rf)
confusionMatrix(test$churn,pred1)

