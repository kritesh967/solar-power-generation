library(ggplot2)
library(forecast)
library(tseries)
library(stR)
library("Metrics")
library(corpcor)
library(lattice) 
library(car)
library(psych)
library(psy)
library(GPArotation)
library(stats)

data=read.csv("E:\\dsa\\final analytical test for R\\3.5MW POWER.csv")
summary(data$POWER)
str(data)
data$POWER <- gsub(",","",data$POWER)
data$POWER=as.numeric(as.character(data$POWER))
data$date=as.Date(data$date,format="%d-%m-%Y")
str(data)
plot(data$date,data$POWER)
plot(data)

#checking for stationary
library(tseries)
adf.test(data$POWER, alternative = "stationary", k=0)
data=data[,-4]

#building regression model
str(data)

#imputing data variables
library(VIM)
library(mice)
library(missForest)

#Check missing value pattern
md.pattern(data)
#plot missing value pattern
mice_plot <- aggr(data, col=c('grey','red'),numbers=TRUE, sortVars=TRUE,labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
summary(data)

#Start MICE imputing
imputed_Data <- mice(data, m=10, maxit = 20,meth='pmm')
summary(imputed_Data)

#Check complete data using one impute set
data <- complete(imputed_Data,2)
summary(data)

#data=data[,-c(15,14,13,4,5)]
#building regression model

#Create Training and testing data set

set.seed(121)
train = sample(1:nrow(data),nrow(data)/0.8,replace = TRUE)
test = -train
training_data = data[train,]
testing_data = data[test,]
testing_power= data$POWER[test]

#create model
model<-lm(POWER~ .,data=training_data)
summary(model)

#predict values
new_predict = predict.lm(model,testing_data,type = 'response')


#Error estimation
rmse1=rmse(testing_power,new_predict)


# building model with cleaning it


#building regression model

#Create Training and testing data set
data <- complete(imputed_Data,2)
summary(data)
data=data[,-c(5,6,14,15,16)]

set.seed(121)
train = sample(1:nrow(data),nrow(data)/0.8,replace = TRUE)
test = -train
training_data = data[train,]
testing_data = data[test,]
testing_power= data$POWER[test]

#create model
model1<-lm(POWER~ .,data=training_data)
summary(model1)

#predict values
new_predict = predict.lm(model1,testing_data,type = 'response')


#Error estimation
rmse2=rmse(testing_power,new_predict)

#model with cleaning data for noramlisation
data <- complete(imputed_Data,2)
summary(data)

boxplot(data$AWND)

#awnd great than 12 to be zero
data$AWND[data$AWND>12]=0

boxplot(data$PRCP)
boxplot(data$SNOW)
boxplot(data$SNWD)
boxplot(data$POWER)




# seeing corelation between variables
#Check correlations before proceeding
corMat <- cor(data[,-1])
corMat
data=data[,-c(5,6,14,15,16)]

#FA without rotation (still remians orthogonal in nature)
n.factors <-3


#FA with orthogonal rotation- Varimax
fit <- factanal(data[,-1], 
                n.factors,              # number of factors to extract
                rotation="varimax", scores="regression") 
fit

#removinf wsf2,wsf5,wdf2,wdf5 and adding factor1,2
data=data[,-c(9,6,7,8)]
load=fit$loadings[,1:2]
plot(load,type="n") # set up plot 
text(load,labels=names(data),cex=.7)
fact=fit$scores[,1:2]
data=cbind.data.frame(data,fact)


#Create Training and testing data set

set.seed(121)
train = sample(1:nrow(data),nrow(data)/0.8,replace = TRUE)
test = -train
training_data = data[train,]
testing_data = data[test,]
testing_power= data$POWER[test]

#create model
model2<-lm(POWER~ .,data=training_data)
summary(model2)

#predict values
new_predict = predict.lm(model2,testing_data,type = 'response')


#Error estimation
rmse3=rmse(testing_power,new_predict)



