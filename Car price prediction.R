install.packages("reader")
install.packages("DataExplorer")
install.packages("GGally")
install.packages("SmartEDA")
install.packages("stringr")


library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(caretEnsemble)
library(SmartEDA)
library(DataExplorer)
library(GGally)
library(reader)
library(doParallel)
library(stringr)


car_predict=read.csv("CarPrice_prediction.csv")

str(car_predict)

head(car_predict)

# Data cleaning

sum(is.na(car_predict))

car_predict=car_predict[,-c(1,2)]

names(car_predict) <- tolower(names(car_predict))

unique(car_predict$carname)

car_predict$carname <- sapply(strsplit(car_predict$carname, " "), `[`, 1)

car_predict$carname <- str_replace(car_predict$carname, "maxda", "mazda")
car_predict$carname <- str_replace(car_predict$carname, "toyouta", "toyota")
car_predict$carname <- str_replace(car_predict$carname, "vokswagen", "volkswagen")
car_predict$carname <- str_replace(car_predict$carname, "Nissan", "nissan")

#Data visualization

ggplot(car_predict,aes(carname,price,fill = enginelocation))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))

ggplot(car_predict, aes(carname, price,fill= carbody)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(car_predict,aes(citympg,price,col=carbody))+geom_point()

ggplot(car_predict,aes(fueltype,citympg,fill =fuelsystem ))+geom_boxplot()+
  facet_grid(~cylindernumber)

ggplot(car_predict,aes(fueltype,highwaympg,fill =fuelsystem ))+geom_boxplot()+
  facet_grid(~cylindernumber)

ggplot(car_predict,aes(fueltype,price,fill =fuelsystem ))+geom_boxplot()
  
ggplot(car_predict,aes(cylindernumber,price,fill =enginetype))+geom_boxplot()

ggplot(car_predict,aes(enginelocation,price,fill=carbody))+geom_boxplot()

ggplot(car_predict,aes(horsepower,price,col=fuelsystem))+geom_point()

ggplot(car_predict,aes(factor(carbody),price,fill=drivewheel))+geom_boxplot()

# find numeric values
nums = unlist(lapply(car_predict, is.numeric))

# save numeric variables for later
car_predictnums = car_predict[,nums]

# show numeric variables
head(car_predictnums)

car_predictnums=car_predictnums %>% select(-c("price"))
str(car_predictnums)

# Removing of outliers
boxplot(car_predictnums)

boxplot(car_predictnums$enginesize,col = "red")
enginesizeoutliers = which(car_predictnums$enginesize > 200)
hist(car_predictnums$enginesize,col = "red")
car_predictnums$enginesize = log(car_predictnums$enginesize)

boxplot(car_predictnums$horsepower,col='red')
horsepoweroutliers = which(car_predictnums$horsepower>180)
hist(car_predictnums$horsepower,col = 'red')
car_predictnums$horsepower=log(car_predictnums$horsepower)


boxplot(car_predictnums$compressionratio,col = "green")
compressionratiooutliers= which(car_predictnums$compressionratio > 3)
car_predictnums[compressionratiooutliers,"compressionratio"]
car_predictnums = car_predictnums[-compressionratiooutliers,]
hist(car_predictnums$compressionratio,col = 'red')
car_predictnums$compressionratio=log(car_predictnums$compressionratio)

boxplot(car_predictnums$carlength,col = "green")
hist(car_predictnums$carlength,col = 'red')
car_predictnums$carlength=log(car_predictnums$carlength)

#Finding Correlated variables and doing PCA for reducing multicolinearity
cor(car_predictnums)
corrplot(cor(car_predictnums))

corr.matrix=cor(car_predictnums)
car_predict.highlyCorrelated = findCorrelation(corr.matrix, cutoff=0.7)
colnames(car_predictnums[,car_predict.highlyCorrelated])

car_predict.pca=prcomp(car_predictnums,center = TRUE,scale = TRUE,
             retx = T)

summary(car_predict.pca)

dim(car_predict.pca$x)

biplot(car_predict.pca,scale = 0)

car_predict.pca$rotation[1:5,1:4]

# Compute standard deviation
car_predict.pca$sdev

# Compute variance
car_predict.pca.var = car_predict.pca$sdev ^ 2
car_predict.pca.var

# To compute the proportion of variance explained by each component, we simply divide
car_predict.prop = car_predict.pca.var / sum(car_predict.pca.var)
car_predict.prop

# Plot variance explained for each principal component
plot(car_predict.prop, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot")

# Plot the cumulative proportion of variance explained
plot(cumsum(car_predict.prop),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Find Top n principal component
# which will atleast cover 90 % variance of dimension
which(cumsum(car_predict.prop) >= 0.9)[1]

# Changing categorical varibles to factors
car_predict$fueltype=as.factor(car_predict$fueltype)
car_predict$aspiration=as.factor(car_predict$aspiration)
car_predict$doornumber=as.factor(car_predict$doornumber)
car_predict$carbody=as.factor(car_predict$carbody)
car_predict$drivewheel=as.factor(car_predict$drivewheel)
car_predict$enginelocation=as.factor(car_predict$enginelocation)
car_predict$enginetype=as.factor(car_predict$enginetype)
car_predict$cylindernumber=as.factor(car_predict$cylindernumber)
car_predict$fuelsystem=as.factor(car_predict$fuelsystem)

str(car_predict)

# select factor variables to convert, but leave Attrition out
varstod = car_predict[,sapply(car_predict, is.factor)]
head(varstod)

# Create dummy variables with caret
dummies = dummyVars( ~ ., data = varstod)
ddummy = predict(dummies, newdata = varstod)

colnames(ddummy)
view(ddummy)
car_predict_clean = data.frame(ddummy, price = car_predict$price,car_predict.pca$x[,1:6])

str(car_predict_clean)

# remove near zero variables (except for attr)
rem.cols = nearZeroVar(car_predict_clean, names = TRUE)
rem.cols
# Get all column names 
allcols = names(car_predict_clean)
# Remove from data
car_predict_clean.final= car_predict_clean[ , setdiff(allcols, rem.cols)]

str(car_predict_clean.final)

##plot_bar(car_predict_clean.final, by="price")

plot_qq(car_predict_clean.final)

head(car_predict_clean.final)

str(car_predict_clean.final)

view(car_predict_clean.final)

# creating sample Linear model

Linear_model=lm(price~.,data=car_predict_clean.final)
summary(Linear_model)

anova(Linear_model)

pre=predict(Linear_model,car_predict_clean.final)

boxplot(car_predict$price)

err=sqrt(mean((car_predict_clean.final$price-pre)^2))
err

# Splitting the dataset to train and test

carsplit=createDataPartition(car_predict_clean.final$price,
                             p=0.75,
                             list = FALSE)

dtrain=car_predict_clean.final[carsplit,]
dtest=car_predict_clean.final[-carsplit,]

str(car_predict_clean.final)

x=car_predict_clean.final[,-20]
y=car_predict_clean.final[, 20]

xtrain=x[carsplit,]
str(xtrain)
xtest=x[-carsplit,]
str(xtest)

ytrain=y[carsplit]
str(ytrain)
ytest=y[-carsplit]
str(ytest)

#giving the traincontrol parametes
set.seed(123)
mycontrol=trainControl(method = "cv",
                       n=3,
                       savePredictions = TRUE,
                       allowParallel = TRUE)

# Creating the various models using caret package
carmodel=caretList(xtrain,ytrain,
                   trControl = mycontrol,
                   methodList = c("lm","rf","svmRadial","xgbTree","xgbLinear"),
                   tuneList = NULL,
                   continue_on_fail = FALSE,
                   preProcess=c("center","scale"))

carmodel$lm

carmodel$rf

options(digits = 3)
carmodelresults = data.frame(
  LM = min(carmodel$lm$results$RMSE),
  SVM = min(carmodel$svmRadial$results$RMSE),
  RF = min(carmodel$rf$results$RMSE),
  XGBT = min(carmodel$xgbTree$results$RMSE),
  XGBL = min(carmodel$xgbLinear$results$RMSE)
)

print(carmodelresults)

resamples = resamples(carmodel)
dotplot(resamples,metric="RMSE")

modelCor(resamples)

set.seed(123)
ensemble = caretEnsemble(carmodel,
                          metric="RMSE",
                          trControl=mycontrol)
summary(ensemble)
plot(ensemble)


predlm = predict.train(carmodel$lm, newdata = xtest)
predsvm = predict.train(carmodel$svmRadial, newdata = xtest)
predrf = predict.train(carmodel$rf, newdata = xtest)
predxgbT = predict.train(carmodel$xgbTree, newdata = xtest)
predxgbL = predict.train(carmodel$xgbLinear, newdata = xtest)
predictens1 = predict(ensemble, newdata = xtest)

predRMSE = data.frame(ensemble = RMSE(predictens1, ytest),
                      SVM = RMSE(predsvm, ytest),
                      RF = RMSE(predrf, ytest),
                      XGBT = RMSE(predxgbT, ytest),
                      XGBL = RMSE(predxgbL, ytest))
print(predRMSE)

set.seed(123)
xgbTreemodel1 = train(xtrain,
                     ytrain,
                     trControl = mycontrol,
                     method="xgbLinear",
                     metric="RMSE",
                     preProcess = c("center","scale"))

xgbTreemodel1= train(xtrain,
                      ytrain,
                      trControl = mycontrol,
                      method="xgbLinear",
                      metric="RMSE",
                      preProcess = c("center","scale"),
                      importance=TRUE)

summary(carmodel)

plot(varImp(carmodel))

  