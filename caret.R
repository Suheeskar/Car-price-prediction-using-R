library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(caretEnsemble)
library(doParallel)

setwd("D:/DATA ANALYTICS/BY DATASETS/CONCRETE")
c=read.csv("concrete.csv")
str(c)
summary(c)
head(c)

#correlation and plot
corrplot(cor(c))
cor(c)
#renaming
c$strength=c$concrete_compressive_strength
str(c)
c=c[,-9]

#data visualization
boxplot(c[-9],col="red")
boxplot(c$age, col ="red")

#checking outliers
ageoutliers = which(c$age > 100)
c[ageoutliers,"age"]

#simple model
l1=lm(strength~.,c)
summary(l1)


hist(c$superplastic)
plot(c$cement,c$strength)

hist(c$age)
hist(c$superplastic)
hist(log(c$age),col="yellow")
hist(log(c$superplastic),col="yellow")

hist(c$fly_ash)
plot(c$fly_ash,c$strength)
c=c[-3]
summary(c)
c$age = log(c$age)
c$superplastic = log(c$superplastic)
c$superplastic = ifelse(c$superplastic == -Inf, 0,  
                                c$superplastic)

#Data Split
ind = createDataPartition(c$strength, 
                                  p = 0.75,                         
                                list = FALSE)
dim(c)

ind
ctrain=c[ind,]
ctest=c[-ind,]

X = c[, -8]
y = c[, 8]

Xtrain = X[ind, ]
str(Xtrain)
Xtest = X[-ind, ]
str(Xtest)
ytrain = y[ind]
str(ytrain)
ytest = y[-ind]
str(ytest)

#set up train control
set.seed(123)
mycontrol=trainControl(method="cv",
                       number=3,
                       savePredictions = TRUE,
                       allowParallel = TRUE)

#set.seed - for getting the same data that we are taken during the model
#allowparallel - for enabling all machine learning process

#Model Building
set.seed(123)
modellist=caretList(Xtrain,ytrain,trControl = mycontrol,
                    methodList=c("lm","svmRadial","rf",
                                 "xgbTree","xgbLinear"),
                    tuneList=NULL,
                    continue_on_fail=FALSE,
                    preProcess=c("center","scale"))

modellist$lm                    
modellist$svmRadial                    

options(digits = 3)
modelresults = data.frame(
  LM = min(modellist$lm$results$RMSE),
  SVM = min(modellist$svmRadial$results$RMSE),
  RF = min(modellist$rf$results$RMSE),
  XGBT = min(modellist$xgbTree$results$RMSE),
  XGBL = min(modellist$xgbLinear$results$RMSE)
)

print(modelresults)

resamples = resamples(modellist)
dotplot(resamples,metric="rmse")

modelCor(resamples)

set.seed(123)
ensemble1 = caretEnsemble(modellist,
                           metric="RMSE",
                           trControl=mycontrol)
summary(ensemble1)
plot(ensemble1)

predlm = predict.train(modellist$lm, newdata = Xtest)
predsvm = predict.train(modellist$svmRadial, newdata = Xtest)
predrf = predict.train(modellist$rf, newdata = Xtest)
predxgbT = predict.train(modellist$xgbTree, newdata = Xtest)
predxgbL = predict.train(modellist$xgbLinear, newdata = Xtest)
predictens1 = predict(ensemble1, newdata = Xtest)

predRMSE = data.frame(ensemble1 = RMSE(predictens1, ytest),
                        LM = RMSE(predlm, ytest),
                        SVM = RMSE(predsvm, ytest),
                        RF = RMSE(predrf, ytest),
                        XGBT = RMSE(predxgbT, ytest),
                        XGBL = RMSE(predxgbL, ytest))
print(predRMSE)

set.seed(123)
xgbTreemodel = train(Xtrain,
                       ytrain,
                       trControl = mycontrol,
                       method="xgbLinear",
                       metric="RMSE",
                       preProcess = c("center","scale"),
                       importance=TRUE)
plot(varImp(xgbTreemodel))
plot(varImp(l1))


==========================================
  
ExpData(data=car_predict_clean.final, type=1)

ExpCatStat(car_predict_clean.final,Target="price", plot=TRUE)

#to revert back log (exp) is used
car_predict$price=exp(car_predict$price)

#to lowcase the values ina column
RecordsWithIssues$OTHERCOUNTRY <- tolower(RecordsWithIssues$OTHERCOUNTRY)

#to rename the multiple column name
my_dataframe <- my_dataframe %>% 
  rename("pages" = "c2",
         "price" = "c5")
