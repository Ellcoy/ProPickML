install.packages("xgboost")
library(xgboost)

##import data
#Datasets available in github
trainUrineReady<-read_csv("your/file/trainUrineReady.csv")
UrineDataReady<-read_csv("your/file/UrineDataReady.csv")
SemenDataReady<-read_csv("your/file/SemenDataReady.csv")
FullPatientDataReady<-read_csv("your/file/FullPatientDataReady.csv")
FullPlasmaReady<-read_csv("your/file/FullPlasmaDataReady.csv")
LongitudinalDataReady<-read_csv("your/file/LongitudinalDataReady.csv")


##Urine specific model
train_dataset = xgb.DMatrix(data = as.matrix(trainUrineReady[, -c(1,63)]), label = as.numeric(as.logical(trainUrineReady$class)))
test_dataset  = xgb.DMatrix(data = as.matrix(UrineDataReady [, colnames(train_dataset)]), label = as.numeric(as.logical(UrineDataReady$class)))
test_dataset2  = xgb.DMatrix(data = as.matrix(SemenDataReady [, colnames(train_dataset)]), label = as.numeric(as.logical(SemenDataReady$class)))
test_dataset3  = xgb.DMatrix(data = as.matrix(FullPatientDataReady [, colnames(train_dataset)]), label = as.numeric(as.logical(FullPatientDataReady$class)))


params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.2, gamma=0, max_depth=8, min_child_weight=3, subsample=1, colsample_bytree=1)
set.seed(123)
XGB <- xgb.train(params = params, data = train_dataset, watchlist <- list(train = train_dataset, test = test_dataset), eval_metric = "aucpr", print_every_n = 10, nrounds = 100)

mltools::mcc(preds = as.numeric(predict(XGB, train_dataset) >= 0.5), actuals = as.numeric(as.logical(trainUrineReady$class)))# MCC = 0.9975928
mltools::mcc(preds = as.numeric(predict(XGB, test_dataset) >= 0.5), actuals = as.numeric(as.logical(UrineDataReady$class)))# MCC = 0.8933487
mltools::mcc(preds = as.numeric(predict(XGB, test_dataset2) >= 0.5), actuals = as.numeric(as.logical(SemenDataReady$class)))# MCC = 0.1997602
mltools::mcc(preds = as.numeric(predict(XGB, test_dataset3) >= 0.5), actuals = as.numeric(as.logical(FullPatientDataReady$class)))# MCC =  0.82519 

##Export Predictions for Patient Data
IDs<- FullPatientDataReady$ID 
predictions<- as.numeric(predict(XGB, test_dataset3) >= 0.5)
actuals<-as.numeric(as.logical(FullPatientDataReady$class))
UrineModelPredictionsonPatientData<- cbind(IDs,predictions,actuals)
write_csv(as.data.frame(UrineModelPredictionsonPatientData),"your/file/UrineModelPredictionsonPatientData.csv") 


##Generalizable model 
BigComboReady<- rbind(FullPlasmaReady,trainUrineReady,SemenDataReady)

train_dataset = xgb.DMatrix(data = as.matrix(BigComboReady[, -c(1,63)]), label = as.numeric(as.logical(BigComboReady$class)))
test_dataset  = xgb.DMatrix(data = as.matrix(UrineDataReady [, colnames(train_dataset)]), label = as.numeric(as.logical(UrineDataReady$class)))
test_dataset2  = xgb.DMatrix(data = as.matrix(LongitudinalDataReady [, colnames(train_dataset)]), label = as.numeric(as.logical(LongitudinalDataReady$class)))
test_dataset3  = xgb.DMatrix(data = as.matrix(FullPatientDataReady [, colnames(train_dataset)]), label = as.numeric(as.logical(FullPatientDataReady$class)))
test_dataset4  = xgb.DMatrix(data = as.matrix(LongitudinalDataReady_col3 [, colnames(train_dataset)]), label = as.numeric(as.logical(LongitudinalDataReady_col3$class)))


params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.2, gamma=0, max_depth=8, min_child_weight=3, subsample=1, colsample_bytree=1)
set.seed(123)
XGB <- xgb.train(params = params, data = train_dataset, watchlist <- list(train = train_dataset, test = test_dataset), eval_metric = "aucpr", print_every_n = 10, nrounds = 100)

mltools::mcc(preds = as.numeric(predict(XGB, train_dataset) >= 0.5), actuals = as.numeric(as.logical(BigComboReady$class)))# MCC = 0.97
mltools::mcc(preds = as.numeric(predict(XGB, test_dataset) >= 0.5), actuals = as.numeric(as.logical(UrineDataReady$class)))# MCC = 0.89
mltools::mcc(preds = as.numeric(predict(XGB, test_dataset2) >= 0.5), actuals = as.numeric(as.logical(LongitudinalDataReady$class)))# MCC = 0.88
mltools::mcc(preds = as.numeric(predict(XGB, test_dataset3) >= 0.5), actuals = as.numeric(as.logical(FullPatientDataReady$class)))# MCC =  0.83
mltools::mcc(preds = as.numeric(predict(XGB, test_dataset4) >= 0.5), actuals = as.numeric(as.logical(LongitudinalDataReady_col3$class)))# MCC = 0.79

