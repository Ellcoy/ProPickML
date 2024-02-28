###Get data
setwd("~/finalh2o/")
library(h2o)
library(tidyverse)
h2o.init(nthreads = 50)

#Import and format testing data (import ML ready file from previous code)

testingData<- read_csv("Big_file.csv")
testingData$class<-as.factor(testingData$class)
testingData$Peptide<-as.factor(testingData$Peptide)

#filter out non-normalized columns except the max
testingData<-(testingData[c("ID", "Peptide","rtimes","t1inpeak6","t2inpeak6","t3inpeak6","t4inpeak6", colnames(testingData)[grep("Percent|Proportions", colnames(testingData), ignore.case = TRUE)], "zScore", "zscore1", "zscore2", "zscore3", "zscore4", "class")])
testingData.h2o<-as.h2o(testingData)

##Reduce from 10 to 7 

testingData.h2o<-testingData.h2o[-which(names(testingData.h2o) %in% c("t1inpeakProportions1","t2inpeakProportions1","t3inpeakProportions1","t4inpeakProportions1",
                                                                      "t1inpeakPercent1","t2inpeakPercent1","t3inpeakPercent1","t4inpeakPercent1",
                                                                      "t1inpeakProportions2","t2inpeakProportions2","t3inpeakProportions2","t4inpeakProportions2", 
                                                                      "t1inpeakPercent2","t2inpeakPercent2","t3inpeakPercent2","t4inpeakPercent2",
                                                                      "t1inpeakProportions10","t2inpeakProportions10","t3inpeakProportions10","t4inpeakProportions10",
                                                                      "t1inpeakPercent10","t2inpeakPercent10","t3inpeakPercent10","t4inpeakPercent10",
                                                                      "t1inpeakProportions11","t2inpeakProportions11","t3inpeakProportions11","t4inpeakProportions11", 
                                                                      "t1inpeakPercent11","t2inpeakPercent11","t3inpeakPercent11","t4inpeakPercent11"))]

###Add multiplication column

testingData.h2o$moygeom<- 1/4*(log(ifelse(testingData.h2o$t1inpeak6 == 0, testingData.h2o$t1inpeak6+1, testingData.h2o$t1inpeak6))+log(ifelse(testingData.h2o$t2inpeak6 == 0, testingData.h2o$t2inpeak6+1, testingData.h2o$t2inpeak6))+log(ifelse(testingData.h2o$t3inpeak6 == 0, testingData.h2o$t3inpeak6+1, testingData.h2o$t3inpeak6))+log(ifelse(testingData.h2o$t4inpeak6 == 0, testingData.h2o$t4inpeak6+1, testingData.h2o$t4inpeak6)))
trainingData.h2o$moygeom<- 1/4*(log(ifelse(trainingData.h2o$t1inpeak6 == 0, trainingData.h2o$t1inpeak6+1, trainingData.h2o$t1inpeak6))+log(ifelse(trainingData.h2o$t2inpeak6 == 0, trainingData.h2o$t2inpeak6+1, trainingData.h2o$t2inpeak6))+log(ifelse(trainingData.h2o$t3inpeak6 == 0, trainingData.h2o$t3inpeak6+1, trainingData.h2o$t3inpeak6))+log(ifelse(trainingData.h2o$t4inpeak6 == 0, trainingData.h2o$t4inpeak6+1, trainingData.h2o$t4inpeak6)))

#get rid of max values
trainingData.h2o<-trainingData.h2o[-which(names(trainingData.h2o) %in%c("t1inpeak6","t2inpeak6","t3inpeak6","t4inpeak6"))]
testingData.h2o<-testingData.h2o[-which(names(testingData.h2o) %in% c("t1inpeak6","t2inpeak6","t3inpeak6","t4inpeak6"))]


##Apply Trained model 
model<- h2o.upload_model("DRF_1_AutoML_1_20240216_115046")
predictions <- h2o.predict(model,testingData.h2o)
write_csv(as.data.frame(predictions),"predictions.csv")

