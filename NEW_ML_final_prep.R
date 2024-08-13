###This code formats the final csv with all the information into the matrix for machine learning 


FullData<- read_csv("file/with/data/fulldatawithLabelsCorrectedProportions.csv")
FullData$class<-as.factor(FullData$class)
FullData$Peptide<- sub(".*_","", FullData$filename)


#filter out non-normalized columns
FullData<- FullData[c("ID","t1inpeak6","t2inpeak6","t3inpeak6","t4inpeak6", colnames(FullData)[grep("^t1in.*Proportions|^t2in.*Proportions|^t3in.*Proportions|^t4in.*Proportions|^t1in.*Percent|^t2in.*Percent|^t3in.*Percent|^t4in.*Percent", colnames(FullData), ignore.case = FALSE)], "zScore", "zscore1","zscore2","zscore3","zscore4","class")]
FullData.h2o<-as.h2o(FullData)



##Reduce from 10 to 7 
FullData.h2o<-FullData.h2o[-which(names(FullData.h2o) %in%c("t1inpeakProportions1","t2inpeakProportions1","t3inpeakProportions1","t4inpeakProportions1",
                                                                                    "t1inpeakPercent1","t2inpeakPercent1","t3inpeakPercent1","t4inpeakPercent1",
                                                                                    "t1inpeakProportions2","t2inpeakProportions2","t3inpeakProportions2","t4inpeakProportions2", 
                                                                                    "t1inpeakPercent2","t2inpeakPercent2","t3inpeakPercent2","t4inpeakPercent2",
                                                                                    "t1inpeakProportions10","t2inpeakProportions10","t3inpeakProportions10","t4inpeakProportions10",
                                                                                    "t1inpeakPercent10","t2inpeakPercent10","t3inpeakPercent10","t4inpeakPercent10",
                                                                                    "t1inpeakProportions11","t2inpeakProportions11","t3inpeakProportions11","t4inpeakProportions11", 
                                                                                    "t1inpeakPercent11","t2inpeakPercent11","t3inpeakPercent11","t4inpeakPercent11"))]



###Add multiplication column

FullData.h2o$moygeom<- 1/4*(log(ifelse(FullData.h2o$t1inpeak6 == 0,FullData.h2o$t1inpeak6+1, FullData.h2o$t1inpeak6))+log(ifelse(FullData.h2o$t2inpeak6 == 0, FullData.h2o$t2inpeak6+1, FullData.h2o$t2inpeak6))+log(ifelse(FullData.h2o$t3inpeak6 == 0, FullData.h2o$t3inpeak6+1, FullData.h2o$t3inpeak6))+log(ifelse(FullData.h2o$t4inpeak6 == 0, FullData.h2o$t4inpeak6+1, FullData.h2o$t4inpeak6)))

#get rid of max values
FullData.h2o<-FullData.h2o[-which(names(FullData.h2o) %in% c("t1inpeak6","t2inpeak6","t3inpeak6","t4inpeak6"))]

FullDataReady<- as.data.frame(FullData.h2o)

write_csv(FullDataReady,"FullDataReady.csv")