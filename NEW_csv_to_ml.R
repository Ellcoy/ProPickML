library(tidyverse)
##Change Path to folder to folder where you mzXML and csv files are 

path_to_Eco_folder <- "your/filepath/with/mzXMLs"

# List all files in the directory
all_files <- list.files(path_to_Eco_folder)

# Filter to include only .mzXML files
mzXML_files <- all_files[grep(".mzXML$", all_files)]

# Create a vector with the names of .mzXML files
allpaths <- file.path(path_to_Eco_folder, mzXML_files)

# Now 'allpaths' contains the full file paths of all .mzXML files in your Eco folder



###Function to add all columns for machine learning
AllColumns<-function(seq,filepath){
  library(splus2R)
  library(tidyverse) # god dammit always have TIDYVERSE!!!
  newdf<-read.csv(paste(gsub("\\..*","",filepath),"summary",".csv",sep = ""), header = TRUE, sep = ",")
  
  ##we want to get all the transition in the peaks and then trim down to the most important 4
  transition_columns = intersect(paste0("t",1:100), colnames(newdf))
  message(paste0("Number of transition detected: ", length(transition_columns)))
  
  testSEQdf<-newdf[newdf$sequence==seq,c("sequence","rtimes",transition_columns,"totIntensity")]
  # testSEQdf<-newdf[newdf$sequence==seq,c("sequence","rtimes","t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12","t13","t14","t15","t16","t17","t18","t19","totIntensity")]
  centsmoothPeaksSEQtotIntensity<-splus2R::peaks(data.frame(testSEQdf$rtimes,testSEQdf$totIntensity),span = 7, endbehavior = 0)
  # Bob is dead to me
  bob<-centsmoothPeaksSEQtotIntensity$testSEQdf.totIntensity
  testSEQdf$peaks<- bob
  peakList<-testSEQdf[testSEQdf$peaks=="TRUE",c("rtimes","totIntensity",transition_columns)]
  # peakList<-testSEQdf[testSEQdf$peaks=="TRUE",c("rtimes","totIntensity","t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12","t13","t14","t15","t16","t17","t18","t19")]
  assign(paste("peakList",seq,sep = ""),peakList)
  
  # Add NAs if necessary
  for (t in get(paste0("peakList",seq))$rtimes){
    index = match(t, testSEQdf$rtimes)
    if (index-5 > 0){
      test<-testSEQdf$rtimes[(index-5):(index+5)]
      print(test)
      assign( paste0("variable_", t),test)
    } 
    else if(index-4 > 0){
      test<-c(NA,testSEQdf$rtimes[(index-4):(index+5)])
      assign( paste0("variable_", t),test)
    }
    else if(index-3 > 0){
      test<-c(NA,NA,testSEQdf$rtimes[(index-3):(index+5)])
      assign( paste0("variable_", t),test)
    }
    else if(index-2 > 0){
      test<-c(NA,NA,NA,testSEQdf$rtimes[(index-2):(index+5)])
      print(test)
      assign( paste0("variable_", t),test)
    }
    else if(index-1 > 0){
      test<-c(NA,NA,NA,NA,testSEQdf$rtimes[(index-1):(index+5)])
      print(test)
      assign( paste0("variable_", t),test)
    }
    else{
      test<-c(NA,NA,NA,NA,NA,testSEQdf$rtimes[(index):(index+5)])
      print(test)
      assign( paste0("variable_", t),test)
    }
  }
  
  # Extract information at each timepoint in potential peak
  y=get(paste0("peakList",seq))
  for (i in y$rtimes){
    var_time = get(paste0("variable_",i))
    for(x in 1:length(var_time)){
      wow = y$rtimes==i
      TT = testSEQdf$rtimes==var_time[x]
      for(trans in transition_columns){
        y[[paste0(trans,"inpeak",x)]][wow]<-testSEQdf[TT,trans]
        y[[paste0(trans, "inpeakProportions",x)]][wow]<-testSEQdf[TT,trans]/testSEQdf[TT,"totIntensity"]*100
        y[[paste0(trans, "inpeakPercent",x)]][wow]<-testSEQdf[TT,trans]/testSEQdf[testSEQdf$rtimes==var_time[6],"totIntensity"]*100
      }
    }
  }
  
  # Add additional features
  y$standardDev<-sd(y$totIntensity) #standard deviation
  y$zScore<-as.vector(scale(y$totIntensity)) #zscore
  for(trans in transition_columns){
    y[[paste0("zscore", str_remove(trans, "t"))]]<-as.vector(scale(y[[trans]]))
  }
  
  # write.csv(y,paste(gsub("\\..*","",filepath),"_",seq,".csv",sep = ""), row.names = F)
  write.csv(y,paste(gsub("\\..*","",filepath),"_withProportions","_",seq,".csv",sep = ""), row.names = F)
}



##To run parallell
library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-3) #not to overload your computer
registerDoParallel(cl)

for (filepath in allpaths){
  newdf<-read.csv(paste(gsub("\\..*","",filepath),"summary",".csv",sep = ""), header = TRUE, sep = ",")
  sequences<-unique(newdf$sequence)
  foreach(seq=sequences, .combine=cbind) %dopar% {
    AllColumns(seq,filepath)
  }
}

#stop cluster
stopCluster(cl)



# for troubleshooting
# filepath = allpaths[1]
# seq = newdf$sequence[40]




###Combine proportion files together

# Set the directory where your CSV files are located
setwd("file/with/data")

# Define the regular expression pattern for matching the desired files
pattern <- "*_withProportions_[A-Z].*\\.csv"

# List all CSV files that match the pattern
csv_files <- list.files(pattern = pattern)

# Initialize an empty list to store dataframes
dataframes <- list()

# Iterate through the matching CSV files, read them, and add a "filename" column
for (file in csv_files) {
  df <- read.csv(file)
  df$filename <- file
  dataframes[[file]] <- df
}

# Combine all dataframes into a single dataframe
combined_dataframe <- do.call(rbind, dataframes)

# Remove the ".csv" extension from the "filename" column
combined_dataframe$filename <- sub("\\.csv$", "", combined_dataframe$filename)

# Reorder the columns with "filename" as the first column
combined_dataframe <- combined_dataframe[, c("filename", names(combined_dataframe)[!names(combined_dataframe) %in% "filename"])]

# Write the combined dataframe to a CSV file
write.csv(combined_dataframe, file = "AllPeptidesAllColumnsProportions.csv", row.names = FALSE)




####Labelling


library(tidyverse)
# # Set the directory where your CSV files are located
setwd("file/with/data/")

#import data (skip this and use commented line if pervious step is still in evrironment to save time)
fulldata<-read.csv("AllPeptidesAllColumnsProportions.csv", header = TRUE)
# fulldata<-combined_dataframe

timedata<-read.csv("Peak Boundaries.csv", header = TRUE)

#handle NAs
timedata[timedata == "#N/A"] = -1
timedata$Min.Start.Time = as.numeric(timedata$Min.Start.Time)
timedata$Max.End.Time = as.numeric(timedata$Max.End.Time)

#Function to check if Rtime is in intreval of start and end time
in.interval = function(value, interval) {
  if(value >= interval[1] & value <= interval[2]) {
    return(T)
  }
  else {
    return(F)
  }
}

#Get correspondance between fulldata$filename and timedata$File.Name and timedata$Peptide.Modified.Sequence
obs = paste0(str_remove(timedata$File.Name, ".raw"),"_withProportions","_", timedata$Peptide.Modified.Sequence)
timedata$obs = obs
obs[!(obs %in% fulldata$filename)] #Pools may appear here as are not included in the analysis


class = c()
for(i in 1:nrow(fulldata)){
  echantillon = fulldata$filename[i]
  time = fulldata$rtimes[i]
  row_in_time_data = which(timedata$obs %in% echantillon)
  interval = unique(60*c(timedata$Min.Start.Time[row_in_time_data], timedata$Max.End.Time[row_in_time_data]))
  class = c(class, in.interval(value = time, interval = interval))
}

fulldata$class = class

#turn all nas into zeroes
fulldata[is.na(fulldata)] = 0

#write new labelled csv Add an "ID" column with sequential numbers
fulldata$ID <- 1:nrow(fulldata)

#put ID first
fulldata <- fulldata[, c("ID", setdiff(names(fulldata), "ID"))]

# Write the data frame to a CSV file
write.csv(fulldata,"fulldatawithLabelsCorrectedProportions.csv", row.names = FALSE)