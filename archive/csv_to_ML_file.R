##Change Path to folder to folder where you mzXML and csv files are 

path_to_Eco_folder <- "your_file_path"

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
  
  
  newdf<-read.csv(paste(gsub("\\..*","",filepath),"summary",".csv",sep = ""), header = TRUE, sep = ",")
  
  testSEQdf<-newdf[newdf$sequence==seq,c("sequence","rtimes","t1","t2","t3","t4","totIntensity")]
  
  
  centsmoothPeaksSEQtotIntensity<-splus2R::peaks(data.frame(testSEQdf$rtimes,testSEQdf$totIntensity),span = 7, endbehavior = 0)
  
  bob<-centsmoothPeaksSEQtotIntensity$testSEQdf.totIntensity
  testSEQdf$peaks<- bob
  peakList<-testSEQdf[testSEQdf$peaks=="TRUE",c("rtimes","totIntensity","t1","t2","t3","t4")]
  assign(paste("peakList",seq,sep = ""),peakList)
  
  
  
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
  
  y=get(paste0("peakList",seq))
  for (i in y$rtimes){
    var_time = get(paste0("variable_",i))
    for(x in 1:length(var_time)){
      wow = y$rtimes==i
      TT = testSEQdf$rtimes==var_time[x]
      y[[paste0("t1inpeak",x)]][wow]<-testSEQdf[TT,"t1"]
      y[[paste0("t2inpeak",x)]][wow]<-testSEQdf[TT,"t2"]
      y[[paste0("t3inpeak",x)]][wow]<-testSEQdf[TT,"t3"]
      y[[paste0("t4inpeak",x)]][wow]<-testSEQdf[TT,"t4"]
      y[[paste0("t1inpeakProportions",x)]][wow]<-testSEQdf[TT,"t1"]/testSEQdf[TT,"totIntensity"]*100
      y[[paste0("t2inpeakProportions",x)]][wow]<-testSEQdf[TT,"t2"]/testSEQdf[TT,"totIntensity"]*100
      y[[paste0("t3inpeakProportions",x)]][wow]<-testSEQdf[TT,"t3"]/testSEQdf[TT,"totIntensity"]*100
      y[[paste0("t4inpeakProportions",x)]][wow]<-testSEQdf[TT,"t4"]/testSEQdf[TT,"totIntensity"]*100
      y[[paste0("t1inpeakPercent",x)]][wow]<-testSEQdf[TT,"t1"]/testSEQdf[testSEQdf$rtimes==var_time[6],"totIntensity"]*100
      y[[paste0("t2inpeakPercent",x)]][wow]<-testSEQdf[TT,"t2"]/testSEQdf[testSEQdf$rtimes==var_time[6],"totIntensity"]*100
      y[[paste0("t3inpeakPercent",x)]][wow]<-testSEQdf[TT,"t3"]/testSEQdf[testSEQdf$rtimes==var_time[6],"totIntensity"]*100
      y[[paste0("t4inpeakPercent",x)]][wow]<-testSEQdf[TT,"t4"]/testSEQdf[testSEQdf$rtimes==var_time[6],"totIntensity"]*100
    }
  }
  
  y$standardDev<-sd(y$totIntensity)#standard deviation
  y$zScore<-scale(y$totIntensity)#zscore
  y$zscore1<-scale(y$t1)#zscore1
  y$zscore2<-scale(y$t2)#zscore2
  y$zscore3<-scale(y$t3)#zscore3
  y$zscore4<-scale(y$t4)#zscore4
  
  # write.csv(y,paste(gsub("\\..*","",filepath),"_",seq,".csv",sep = ""), row.names = F)
  write.csv(y,paste(gsub("\\..*","",filepath),"_withProportions","_",seq,".csv",sep = ""), row.names = F)
}


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



###Combine proportion files together

# Set the directory where your CSV files are located
setwd("your_path")

# Define the regular expression pattern for matching the desired files
pattern <- "RD152_231018-CV_U..._withProportions_[A-Z]+\\.csv"

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
# Set the directory where your CSV files are located
setwd("your_path")

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
# obs[!(obs %in% fulldata$filename)] #Pools may appear here as are not included in the analysis

class = c()
for(i in 1:nrow(fulldata)){
  echantillon = fulldata$filename[i]
  time = fulldata$rtimes[i]
  row_in_time_data = which(timedata$obs %in% echantillon)
  interval = 60*c(timedata$Min.Start.Time[row_in_time_data], timedata$Max.End.Time[row_in_time_data])
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



#If necessary combine files from multiple paths

BIG_file = do.call(rbind, list(read.csv("File1"),
                                read.csv("File2"),
                                read.csv("File3")))

colnames(BIG_file)
write.csv(BIG_file,"Big_File.csv",row.names = F)



#####Peptide columns 


library(tidyverse)

data<- read.csv("Big_File.csv")

data$Peptide = sapply(strsplit(data$filename, "_"), function(x) tail(x, 1))
data_peptides <- data %>% relocate(Peptide,.after = filename)
data_peptides<- data_peptides %>% mutate(ID = paste(filename, ID, sep = "_"), .before = 1)


#Drop non feature columns
Metadata_data_peptides<-select( data_peptides, c(ID,rtimes,totIntensity, t1, t2,t3,t4,class))

data_peptide_cols<-select(data_peptides, -c(X ,ID,filename,totIntensity, t1, t2,t3,t4,standardDev))

write.csv(data_peptide_cols,"Big_file_ready.csv")
write.csv(Metadata_data_peptides,"metadata.csv")
