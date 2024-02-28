library(tidyverse)

# Replace 'path_to_Eco_folder' with the path to your folder containing mzXML files

path_to_Eco_folder <- "C:/Users/Ellie/Documents/THESIS/PapierClarisse/FilesFromServer/ECO"

# List all files in the directory
all_files <- list.files(path_to_Eco_folder)

# Filter to include only .mzXML files
mzXML_files <- all_files[grep(".mzXML$", all_files)]

# Create a vector with the names of .mzXML files
allpaths <- file.path(path_to_Eco_folder, mzXML_files)

# Now 'allpaths' contains the full file paths of all .mzXML files in your folder



###main analysis function
main<-function(filepath){
  library(MSnbase)
  rawdata<-readMSData(filepath)
  lim<-length(spectra(rawdata))
  range<-1:lim
  intensities<-c()
  mzs<-c()
  rtimes<-c()
  precursormzs<-c()
  
  for (i in range){
    intensity<-intensity(rawdata[[i]])
    mz<-mz(rawdata[[i]])
    rtime<-rtime(rawdata[[i]])
    precmz<-precursorMz(rawdata[[i]])
    intensities<-append(intensities,intensity)
    mzs<-append(mzs,mz)
    rtimes<-append(rtimes,rep(rtime,4))
    precursormzs<-append(precursormzs,rep(precmz,4))
  }
  
  rawdatadf<-data.frame(precursormzs,rtimes,intensities,mzs)
  write.csv(rawdatadf,paste(gsub("\\..*","",filepath),".csv",sep = ""), row.names = TRUE)
  
  seqdata <- read.csv("seqdata.csv", header = FALSE, sep = ",")
  rawdatadf$sequence <- 0
  rawdatadf$ion<- 0
  for (j in 1:nrow(rawdatadf)){
    for(i in 1:nrow(seqdata)) {
      if(abs(seqdata$V1[i]-rawdatadf$precursormzs[j])<0.01 && abs(seqdata$V2[i]-rawdatadf$mzs[j])<0.01){
        rawdatadf$sequence[j]<-seqdata$V4[i]
        rawdatadf$ion[j]<-seqdata$V6[i]
      }
    }
  }
  
  write.csv(rawdatadf,paste(gsub("\\..*","",filepath),"seqs",".csv",sep = ""), row.names = TRUE)
  
  newdf <- rawdatadf[c(TRUE,rep(FALSE,3)),c(1,2,5) ]
  newdf$t1 <- rawdatadf[c(TRUE,rep(FALSE,3)),c(3) ]
  newdf$t2 <- rawdatadf[c(FALSE,TRUE,rep(FALSE,2)),c(3) ]
  newdf$t3 <- rawdatadf[c(FALSE,FALSE,TRUE,FALSE),c(3) ]
  newdf$t4 <- rawdatadf[c(FALSE,FALSE,FALSE,TRUE),c(3) ]
  newdf$totIntensity<-rowSums(newdf[,c(4,5,6,7)])
  
  write.csv(newdf,paste(gsub("\\..*","",filepath),"summary",".csv",sep = ""), row.names = TRUE)
}

##On windows
library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)

foreach(i=allpaths, .combine=cbind) %dopar% {
  
  main(i)
  
}
#stop cluster
stopCluster(cl)


##On Linux Server

library(parallel)

# Use mclapply for parallel processing

result <- mclapply(allpaths, main, mc.cores = parallel::detectCores() - 2)

# You can access the results in the 'result' variable


# ####Should you need too troubleshoot:
# #This takes just one file and lets you run each part of main one by one
# filepath = allpaths[1]