library(tidyverse)

# Replace 'path_to_Eco_folder' with the path to your folder containing mzXML files

path_to_Eco_folder <- "your/filepath/with/mzXMLs"

# List all files in the directory
all_files <- list.files(path_to_Eco_folder)

# Filter to include only .mzXML files
mzXML_files <- all_files[grep(".mzXML$", all_files)]

# Create a vector with the names of .mzXML files
allpaths <- file.path(path_to_Eco_folder, mzXML_files)

# Now 'allpaths' contains the full file paths of all .mzXML files in your folder


# This function, `main`, processes mass spectrometry data from an mzXML file and matches it with corresponding sequence data.
# It reads the mzXML file to extract spectra information, including intensities, m/z values, retention times, and precursor m/z values.
# These data points are compiled into a dataframe and saved as a CSV file. The function then matches this raw data with provided 
# sequence data (`seqdata`) based on m/z values, adding sequence and ion information to the dataframe. If the sequence data contains 
# weight information, it is also included. The enriched dataframe is saved as another CSV file. Additionally, a summary dataframe is 
# created, which consolidates intensities into total intensity values and includes relevant metadata, and this summary is saved as a 
# final CSV file. This function simplifies and automates the data extraction, matching, and summary process for mass spectrometry analysis.

###main analysis function
main <- function(filepath, seqdata) {
  library(MSnbase)
  library(mzR)
  library(tidyverse)
  
  # Read raw data
  rawdata <- readMSData(filepath)
  lim <- length(spectra(rawdata))
  range <- 1:lim
  intensities <- c()
  mzs <- c()
  rtimes <- c()
  precursormzs <- c()
  
  # Extract intensity, mz, retention time, and precursor mz from each spectrum
  for (i in range) {
    intensity <- intensity(rawdata[[i]])
    mz <- mz(rawdata[[i]])
    rtime <- rtime(rawdata[[i]])
    precmz <- precursorMz(rawdata[[i]])
    x <- length(intensity)
    intensities <- append(intensities, intensity)
    mzs <- append(mzs, mz)
    rtimes <- append(rtimes, rep(rtime, x))
    precursormzs <- append(precursormzs, rep(precmz, x))
  }
  
  # Create a dataframe and write to CSV
  rawdatadf <- data.frame(precursormzs, rtimes, intensities, mzs)
  
  # Initialize new columns
  rawdatadf$sequence <- 'placeholder'
  rawdatadf$ion <- 'placeholder'
  if(any(names(seqdata) == 'V8')) {
    rawdatadf$weight <- 'placeholder'
  }
  
  # Match sequence data to raw data
  for (j in 1:nrow(rawdatadf)) {
    for (i in 1:nrow(seqdata)) {
      if (abs(seqdata$V1[i] - rawdatadf$precursormzs[j]) < 0.001 && abs(seqdata$V2[i] - rawdatadf$mzs[j]) < 0.001) {
        rawdatadf$sequence[j] <- seqdata$V4[i]
        rawdatadf$ion[j] <- seqdata$V6[i]
        if (any(names(seqdata) == 'V8')) {
          rawdatadf$weight[j] <- seqdata$V8[i]
        }
      }
    }
  }

  # Write matched data to CSV
  write.csv(rawdatadf, paste(gsub("\\..*","",filepath),"seqs",".csv",sep = ""), row.names = TRUE)
  rawdatadf_cleaned = rawdatadf %>% 
    filter(sequence != "placeholder" | ion != "placeholder" ) %>% 
    mutate(sequence_count = cumsum(c(TRUE, head(sequence, -1) != tail(sequence, -1)))) %>% # find groups of sequence
    group_by(sequence_count) %>% #
    mutate(transition = row_number(mzs)) %>% # give transitions a number ordered by mzs
    dplyr::select(- mzs, -ion) %>%
    dplyr::mutate(transition = paste0("t", transition)) %>% 
    mutate(rtimes = min(rtimes)) %>%  # only take the minimum rtimes in groups.
    pivot_wider(names_from = transition, values_from = intensities, values_fill = NA) %>% 
    ungroup %>% 
    mutate(totIntensity = rowSums(select(., starts_with("t")), na.rm=TRUE), .before = t1)
  
  # Write summary data to CSV
  write.csv(rawdatadf_cleaned, paste(gsub("\\..*","",filepath),"summary",".csv",sep = ""), row.names = TRUE)
}


##On windows
library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)
seqdata <- read.csv("path/to/data/seqdata.csv", header = FALSE, sep = ",")
foreach(i=allpaths, .combine=cbind) %dopar% {
  
  main(i,seqdata)
  
}
#stop cluster
stopCluster(cl)


## On Linux Server

library(parallel)

# Read the seqdata file
seqdata <- read.csv("path/to/data/seqdata.csv", header = FALSE, sep = ",")

# Use mclapply for parallel processing and pass seqdata to the main function
result <- mclapply(allpaths, function(i) main(i, seqdata), mc.cores = parallel::detectCores() - 2)

# You can access the results in the 'result' variable


# ####Should you need too troubleshoot:
# #This takes just one file and lets you run each part of main one by one
# filepath = allpaths[1]





