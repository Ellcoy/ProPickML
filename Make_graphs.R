##Make graphs
path_to_Eco_folder <- "path to file with CSV and mzXML"

# List all files in the directory
all_files <- list.files(path_to_Eco_folder)

# Filter to include only .mzXML files
mzXML_files <- all_files[grep(".mzXML$", all_files)]

# Create a vector with the names of .mzXML files
allpaths <- file.path(path_to_Eco_folder, mzXML_files)


create_graphs <- function(filepath, save_dir) {
  # libraries
  require(spatialEco)
  require(ggplot2)
  
  # Read the data
  newdf <- read.csv(paste(gsub("\\..*", "", filepath), "summary.csv", sep = ""), header = TRUE, sep = ",")
  
  sequences <- unique(newdf$sequence)
  
  # Create a new "graphs" directory if it doesn't exist
  graphs_dir <- file.path(save_dir, "graphs")
  if (!dir.exists(graphs_dir)) {
    dir.create(graphs_dir)
  }
  
  # Define a custom color palette for better colors
  custom_colors <- c("total" = "#E41A1C", "t4" = "#377EB8", "t3" = "#4DAF4A", "t2" = "#984EA3", "t1" = "#FF7F00")
  
  # Create a ggplot theme with improved aesthetics
  my_theme <- theme_minimal() +
    theme(
      panel.border = element_blank(),
      text = element_text(size = 12),
      legend.position = "top",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10)
    )
  
  for (seq in sequences) {
    testSEQdf <- newdf[newdf$sequence == seq, c("sequence", "rtimes", "t1", "t2", "t3", "t4", "totIntensity")]
    
    # # Create a ggplot object with pmax transformation to stop smoothing at zero
    # p <- ggplot(data = testSEQdf, aes(x = rtimes)) +
    #   geom_line(aes(y = pmax(sg.smooth(totIntensity, l = 21), 0), color = "total"), size = 1) +
    #   geom_line(aes(y = pmax(sg.smooth(t4, l = 21),0), color = "t4"), size = 1) +
    #   geom_line(aes(y = pmax(sg.smooth(t3, l = 21), 0), color = "t3"), size = 1) +
    #   geom_line(aes(y = pmax(sg.smooth(t2, l = 21), 0), color = "t2"), size = 1) +
    #   geom_line(aes(y = pmax(sg.smooth(t1, l = 21), 0), color = "t1"), size = 1) +
    
    # Create a ggplot object with no smoothing
    p <- ggplot(data = testSEQdf, aes(x = rtimes)) +
      geom_line(aes(y = totIntensity, color = "total"), size = 0.5) +
      geom_line(aes(y = t4, color = "t4"), size = 0.5) +
      geom_line(aes(y = t3, color = "t3"), size = 0.5) +
      geom_line(aes(y = t2, color = "t2"), size = 0.5) +
      geom_line(aes(y = t1, color = "t1"), size = 0.5) +
      
      # Set custom colors
      scale_color_manual(values = custom_colors) +
      
      # Customize the plot labels and legend
      labs(title = seq,
           x = "rtime",
           y = "intensity") +
      
      my_theme +  # Apply the custom theme
      
      # Add a legend with custom labels
      guides(color = guide_legend(title = "Legend",
                                  override.aes = list(size = 3,
                                                      linetype = "solid",
                                                      shape = NA)))
    
    # Define the filename based on sequence name and filepath
    filename <- paste(seq, "from", gsub(".*/", "", gsub("\\..*", "", filepath)), ".png", sep = "_")
    
    # Specify the full path where you want to save the plot in the "graphs" directory
    full_path <- file.path(graphs_dir, filename)
    
    # Save the plot as a PNG file in the "graphs" directory
    ggsave(full_path, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
  }
}


library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)
start_time <- Sys.time()
foreach(i=allpaths) %dopar% {
  
  create_graphs(i,path_to_Eco_folder)
  
}
#stop cluster
stopCluster(cl)
# Record the end time
end_time <- Sys.time()

# Calculate the elapsed time
elapsed_time <- end_time - start_time

# Print the elapsed time in seconds
cat("Task completed in", as.numeric(elapsed_time, units = "secs"), "seconds.\n")