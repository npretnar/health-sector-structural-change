rm(list=ls())

library(dplyr)
library(stringr)

# figure/table results output directory
results_dir = "../../../results"

# analysis directory
#analysis_dir = "data/analysis"

sink(paste0(results_dir,"/2_monte_carlo_results_GE_Calibration_Variable_Alpha_c.txt"))

print("Monte Carlo Results for GE_Calibration_Variable_Alpha_c")

# Get the list of files with names starting with "loss" and ending with ".txt"
file_list <- list.files(pattern = "^loss.*\\.txt$")

# Initialize an empty list to store the data frames
data_frames <- list()
loss_mins <- list()
loss_mins_where <- list()

# Loop through each file and read its contents into a data frame
for (file in file_list) {
  # Read the file into a data frame
  df <- t(read.table(file, quote="\"", comment.char=""))
  
  # Store the data frame in the list, using the file name as the list element name
  data_frames[[file]] <- df
  
  # mins
  loss_mins[[file]] = min(data_frames[[file]])
  
  # mins where
  loss_mins_where[[file]] = which.min(data_frames[[file]])
  
}

# where is the min of the min?
which.file = substr(file_list[which.min(unlist(loss_mins))],5,11)

# get the minimal parameters
min_parms_file <- list()
for(i in 1:6){
  min_parms_file[[i]] = t(read.table(paste0("parm",which.file,"_",i,".txt"), quote="\"", comment.char=""))
}

### find loss minimum ###
n = which.min(unlist(loss_mins))
print("Value of min. loss function:")
print(loss_mins[[n]])
print("Parameter values at loss function minimum:")
print("A_h0:")
print(min_parms_file[[1]][loss_mins_where[[n]]])
print("g_h1:")
print(min_parms_file[[2]][loss_mins_where[[n]]])
print("g_h2:")
print(min_parms_file[[3]][loss_mins_where[[n]]])
print("g_h3:")
print(min_parms_file[[4]][loss_mins_where[[n]]])
print("g_h4:")
print(min_parms_file[[5]][loss_mins_where[[n]]])
print("xi:")
print(min_parms_file[[6]][loss_mins_where[[n]]])

write.table(c(min_parms_file[[1]][loss_mins_where[[n]]],
              min_parms_file[[2]][loss_mins_where[[n]]],
              min_parms_file[[3]][loss_mins_where[[n]]],
              min_parms_file[[4]][loss_mins_where[[n]]],
              min_parms_file[[5]][loss_mins_where[[n]]],
              min_parms_file[[6]][loss_mins_where[[n]]]
              ),file="parms_min.txt",sep=" ",col.names=FALSE,row.names=FALSE)

closeAllConnections()
