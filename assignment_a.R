# Assignment Games and Economic Models
# Group 7
# With chain selection (e)


# Step 1
# Initialise kidneys and living donors
library(readxl)
library(tidyverse)
data <- read_excel("data/dataset7.xlsx")

# Make patients point to kidneys
## Initialise to first choice
current_preferences <- vector("list")
patient_id <- data[,1]
data <- data[,-1]
cp <- t(data[,1])
names(cp) <- lapply(as.list(patient_id), as.character)$Patient # unhardcode this



for (i in 1:length(data[,2])){
  current_preferences[[as.character(patient_id[i])]] <- data[i,1]
}
current_preferences <- as.list(data[,1])
names(current_preferences) <- lapply(as.list(patient_id), as.character)


setNames(as.list(data[,2]), as.character(data[,1]))

# Make kidneys point to patients

# Step 2


# a
# Check if cycle
while(T)
## If cycle, kick out all cycles

  ### Reassign arrows and recheck cycles

## Else, go to step 3
  break

# Step 3
## Check if there are pairs left
  ### Stop
## Else, find w-chain according to (e)
  ## Repeat 2 and 3
