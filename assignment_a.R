# Assignment Games and Economic Models
# Group 7
# With chain selection (e)

#### installing packages ####
install.packages("readxl")
library(readxl)

#### importing the data ####
import_data <- function(file_location) {
  data <- read_excel(file_location)
  preferences <- data[,-1] # select preferences; all data except for patient ID

  #### Step 1 ####
  # Initialise kidneys and living donors # Make patients point to kidneys
  # TODO: Unhardcode this
  patient_names <- lapply(as.list(data[,1]), as.character)$Patient
  
  # TODO: Test if this can be done easier
  initial_assignment <- t(preferences[,1]) # Initialise to first choice
  names(initial_assignment) <- patient_names
  initial_assignment <- as.list(initial_assignment)
  
  final_assignment <- t(as.matrix(rep(0, dim(data)[1]))) #set the final_assignment to 0
  names(final_assignment) <- patient_names
  final_assignment <- as.list(final_assignment) 
  
  # TODO: Give descriptive names
  f <- 1:dim(data)[1] # Define the priority order f
  w <- dim(preferences)[1] + 1 # Define which patient ID is the waiting list
  
  # TODO: Test this
  # f_to_test <- sort(data$Patient)
  # w_to_test <- max(data$Patient) # or perhaps hardcode to 150
  
  assigned <- c() # Initialise set of assigned kidneys
  
  return(list('initial_assignment' = initial_assignment,
              'preferences' = preferences,
              'final_assignment' = final_assignment,
              'assigned' = assigned,
              'f' = f, 
              'w' = w))
}

# TODO: Unhardcode this
# iterate_data <- import_data(file_location="data/dataset7.xlsx")
iterate_data <- import_data(file_location = "C:/Users/Joost/Desktop/gem_assignment-first_setup/dataset7.xlsx") #locatie pc joost
# dataStart <- import_data(file_location="C:/Users/Joost/Desktop/gem_assignment-first_setup/dataset7.xlsx") #locatie pc mike
# dataStart <- import_data(file_location="C:/Users/Joost/Desktop/gem_assignment-first_setup/dataset7.xlsx") #locatie pc steffie

# Step 2
# a
# Check if cycle
cycle_finder <- function(dataStart){
  current_assignment <- dataStart$initial_assignment
  preferences <- dataStart$preferences
  final_assignment <- dataStart$final_assignment
  assigned <- dataStart$assigned
  f <- dataStart$f
  w <- dataStart$w
  
  cycle_found <- T
  
  while (cycle_found) {
    new_assigned <- c()
    no_cycle_found_so_far <- c(w)
    for (i in 1:length(f)) {
      if (!f[i] %in% no_cycle_found_so_far) {
        current_chain <- c()
        j <- f[i]
        
        while (!j %in% no_cycle_found_so_far) {
          current_chain <- append(current_chain, j)
          j <- current_assignment[[as.character(j)]]
          
          if (j == w || j %in% no_cycle_found_so_far) {
            no_cycle_found_so_far <- append(no_cycle_found_so_far, current_chain)
          }
          
          if (j %in% current_chain) {
            cycle <- current_chain[which(current_chain == j):length(current_chain)]
            
            for (k in 1:length(cycle)) {
              final_assignment[[cycle[k]]] <- current_assignment[[cycle[k]]]
            }
            
            new_assigned <- append(new_assigned, cycle)
            no_cycle_found_so_far <- append(no_cycle_found_so_far, current_chain)
          }
        }
      }
    }
    
    if (!is.null(new_assigned)) { ## If cycle, kick out all cycles
      assigned <- append(assigned, new_assigned)
      selection <- c()
      
      for (k in 1:length(new_assigned)) {
        selection <- append(selection, 
                            which(as.numeric(names(current_assignment)) == new_assigned[k]))
      }
      
      selection <- -selection
      current_assignment <- current_assignment[selection]
      f <- f[selection]
      for (k in 1:length(current_assignment)) { ### Reassign arrows and recheck cycles
        if (current_assignment[k] %in% new_assigned) {
          current_assignment[k] <- preferences[as.numeric(names(current_assignment)[k]) , which(!preferences[as.numeric(names(current_assignment)[k]),] %in% assigned)][1]
        }
      }
    } else {
      cycle_found <- F
    }
  }## Else, go to step 3
  return(list('initial_assignment' = current_assignment,
              'preferences' = preferences,
              'final_assignment' = final_assignment,
              'assigned' = assigned,
              'f' = f,
              'w' = w))
}
iterate_data <- cycle_finder(iterate_data)

# Step 3
## Check if there are pairs left
### Stop
## Else, find w-chain according to (e)
## Repeat 2 and 3