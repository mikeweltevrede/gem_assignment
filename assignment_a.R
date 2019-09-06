# Assignment Games and Economic Models
# Group 7
# With chain selection (e)

#### installing packages ####
library(readxl)

#### importing the data ####
import_data <- function(file_location) {
  data <- readxl::read_excel(file_location, .name_repair = "minimal")
  
  # Define the priority ordering f
  f <- sort(data$Patient)
  number_of_patients <- max(f)
  
  # Define which patient ID codes for the waiting list (maximum patient ID + 1)
  w <- number_of_patients + 1
  
  colnames(data) <- append("Patient", as.character(1:w))
  patient_names <- as.character(data$Patient)

  # Define preference profiles; i.e. all data except for patient ID
  preferences <- data[, -1]
  rm(data) # Clean up; `data` is not needed in the rest of this function
  
  #### Step 1 ####
  # Initialise kidneys and living donors
  
  # Make patients point to kidneys
  # TODO: Test if this can be done easier
  initial_assignment <- t(preferences[, 1]) # Initialise to first choice
  names(initial_assignment) <- patient_names
  initial_assignment <- as.list(initial_assignment)
  
  # Initialise the final_assignment to 0
  # TODO: Test if this can be done easier
  final_assignment <- t(as.matrix(rep(0, number_of_patients)))
  names(final_assignment) <- patient_names
  final_assignment <- as.list(final_assignment) 
  
  # Initialise set of currently assigned kidneys
  assigned <- c()
  
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "initial_assignment" = initial_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned))
}

data <- import_data(file_location = "data/dataset7.xlsx")

# Step 2
# a
# Check if cycle
cycle_finder <- function(data){
  f <- data$f
  w <- data$w
  preferences <- data$preferences
  current_assignment <- data$initial_assignment
  final_assignment <- data$final_assignment
  assigned <- data$assigned
  
  rm(data) # Clean up; `data` is not needed in the rest of this function
  
  cycle_found <- T # Initialise
  
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
            no_cycle_found_so_far <-
              append(no_cycle_found_so_far, current_chain)
          }
          
          if (j %in% current_chain) {
            cycle <-
              current_chain[which(current_chain == j):length(current_chain)]
            
            for (k in 1:length(cycle)) {
              final_assignment[[cycle[k]]] <- current_assignment[[cycle[k]]]
            }
            
            new_assigned <- append(new_assigned, cycle)
            no_cycle_found_so_far <-
              append(no_cycle_found_so_far, current_chain)
          }
        }
      }
    }
    
    ## If cycle, kick out all cycles
    if (!is.null(new_assigned)) {
      assigned <- append(assigned, new_assigned)
      selection <- c()
      
      # TODO: This currently assumes that the patients are 1:end;
      # make this to also recognise random numbers
      for (k in 1:length(new_assigned)) {
        selection <-
          append(selection, which(
            as.numeric(names(current_assignment)) == new_assigned[k]))
      }
      
      selection <- -selection # What does this do?
      current_assignment <- current_assignment[selection]
      f <- f[selection]
      
      # Reassign arrows and recheck cycles
      for (k in 1:length(current_assignment)) {
        if (current_assignment[k] %in% new_assigned) {
          # TODO: This can be neater (repeated indices)
          index <- as.numeric(names(current_assignment)[k])
          current_assignment[k] <- 
            preferences[index, which(!preferences[index, ] %in% assigned)][1]
        }
      }
    } else {
      cycle_found <- F
    }
  } ## Else, go to step 3
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned))
}

data2 <- cycle_finder(data)

# Step 3
## Check if there are pairs left
### Stop
## Else, find w-chain according to (e)
## Repeat 2 and 3
