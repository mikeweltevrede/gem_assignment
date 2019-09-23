#### Initialise document ####
#'Assignment Games and Economic Models
#'@author Steffie van Poppel, Mike Weltevrede, Joost Westland (Group 7)

# Clean environment
rm(list = ls())

# Activating packages
library(readxl)
library(bazar)

#### Defining functions ####

#'Import and process data
#'
#'\code{import_data} reads data and does some basic cleaning and computations.
#'
#'@param \code{file_location} The path to the data to read in.
#'
#'@return A list \code{f} containing the priority ordering (which is computed as
#'  the ascending order of patient numbers), the numeric encoding \code{w}
#'  for the waiting list  (which is computed as the maximum patient number plus
#'  1), the matrix with \code{preferences}, a list \code{current_assignment}
#'  with currently favourite kidneys, an empty list \code{final_assignment}
#'  with patient numbers as names to store the final assignment, an empty vector
#'  \code{assigned} to store the patients that are assigned, and an empty vector
#'  \code{available_kidneys} to store the kidneys that become available due to
#'  assigning w-chains
import_data <- function(file_location) {
  
  data <- readxl::read_excel(file_location, .name_repair = "minimal")
  
  colnames(data)[1] <- "Patient"
  
  # Define the priority ordering f (ascending order of patient number)
  f <- sort(data$Patient)
  number_of_patients <- max(f)
  
  # Define which patient ID codes for the waiting list (maximum patient ID + 1)
  w <- number_of_patients + 1
  
  # Define patient names as the character variant of their patient number
  colnames(data) <- append("Patient", as.character(1:w))
  patient_names <- as.character(data$Patient)

  # Define preference profiles; i.e. all data except for patient ID
  preferences <- data[, -1]

  # Initialise kidneys and living donors
  
  # Make patients point to kidneys
  current_assignment <- t(preferences[, 1]) # Initialise to first choice
  names(current_assignment) <- patient_names
  current_assignment <- as.list(current_assignment)
  
  # Initialise the final_assignment to 0
  final_assignment <- t(as.matrix(rep(0, number_of_patients)))
  names(final_assignment) <- patient_names
  final_assignment <- as.list(final_assignment) 
  
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = c(),
              "available_kidneys" = c()))
}

#'Search for cycles
#'
#'\code{circle_finder} finds circles in the current preferences of patients,
#'assigns these, and updates the variables accordingly. Notice that we use the
#'term circle instead of cycle, since \code{cycle()} is a function in R.
#'
#'@param \code{data} List with structure like the output of \code{import_data}.
#'
#'@return A list \code{f} containing the priority ordering, the numeric encoding
#'  \code{w} for the waiting list, the matrix with \code{preferences}, a list
#'  \code{current_assignment} with currently favourite kidneys, a list
#'  \code{final_assignment} the currently final assignment, a vector
#'  \code{assigned} with patients that are already assigned, and a vector
#'  \code{available_kidneys} with kidneys that become available due to
#'  assigning w-chains.
circle_finder <- function(data){
  
  # Unpack the list
  f <- data$f
  w <- data$w
  preferences <- data$preferences
  current_assignment <- data$current_assignment
  final_assignment <- data$final_assignment
  assigned <- data$assigned
  available_kidneys <- data$available_kidneys
  
  rm(data) # Clean up; `data` is not needed in the rest of this function
  
  circle_found <- TRUE # Initialise
  
  # As long as cycles exist in the current assignment...
  while (circle_found) {
    
    # Initialise
    new_assigned <- c()
    no_circle_found_so_far <- c(w)
    
    # For every patient in the order of priority list...
    for (i in 1:length(f)) {
      
      if (!f[i] %in% no_circle_found_so_far) {
        # Then patient is not yet assigned
        
        current_chain <- c() # Create an empty chain
        j <- f[i] # Becomes the current step
        
        # As long as j is not already assigned to something...
        while (!j %in% no_circle_found_so_far && !j %in% assigned) {
          
          current_chain <- append(current_chain, j)
          j <- current_assignment[[as.character(j)]]
          
          if (j == w || j %in% no_circle_found_so_far) {
            # If j is in a w-chain or already assigned, stop
            
            # Assign to no_circle_found_so_far, when a new patient point to
            # something that's already checked, stop
            no_circle_found_so_far <- append(no_circle_found_so_far,
                                             current_chain)
          }
          
          if (j %in% current_chain) {
            # When j is in the current chain then we have found a cycle
            circle <- current_chain[
              which(current_chain == j):length(current_chain)]
            
            for (k in 1:length(circle)) {
              # Update the final result
              final_assignment[[as.character(circle[k])]] <-
                current_assignment[[as.character(circle[k])]]
            }
            
            # Append new_assigned, we need to update this in current_assignment
            new_assigned <- append(new_assigned, circle)
            
            # When a patient points to the cycle, we don't need to look further
            # for this chain.
            no_circle_found_so_far <- append(no_circle_found_so_far,
                                             current_chain)
          }
        }
      }
    }
    
    # If circles exist, assign all circles
    if (!is.null(new_assigned)) {
      # Check if we have assigned anything
      assigned <- append(assigned, new_assigned)
      selection <- c()
      
      for (k in 1:length(new_assigned)) {
        # Select patients that are assigned in this loop
        selection <- append(selection,
                            which(as.numeric(
                              names(current_assignment)) == new_assigned[k]))
      }
      
      # Select all patients not assigned in this loop (with the minus sign)
      selection <- -selection 
      
      # Drop all assigned patients from the graph
      current_assignment <- current_assignment[selection] 
      f <- f[selection]
      
      # Check if their are any patients left
      if (!bazar::is.empty(current_assignment)) {
      
        # Reassign arrows and recheck circles
        for (k in 1:length(current_assignment)) {
          # Only look to the remaining patients
          index.X <- as.numeric(names(current_assignment)[k])
          
          # Not allowed to point to the assigned patients, except the kidneys
          # that remain after the w chain
          index.Y <- !preferences[index.X, ] %in% 
            assigned[which(!assigned %in% available_kidneys)]
          
          current_assignment[k] <- preferences[index.X, index.Y][1]
        }
      } else {
        # If no patients are left, exit the loop
        circle_found <- FALSE
      }
    } else {
      # If no cycles are found, exit the loop
      circle_found <- FALSE
    }
  }
  # Return updated data points
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys" = available_kidneys))
}

#'Find w-chains
#'
#'\code{w_finder} finds w-chains according to chain selection rule (e) in the
#'current preferences of patients, assigns these, and updates the variables
#'accordingly.
#'
#'@param \code{data} List with structure like the output of \code{import_data}.
#'
#'@return A list \code{f} containing the priority ordering, the numeric encoding
#'  \code{w} for the waiting list, the matrix with \code{preferences}, a list
#'  \code{current_assignment} with currently favourite kidneys, a list
#'  \code{final_assignment} the currently final assignment, a vector
#'  \code{assigned} with patients that are already assigned, and a vector
#'  \code{available_kidneys} with kidneys that become available due to
#'  assigning w-chains.
w_finder <- function(data){
  
  # Unpack the list
  f <- data$f
  w <- data$w
  preferences <- data$preferences
  current_assignment <- data$current_assignment
  final_assignment <- data$final_assignment
  assigned <- data$assigned
  available_kidneys <- data$available_kidneys
  
  rm(data) # Clean up; `data` is not needed in the rest of this function

  # Initialize temporary data
  w_chain <- c()
  new_assigned <- c()
  already_checked <- c(w)
  first_of_w_chain <- w
  
  # Search the w_chain for all patients in the order of priority list
  for (i in 1:length(f)) {
    
    # Check if we have not yet found the current patient
    if (!f[i] %in% already_checked) {
      current_chain <- c()
      
      # Similar as in circle_finder, j is the current step in the chain
      j <- f[i]

      # As long as j is available...
      while (!j %in% already_checked && !j %in% assigned) {
        current_chain <- append(current_chain, j)
        j <- current_assignment[[as.character(j)]]
        
        # Check if j is still avalable, otherwise update already checked and
        # stop the loop
        if (j == w || j %in% already_checked) {
          already_checked <- append(already_checked, current_chain)
        }
        
        # Check if j is a remaining kidney, patient is already assigned, but
        # kidney not yet (first entry of w-chain with highest priority)
        if (j %in% available_kidneys) {
          
          index <- which(available_kidneys == j)
          available_kidneys[index] <- current_chain[1]
          
          # Update final_assignment
          for (k in 1:length(current_chain)) {
            final_assignment[[as.character(current_chain[k])]] <-
              current_assignment[[as.character(current_chain[k])]]
          }
          
          # Update the new_assigned entries
          new_assigned <- append(new_assigned, current_chain)
          already_checked <- append(already_checked, current_chain)
        }
        
        # When j is equal to the entry of the current w_chain with the highest
        # priority, then we expand the w-chain
        if (j == first_of_w_chain) {
          
          w_chain <- append(current_chain, w_chain)
          if (w != first_of_w_chain) {
          already_checked <- append(already_checked, current_chain)
          }
          first_of_w_chain <- w_chain[1]
        }
      }
    }
  }
  
  # Check if we have found a w-chain
  if (!is.null(w_chain)) {
    
    # Update final_assignment
    for (k in 1:length(w_chain)) {
      final_assignment[[as.character(w_chain[k])]] <-
        current_assignment[[as.character(w_chain[k])]]
    }
    
    # If w is not equal to the first_of_w_chain...
    if (w != first_of_w_chain) {
      available_kidneys <- append(available_kidneys, first_of_w_chain)
    }
  }
  
  new_assigned <- append(new_assigned, w_chain)
  
  # Check if the new_assigned are not empty
  if (!is.null(new_assigned)) {
    assigned <- append(assigned, new_assigned)
    selection <- c()
    
    # Throw out all already assigned patients
    for (k in 1:length(new_assigned)) {
      selection <- append(selection, which(as.numeric(
        names(current_assignment)) == new_assigned[k]))
    }
    
    selection <- -selection
    current_assignment <- current_assignment[selection]
    f <- f[selection]
  }
  
  # Update preferences, patients can only point to the first kidney of the
  # w-chain(s), not to the other patients
  for (k in 1:length(current_assignment)) {
    
    index.X <- as.numeric(names(current_assignment)[k])
    index.Y <- !preferences[index.X, ] %in%
      assigned[which(!assigned %in% available_kidneys)]
    current_assignment[k] <- preferences[index.X, index.Y][1]
  }
    
  # Return updated data
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys" = available_kidneys))
}

#'Runs the TTCC algorithm with chain selection rule (e)
#'
#'\code{exercise_a} executes the TTCC algorithm with chain selection rule (e) by
#'repeating the process of finding cycles, followed by finding w-chains until
#'all patients are assigned
#'
#'@param \code{file_location} The path to the data to read in.
#'
#'@return A \code{data.frame} \code{final_assignment} with the final assignment
#'for all patients and a list \code{remaining_kidneys} with the kidneys that are
#'not assigned to a patient (as a result of their respective patient being the
#'first patient in an assigned w-chain).
exercise_a <- function(file_location){
  # Import data, using the function that is written above
  iterate_data <- import_data(file_location = file_location)
  
  # All patients that are currently not yet assigned
  f <- iterate_data$f

  # As long as patients remain unassigned
  while (!bazar::is.empty(f)) {
    
    # Search for cycles...
    iterate_data <- circle_finder(iterate_data)
    
    # And update f accordingly
    f <- iterate_data$f
    
    # Check if patients are unassigned
    if (!bazar::is.empty(f)) {
      # Then there is a w-chain left, since all cycles were removed by
      # circle_finder
      iterate_data <- w_finder(iterate_data) 
      f <- iterate_data$f #update f
    }
  }
  
  # Update the final output for the waiting list
  iterate_data$final_assignment[
    which(iterate_data$final_assignment == iterate_data$w)] <- "w"
  
  # Create tidy table
  df.final <- data.frame(iterate_data$final_assignment)
  colnames(df.final) <- names(iterate_data$final_assignment)
  
  # Return final result
  return(list("final_assignment" = df.final,
              "remaining_kidneys" = iterate_data$available_kidneys))
}

#### Run the algorithm ####
result <- exercise_a(file_location = "data/dataset7.xlsx")
