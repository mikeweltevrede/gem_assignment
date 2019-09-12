### From pseudocode
rm(list = ls())

## Initialising steps
library(readxl)
library(bazar)

import_data <- function(file_location) {
  data <- readxl::read_excel(file_location, .name_repair = "minimal")
  colnames(data)[1] <- "Patient"

  # Define the priority ordering f
  f <- sort(data$Patient)

  # Define which patient ID codes for the waiting list (maximum patient ID + 1)
  number_of_patients <- max(f)
  w <- number_of_patients + 1

  colnames(data) <- append("Patient", as.character(1:w))
  patient_names <- as.character(data$Patient)

  # Define preference profiles; i.e. all data except for patient ID
  preferences <- data[, -1]
  rm(data) # Clean up; `data` is not needed in the rest of this function

  # Initialise kidneys: make patients "point" to currently "favourite" kidneys
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

data <- import_data(file_location = "data/dataset7.xlsx")

## Algorithm
# Start;

# Reassign preferences for non-assigned patients to the most preferred
# available option;

# Find all cycles C;
cycle_finder <- function(current_assignment){
  return(c(0,1,2,3))
}

cycles = cycle_finder(data$current_assignment)
print(cycles)

# Find the patient t with the highest priority in C with currently best
# preference p_t^∗

# Set p_t := p_t^∗

# What if p_t^∗ is an available kidney?

# while cycles exist do

  # Step X;

  # while cycles of length less than or equal to q exist do
    # Assign patients in the cycle accordingly;

    # Remove assigned patients from the patient list;

    # Return to Start;

  # Step Y: Set pt to the next preferred option pt' of patient t (“break” their
  # current preference);

  # if pt is w then
    # Reset pt to p_t^∗

    # Find the patient t' != t with the highest priority in C;

    # Set t to t'

    # Return to Step Y;

  # else if pt is an available kidney then
    # Assign pt to patient t;
    # Remove pt from and add kt to the list of available kidneys;
    # Remove t from patient list;
    # Return to Start;
  # else
    # Find all cycles;
    # Go to Step X

# w chains
