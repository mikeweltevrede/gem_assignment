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

  patient_names <- as.character(data$Patient)
  colnames(data) <- append("Patient", append(patient_names, w))

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
              "patient_names" = patient_names,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = c(),
              "available_kidneys" = c()))
}

# Find all cycles C;
cycle_finder <- function(data){
  current_assignment = data$current_assignment
  w = data$w
  cycles = list()
  assigned_patients = c()
  
  # for (i in c(1)) {
  for (i in 1:length(current_assignment)) {
    
    if (i %in% assigned_patients) {
      next
    }
    
    # Start a chain
    current_chain = c(current_assignment[i])
    
    # Find the preferred kidney by this patient
    points_to = as.character(current_assignment[i])
    
    if (points_to == w) {
      # TODO: Here we can also find chains, possibly
      next
    } else if (points_to %in% assigned_patients) {
      # Then this kidney is not available
      # TODO: point to next one
    }
    
    if (points_to == names(current_assignment)[i]) {
      # Then the patient points to themselves, assign this
      cycle = current_chain
      cycles = c(cycles, list(cycle))
      assigned_patients = append(assigned_patients, as.character(points_to))
      next
    }
    
    # Set the next patient to the one corresponding to the preferred kidney
    next_patient = current_assignment[points_to]
    
    while (next_patient != w) {
      if (next_patient %in% names(current_chain)) {
        # Cycle found
        cycle = append(current_chain, next_patient)
        assigned_patients = append(assigned_patients,
                                   as.character(unique(cycle)))
        break
      }
      
      # Extend chain with the patient that is being pointed to
      current_chain = append(current_chain, next_patient)
      
      # Find patient that the this next patient prefers
      next_patient = current_assignment[as.character(next_patient)]
    }
    # Here, next_patient will be w
    # TODO: Can we do something with this?
    
    # Append new cycle to set of known cycles
    cycles = c(cycles, list(cycle))
  }
  return(list("cycles" = cycles,
              "assigned_patients" = assigned_patients))
}

cycle_assigner = function(final_assignment = list(), cycles) {
  # Assign cycles
  
  for (cycle in cycles) {
    # Assign to final_assignment
    final_assignment = append(final_assignment, cycle)
  }
  
  return(final_assignment)
}

preference_updater = function(preferences, assigned_patients, patient_names,
                              hpbm = FALSE) {
  
  # Cannot drop since we index by row number
  for (i in 1:dim(preferences)[1]) {
    if (i %in% assigned_patients) {
      next
    }
    
    j = 2
    while (preferences[i, 1] %in% assigned_patients) {
      preferences[i, 1] = preferences[i, j]
      j = j + 1
    }
  }
  
  current_assignment = t(preferences[, 1]) # Initialise to first choice
  names(current_assignment) = patient_names
  current_assignment = as.list(current_assignment)
  
  if (hpbm) {
    assigned_patients = head(assigned_patients, -1)
  }
  
  current_assignment = current_assignment[!names(current_assignment) %in% assigned_patients]
  
  return(list("preferences" = preferences, "current_assignment" = current_assignment))
}

# Write the HPBM Algorithm
hpbm = function(cycles, preferences, assigned_patients, patient_names,
                current_assignment, final_assignment, f, w, available_kidneys) {
  
  preferences_old = preferences
  
  # Cycles is a list of list with too long cycles
  patients_in_cycles = unique(names(unlist(cycles)))
  candidates = patients_in_cycles
  
  # Return here
  while (TRUE) {
    t = f[min(match(candidates, f), na.rm = TRUE)]
    p_star = current_assignment[[t]]
    
    not_allowed = c(unlist(assigned_patients), p_star)
    p = preference_updater(preferences_old, not_allowed, patient_names, hpbm = TRUE)
    preferences = p$preferences
    current_assignment = p$current_assignment
    p_prime = current_assignment[t]
    
    if (p_prime == w) {
      # Reset preferences
      preferences = preferences_old
      candidates = candidates[-1]
      
    } else if (p_prime %in% available_kidneys){
      # Available kidneys
      
      final_assignment = append(final_assignment, p_prime)
      
      # Go to Start
      # TODO!!!!!!!!!
      
    } else {
      # Find cycles
      my_data = list("current_assignment" = current_assignment, "w" = w)
      result = cycle_finder(my_data)
      cycles = result$cycles
      
      if (t %in% unique(names(unlist(cycles)))) {
        t_cycles = cycles[sapply(cycles, function(x) {t %in% names(x)})]
        cycle_lengths = sapply(t_cycles, length)
        
        # And find cycles that obey the capacity constraint
        correct_index = which(cycle_lengths <= q)
        correct_cycles = cycles[correct_index]
        
        if (length(correct_cycles) > 0) {
          final_assignment = cycle_assigner(final_assignment, correct_cycles)
          
          # And kick them out / update preferences
          assigned_patients = names(unlist(correct_cycles))
          update = preference_updater(preferences, assigned_patients, patient_names)
          preferences = update$preferences
          current_assignment = update$current_assignment
          
          # Go to Start
          # TODO!!!!!!!!!
        } else {
          # Go to step Y; namely stay with t and update preferences
          # I.e. go to beginning of while loop
        }
      } else {
        # Go to step Y; namely stay with t and update preferences
        # I.e. go to beginning of while loop
      }
    }
    
  }
}

# data <- import_data(file_location = "data/dataset7.xlsx")
data <- import_data(file_location = "data/test_steffie.xlsx")
final_assignment = list()
preferences = data$preferences
patient_names = data$patient_names
current_assignment = data$current_assignment
f = data$f
w = data$w

## Algorithm
# Start;
q = 3 # Unhardcode, function?

# Reassign preferences for non-assigned patients to the most preferred
# available option;
# TODO -> Check, see function preference_updater

# Go here after HPBM is successful
result = cycle_finder(data)
assigned_patients = result$assigned_patients
cycles = result$cycles

# TODO: Remove
assigned_patients = c()

while (length(cycles) > 0) {
  # Then cycles exist; calculate lengths of these cycles...
  cycle_lengths = sapply(cycles, length)
  
  # And find cycles that obey the capacity constraint
  correct_index = which(cycle_lengths <= q)
  correct_cycles = cycles[correct_index]

  if (length(correct_cycles) > 0) {
    # There are correct cycles, assign these
    final_assignment = cycle_assigner(final_assignment, correct_cycles)
    
    # And kick them out / update preferences
    assigned_patients = names(unlist(correct_cycles))
    update = preference_updater(preferences, assigned_patients, patient_names)
    preferences = update$preferences
    current_assignment = update$current_assignment
    
    # Update cycles variable; remove assigned cycles
    cycles = cycles[-correct_index]
    # TODO: Move into function? See also hpbm
    
  } else {
    # Then there are cycles, but these are all too long
    # TODO: Apply HPBM
    # print("HPBM")
    # print("Deleting first cycle for testing purposes")
    # final_assignment = cycle_assigner(final_assignment, cycles[-1])
    # update = preference_updater(preferences, assigned_patients, patient_names) # TODO
    # preferences = update$preferences
    # current_assignment = update$current_assignment
    # 
    # # Update cycles variable; remove assigned cycles
    # cycles = cycles[-1]
    break
  }
    
}




if (FALSE) {
  # Just to avoid error
  # TODO: Remove
} else {
  # There are no cycles at all
  # TODO: look at w-chains
}



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
