### From pseudocode
rm(list = ls())

## Initialising steps
library(readxl)
library(bazar)

import_data = function(file_location) {
  data = readxl::read_excel(file_location, .name_repair = "minimal")
  colnames(data)[1] = "Patient"

  # Define the priority ordering f
  f = sort(as.numeric(data$Patient))

  # Define which patient ID codes for the waiting list (maximum patient ID + 1)
  number_of_patients = max(f)
  w = number_of_patients + 1

  # Clean up column names
  patient_names = as.character(data$Patient)
  colnames(data) = append("Patient", append(patient_names, w))

  # Define preference profiles; i.e. all data except for patient ID
  preferences = data[, -1]

  return(list("preferences" = preferences,
              "patient_names" = patient_names,
              "f" = f,
              "w" = w))
}

# Find all circles C;
circle_finder = function(current_assignment, w){
  circles = list()
  chains = list()
  
  # This is not the set of already assigned but those in a cycle/chain
  already_checked = c() 
  
  # Initialise an empty cycle and w-chain
  circle = c()
  w_chain = c()
  
  for (i in 1:length(current_assignment)) {
    already_found = FALSE # Initialise boolean # TODO
    
    if (names(current_assignment)[i] %in% already_checked) {
      # This patient belongs to a cycle or chain that we already explored
      next
    }

    # Start a chain
    current_chain = c(current_assignment[i])
    
    # Find the preferred kidney by this patient
    points_to = as.character(current_assignment[i])
    
    if (points_to == names(current_assignment)[i]) {
      # This patient points to themselves, assign this
      circle = current_chain
      circles = c(circles, list(circle))
      
      already_checked = append(already_checked, unique(names(circle)))
      next
    }
    
    if (points_to == w) {
      # w-chain found
      w_chain = current_chain
      
      chains = c(chains, list(w_chain))
      already_checked = append(already_checked, unique(names(w_chain)))
      next
    }
    
    # Set the next patient to the one corresponding to the preferred kidney
    next_patient = current_assignment[points_to]
    
    while (TRUE) {
      if (next_patient %in% unlist(circles)) {
        # This will lead to a circle already found before: skip
        checked = append(current_chain, next_patient)
        already_checked = append(already_checked, unique(names(checked)))
        
        already_found = TRUE
        break
        
      } else if (next_patient %in% names(unlist(chains))) {
        # This will extend an existing chain or will be a new branch.
        # TODO: Write this (!!!!!!!!!!!!!!)
        
        already_found = TRUE
        break
      }
      
      if (next_patient %in% names(current_chain)) {
        # Circle found
        
        # Check where it is linked to, don't keep the patients before this (appendix to the cycle)
        starting_index = which(next_patient == names(current_chain))
        
        circle = append(current_chain[starting_index:length(current_chain)],
          next_patient)
        
        already_checked = append(already_checked, unique(names(circle)))
        break
      }
      
      if (next_patient == w) {
        # w-chain found
        w_chain = append(current_chain, next_patient)
        
        already_checked = append(already_checked, unique(names(w_chain)))
        break
      }
      
      # No circle or w-chain is found with this iteration, so continue by
      # extending the chain with the patient that is being pointed to.
      current_chain = append(current_chain, next_patient)
      
      # Find patient that this next patient prefers
      next_patient = current_assignment[as.character(next_patient)]
    }
    
    if (already_found) {
      # Don't append this circle or chain!
      next
    }

    if (length(circle) > 0) {
      # A cycle was found, append this to the list of cycles
      circles = c(circles, list(circle))
    }
    
    if (length(w_chain) > 0) {
      # A w-chain was found, append this to the list of w-chains
      chains = c(chains, list(w_chain))
    }
  }
  
  return(list("circles" = circles,
              "chains" = chains))
}

circle_assigner = function(final_assignment = list(), circles) {
  
  for (circle in circles) {
    final_assignment = append(final_assignment, circle)
  }
  
  return(final_assignment)
}

chain_assigner = function(final_assignment = list(), chains, f,
                          available_kidneys, assigned_patients) {
  
  in_chain = names(unlist(chains))
  
  # Find the person highest on the priority list
  index = which.min(match(in_chain, f))
  highest_priority = in_chain[index]
  
  selected_chain = chains[[which(sapply(chains,
                                 function(x) {highest_priority %in% names(x)}
                                 ))]]
  
  # Add first kidney in chain to available kidneys
  available_kidneys = c(available_kidneys, names(selected_chain)[1])
  final_assignment = append(final_assignment, selected_chain)
  
  
  assigned_patients = c(assigned_patients, names(unlist(selected_chain)))
  
  return(list("final_assignment" = final_assignment,
              "available_kidneys" = available_kidneys,
              "assigned_patients" = assigned_patients))
}

preference_updater = function(preferences, assigned_patients, patient_names,
                              hpbm = FALSE) {
  
  for (i in 1:dim(preferences)[1]) {
    if (i %in% assigned_patients) {
      # Don't update preferences for assigned patients
      next
    }
    
    j = 2
    while (preferences[i, 1] %in% assigned_patients) {
      # Patient cannot have their next best preference be an assigned patient
      preferences[i, 1] = preferences[i, j]
      j = j + 1
    }
  }
  
  current_assignment = t(preferences[, 1]) # Initialise to first choice
  names(current_assignment) = patient_names
  current_assignment = as.list(current_assignment)
  
  if (hpbm) {
    # Then remove the last appended "assigned_patient" because this is allowed
    # again
    assigned_patients = head(assigned_patients, -1)
  }
  
  # Only take the current preference of non-assigned patients
  current_assignment =
    current_assignment[!names(current_assignment) %in% assigned_patients]
  
  return(list("preferences" = preferences,
              "current_assignment" = current_assignment))
}

# Write the HPBM Algorithm
hpbm = function(circles, preferences, assigned_patients, patient_names,
                current_assignment, final_assignment, f, w, available_kidneys) {
  
  preferences_old = preferences
  
  # circles is a list of list with too long circles
  patients_in_circles = unique(names(unlist(circles)))
  candidates = patients_in_circles
  
  # Return here
  t = f[min(match(candidates, f), na.rm = TRUE)]
  p_star = current_assignment[as.character(t)]
  not_allowed = c(unlist(assigned_patients), p_star[[1]])
  
  while (TRUE) {
    p = preference_updater(preferences_old, not_allowed, patient_names,
                           hpbm = TRUE)
    preferences = p$preferences
    current_assignment = p$current_assignment
    
    p_prime = current_assignment[as.character(t)]
    
    if (p_prime == w) {
      # Reset preferences
      preferences = preferences_old
      
      # Continue with the next highest priority patient (remove t from
      # candidates)
      candidates = candidates[-1]
      t = f[min(match(candidates, f), na.rm = TRUE)]
      
    } else if (p_prime %in% available_kidneys) {
      # Remove p_prime from kidneys and add t
      available_kidneys = available_kidneys[-which(available_kidneys == p_prime)]
      available_kidneys = c(available_kidneys, t)
      
      final_assignment = append(final_assignment, p_prime)
      
      return(list("final_assignment" = final_assignment,
                  "available_kidneys" = available_kidneys,
                  "assigned_patients" = assigned_patients,
                  "preferences" = preferences))
      
    } else {
      # Find circles
      my_data = list("current_assignment" = current_assignment,
                     "w" = w)

      result = circle_finder(my_data$current_assignment, my_data$w)
      circles = result$circles
      
      if (t %in% unique(names(unlist(circles)))) {
        t_circles = circles[sapply(circles, function(x) {t %in% names(x)})]
        circle_lengths = sapply(t_circles, length)
        
        # And find circles that obey the capacity constraint
        correct_index = which(circle_lengths <= q)
        correct_circles = circles[correct_index]
        
        if (length(correct_circles) > 0) {
          final_assignment = circle_assigner(final_assignment, correct_circles)
          
          # And kick them out / update preferences
          assigned_patients = c(assigned_patients, names(unlist(correct_circles)))
          
          update = preference_updater(preferences, assigned_patients, patient_names)
          preferences = update$preferences
          current_assignment = update$current_assignment
          
          return(list("final_assignment" = final_assignment,
                      "available_kidneys" = available_kidneys, 
                      "assigned_patients" = assigned_patients,
                      "preferences" = preferences,
                      "current_assignment" = current_assignment))
        } else {
          not_allowed = c(not_allowed, p_prime)
          next
          # Go to step Y; namely stay with t and update preferences
          # I.e. go to beginning of while loop
        }
      } else {
        not_allowed = c(not_allowed, p_prime)
        next
        # Go to step Y; namely stay with t and update preferences
        # I.e. go to beginning of while loop
      }
    }
    
  }
  return(list("final_assignment" = final_assignment,
              "available_kidneys" = available_kidneys, 
              "assigned_patients" = assigned_patients,
              "preferences" = preferences))
}

combined = function(current_assignment, preferences, final_assignment,
                    assigned_patients, # Is this needed? (names(final_assignment))
                    patient_names, available_kidneys, f, w) {
  # Go here after HPBM is successful / Return to start
  result = circle_finder(current_assignment, w)
  circles = result$circles
  chains = result$chains
  
  while (length(circles) > 0) {
    # Then circles exist; calculate lengths of these circles...
    circle_lengths = sapply(circles, length)
    
    # And find circles that obey the capacity constraint
    correct_index = which(circle_lengths <= q)
    correct_circles = circles[correct_index]
  
    if (length(correct_circles) > 0) {
      # There are correct circles, assign these
      final_assignment = circle_assigner(final_assignment, correct_circles)
      
      # And kick them out / update preferences
      assigned_patients = c(assigned_patients, names(unlist(correct_circles)))
      update = preference_updater(preferences, assigned_patients, patient_names)
      preferences = update$preferences
      current_assignment = update$current_assignment
      
      # Update circles variable; remove assigned circles
      circles = circles[-correct_index]
      # TODO: Move into function of circle assigner? See also hpbm
      
    } else {
      # Then there are circles, but these are all too long -> apply HPBM
      hpbm_out = hpbm(circles, preferences, assigned_patients, patient_names,
            current_assignment, final_assignment, f, w, available_kidneys)
      
      return(list("current_assignment" = hpbm_out$current_assignment,
                  "preferences" = hpbm_out$preferences,
                  "final_assignment" = hpbm_out$final_assignment,
                  "assigned_patients" = hpbm_out$assigned_patients,
                  "available_kidneys" = hpbm_out$available_kidneys))
    }
  }
  
  # No cycles exist, so a w-chain must be used
  result = chain_assigner(final_assignment, chains, f, available_kidneys,
                          assigned_patients)
  final_assignment = result$final_assignment
  available_kidneys = result$available_kidneys
  assigned_patients = result$assigned_patients
  
  update = preference_updater(preferences, assigned_patients, patient_names)
  current_assignment = update$current_assignment
  
  return(list("current_assignment" = current_assignment,
              "preferences" = preferences,
              "final_assignment" = final_assignment,
              "assigned_patients" = assigned_patients,
              "available_kidneys" = available_kidneys))
}

###################### RUN FUNCTIONS ##################################
# data = import_data(file_location = "data/dataset7.xlsx")
# data = import_data(file_location = "data/test_mike2.xlsx") # TODO: remove when done
data = import_data(file_location = "data/test_steffie.xlsx") # TODO: remove when done

# Initialise kidneys: make patients "point" to currently "favourite" kidneys
p = preference_updater(data$preferences,
                       assigned_patients = c(),
                       patient_names = data$patient_names)
current_assignment = p$current_assignment
rm(p)

## Algorithm
# Start;
q = 3 # TODO: Unhardcode, function?

# Initialise list of input
outcome = list("current_assignment" = current_assignment,
               "preferences" = data$preferences,
               "final_assignment" = list(),
               "assigned_patients" = c(),
               "available_kidneys" = c(),
               "patient_names" = data$patient_names, 
               "f" = data$f,
               "w" = data$w)

while (length(outcome$final_assignment) != length(data$f)) {
  # Continue running the algorithm until everyone has been assigned
  #TODO: Sometimes duplicates in final_assignment. Mend the quickfix below
  outcome = combined(current_assignment = outcome$current_assignment,
                     preferences = outcome$preferences,
                     final_assignment = outcome$final_assignment,
                     assigned_patients = outcome$assigned_patients, # Is this needed? (e.g. use names(final_assignment) instead)
                     available_kidneys = outcome$available_kidneys,
                     patient_names = data$patient_names,
                     f = data$f,
                     w = data$w)
}

assignment = outcome$final_assignment[!duplicated(outcome$final_assignment)]
print(assignment)

