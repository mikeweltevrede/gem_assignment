#### Initialise document ####
#'Assignment Games and Economic Models - Question B
#'@author Steffie van Poppel, Mike Weltevrede, Joost Westland (Group 7)

# Clean environment
rm(list = ls())

# Activating packages
library(readxl)

#### Defining functions ####

#'Import and process data
#'
#'\code{import_data} reads data and does some basic cleaning and computations.
#'
#'@param \code{file_location} The path to the data to read in.
#'
#'@return The matrix with \code{preferences}, the list of \code{patient_names},
#'  a list \code{f} containing the priority ordering (which is computed as
#'  the ascending order of patient numbers), and the numeric encoding \code{w}
#'  for the waiting list (which is computed as the maximum patient number plus
#'  1).
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

#'Retains the unique entries in a named list
#'
#'\code{uniquefy_list}, given a named list, retains the unique entries. This is
#'different from the \code{unique()} function since that function only considers
#'(unnamed) values of a list when seaching for unique entries.
#'
#'@param \code{lst} The list to "uniquefy".
#'
#'@return The "uniquefied" list \code{list_unique}.
uniquefy_list = function(lst) {
  
  # Initialise
  list_unique = c()
  
  # Loop over the input list
  for (i in 1:length(lst)) {
    
    # Check if this entry is already in the list and, if not, append it
    if (!lst[i] %in% list_unique) {
      
      list_unique = c(list_unique, lst[i])
    }
  }
  
  return(list_unique)
}

#'Search for cycles
#'
#'\code{circle_chain_finder} finds circles and w-chains in the current
#'preferences of patients. Notice that we use the term circle instead of cycle,
#'since \code{cycle()} is a function in R.
#'
#'@param \code{current_assignment} List with the currently favourite preferences
#'@param \code{available_kidneys} List with available kidneys
#'@param \code{w} The numeric encoding for the waiting list
#'
#'@return A list of cycles in \code{circles} and w-chains in \code{chains}.
circle_chain_finder = function(current_assignment, available_kidneys, w){
  
  # Initialise sets of all circles and chains
  circles = list()
  chains = list()
  
  # Initialise an empty cycle and w-chain
  circle = c()
  w_chain = c()
  
  # Initialise patients that are already checked (so that we avoid finding a
  # cycle or chain that we have already found before)
  already_checked = c() 
  
  for (i in 1:length(current_assignment)) {
    
    # Initialise boolean to avoid assignment later in this function
    already_found = FALSE
    
    if (names(current_assignment)[i] %in% already_checked) {
      # This patient belongs to a cycle or chain that we already explored
      
      already_checked = append(already_checked, names(current_assignment)[i])
      next
    }

    # Start a chain
    current_chain = c(current_assignment[i])
    
    # Find the preferred kidney by this patient
    points_to = as.character(current_assignment[i])
    
    if (points_to == names(current_assignment)[i]) {
      # This patient points to themselves, assign this
      
      circles = c(circles, list(current_chain))
      already_checked = append(already_checked, unique(names(circle)))
      next
    }
    
    if (points_to == w) {
      # w-chain found
      
      chains = c(chains, list(current_chain))
      already_checked = append(already_checked, unique(names(current_chain)))
      next
    }
    
    # Set the next patient to the one corresponding to the preferred kidney...
    next_patient = current_assignment[points_to]
    
    # And make the list unique, just to be sure (cheap operation)
    current_chain = uniquefy_list(append(current_chain, next_patient))
    
    # Continue this loop "indefinitely" (if-statements with break calls make
    # sure that the loop gets broken)
    while (TRUE) {
      
      if (next_patient %in% names(unlist(circles))) {
        # This will lead to a circle already found before: skip
        
        checked = append(current_chain, next_patient)
        already_checked = append(already_checked, unique(names(checked)))
        already_found = TRUE
        break
        
      } else if (next_patient %in% names(unlist(chains))) {
        # This will extend an existing chain or will be a new branch.
        
        for (i in 1:length(chains)) {
          chain = chains[[i]]
          
          if (next_patient %in% names(chain)) {
            selected_chain = chain
            which_chain = i
            break
          }
        }
        
        ind = which(next_patient == names(selected_chain))
        new_chain = uniquefy_list(append(next_patient,
                                         selected_chain[
                                           ind:length(selected_chain)]))
        
        if (ind == 1) {
          # This extends an existing chain. Since we want to keep the longest
          # chain (containing the highest-priority patient), we delete the
          # original chain and add this one instead
          
          chains[[which_chain]] = new_chain
        } else {
          chains = append(chains, list(new_chain))
        }
        
        checked = append(current_chain, next_patient)
        already_checked = append(already_checked, unique(names(checked)))
        already_found = TRUE
        break
        
      } else if (next_patient %in% names(current_chain)) {
        # Circle found
        
        # Check where it is linked to, don't keep the patients before this
        # (we are only interested in cycles, and not loose chains appended to
        # a cycle - we know that these will not form a cycle anyway)
        starting_index = which(next_patient == names(current_chain))
        
        circle = append(current_chain[starting_index:length(current_chain)],
          next_patient)
        
        already_checked = append(already_checked, unique(names(circle)))
        break
      } else if (next_patient == w) {
        # w-chain found
        w_chain = append(current_chain, next_patient)
        
        already_checked = append(already_checked, unique(names(w_chain)))
        break
      } else if (next_patient %in% available_kidneys) {
        # This is a path that can safely be assigned, treat this as a w-chain
        w_chain = append(current_chain, next_patient)
        
        already_checked = append(already_checked, unique(names(w_chain)))
        break
      } else {
        # No circle or w-chain is found with this iteration, so continue by
        # extending the chain with the patient that is being pointed to.
        current_chain = uniquefy_list(append(current_chain, next_patient))
        
        # Find patient that this next patient prefers
        next_patient = current_assignment[as.character(next_patient)]
      }
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
  
  # Return the lists of cycles and chains
  return(list("circles" = circles,
              "chains" = chains))
}

#'Assigns cycles
#'
#'\code{circle_assigner} assigns cycles from \code{circles} by placing the
#'corresponding assignments in \code{final_assignment}
#'
#'@param \code{final_assignment} List with currently final assignment
#'@param \code{circles} List of circles
#'
#'@return Updated \code{final_assignment} list
circle_assigner = function(final_assignment = list(), circles) {
  
  for (circle in circles) {
    final_assignment = append(final_assignment, circle)
  }
  
  return(final_assignment)
}

#'Assigns chains using chain selection rule (e)
#'
#'\code{chain_assigner} assigns chains from \code{chains} by placing the
#'corresponding assignments in \code{final_assignment}. It takes into account
#'chain selection rule (e), which states that the chain containing the highest
#'priority patient should be assigned. When that patient appears in multiple
#'chains, we take the longest chain, breaking the tie with the first one found.
#'
#'@param \code{final_assignment} List with currently final assignment
#'@param \code{chains} List of chains
#'@param \code{f} List with the priority ordering
#'@param \code{available_kidneys} List with available kidneys
#'@param \code{assigned_patients} List with already assigned patients
#'
#'@return A list with updated \code{final_assignment}, \code{available_kidneys},
#'  and \code{assigned_patients}.
chain_assigner = function(final_assignment = list(), chains, f,
                          available_kidneys, assigned_patients) {
  
  in_chain = names(unlist(chains))
  
  # Find the person highest on the priority list
  index = which.min(match(in_chain, f))
  highest_priority = in_chain[index]
  
  # Find chains which have highest_priority in them
  candidate_chains = which(sapply(chains,
                                  function(x) {highest_priority %in% names(x)}))
  
  # Find which of these chains is the longest. If there is a tie, we take the
  # first one found
  longest_candidates = which.max(sapply(chains[candidate_chains], length))
  selected_chain = chains[longest_candidates]
  
  # Add first kidney in chain to available kidneys
  available_kidneys = c(available_kidneys, names(selected_chain)[1])
  
  # Update final_assignment and assigned_patients accordingly
  final_assignment = append(final_assignment, selected_chain)
  assigned_patients = c(assigned_patients, names(unlist(selected_chain)))
  
  return(list("final_assignment" = final_assignment,
              "available_kidneys" = available_kidneys,
              "assigned_patients" = assigned_patients))
}

#'Updates preferences
#'
#'\code{preference_updater} updates preferences by not allowing patients to have
#'an already assigned patient as their current preference. If \code{HPBM} is
#'\code{TRUE}, then only the preference for the \code{hpbm_patient} should be
#'updated. 
#'
#'@param \code{preferences} Matrix with all preferences
#'@param \code{assigned_patients} List with already assigned patients
#'@param \code{patient_names} List with patient names (for generality)
#'@param \code{available_kidneys} List with available kidneys
#'@param \code{hpbm} Boolean, will the function be run in the HPBM algorithm? 
#'@param \code{hpbm_patient} String specifying which patient the HPBM is
#'  currently considering
#'
#'@return A list with updated \code{preferences} and corresponding
#'  \code{current_assignment}.
preference_updater = function(preferences, assigned_patients, patient_names,
                              available_kidneys, hpbm = FALSE,
                              hpbm_patient=c()) {

  if (hpbm) {
    
    current_assignment = t(preferences[, 1])
    names(current_assignment) = patient_names
    current_assignment = as.list(current_assignment)
    current_assignment[[as.character(hpbm_patient)]] =
      as.numeric(preferences[hpbm_patient, which(
        !preferences[hpbm_patient, ] %in% assigned_patients)][1])
  } else {
    for (i in 1:dim(preferences)[1]) {
      if (i %in% assigned_patients) {
        # Don't update preferences for assigned patients
        next
      }
      
      # Start with their 2nd highest preference
      j = 2
      while (preferences[i, 1] %in% assigned_patients) {
        # Patient cannot have their next best preference be an assigned
        # patient...
        
        if (preferences[i, 1] %in% available_kidneys) {
          # Unless that kidney is available!
          break
        } else {
          preferences[i, 1] = preferences[i, j]
          j = j + 1
        }
      }
    }
    
    # Initialise to current first choice
    current_assignment = t(preferences[, 1])
    names(current_assignment) = patient_names
    current_assignment = as.list(current_assignment)
    
    # Only take the current preference of non-assigned patients
    current_assignment = current_assignment[
      !names(current_assignment) %in% assigned_patients]
    
  }
  
  return(list("preferences" = preferences,
              "current_assignment" = current_assignment))
}

#'Runs the HPBM algorithm
#'
#'\code{hpbm} runs the Highest-Priority Breaking Method (HPBM) for TTCC. In
#'light of practical reasons, it can be so that cycles cannot be larger than
#'some capacity. For example, there are not enough surgeons available. To
#'circumvent this problem, we propose this method. In short, when all found
#'cycles are too long, the HPBM explores the preferences of the patient with the
#'highest priority in those cycles until a proper match is found. For more
#'details, please read our report. 
#'
#'@param \code{circles} List of circles, all exceeding some capacity q
#'@param \code{preferences} Matrix with all preferences
#'@param \code{assigned_patients} List with already assigned patients
#'@param \code{patient_names} List with patient names (for generality)
#'@param \code{current_assignment} List with the currently favourite preferences
#'@param \code{final_assignment} List with currently final assignment
#'@param \code{f} List with the priority ordering
#'@param \code{w} The numeric encoding for the waiting list
#'@param \code{available_kidneys} List with available kidneys
#'@param \code{q} The capacity constraint for length of a cycle
#'
#'@return A list with updated \code{final_assignment},\code{available_kidneys},
#'  \code{assigned_patients}, \code{preferences}, and \code{current_assignment}.
hpbm = function(circles, preferences, assigned_patients, patient_names,
                current_assignment, final_assignment, f, w, available_kidneys,
                q) {
  
  # Save preferences in a different variable to retrieve original preferences
  # later
  preferences_original = preferences
  
  # Initialise list of all patients in these cycles
  candidates = unique(names(unlist(circles)))
  
  # Find highest-priority patient in cycles and their currently favourite kidney
  t = f[min(match(candidates, f), na.rm = TRUE)]
  p_star = current_assignment[as.character(t)]
  
  # Initialise vector of kidneys that this patient is not allowed to receive
  not_allowed = c(unlist(assigned_patients), p_star[[1]])
  
  while (TRUE) {
    # Update preferences, taking into account that the patient is not allowed
    # to receive the kidneys in not_allowed
    p = preference_updater(preferences_original, not_allowed, patient_names,
                           available_kidneys, hpbm = TRUE, hpbm_patient = t)
    preferences = p$preferences
    current_assignment = p$current_assignment
    
    # Find the next favourite kidney of t
    p_prime = current_assignment[as.character(t)]
    
    if (p_prime == w) {
      # We do not want to assign someone to the waiting list via the HPBM method
      
      # Reset preferences to original...
      preferences = preferences_original
      
      # and continue with the highest-priority patient apart from t (so we need
      # to remove t from the possible candidates)
      candidates = candidates[-1]
      
      # Find next patient and their currently favourite kidney
      t = f[min(match(as.numeric(candidates), f), na.rm = TRUE)]
      p_star = current_assignment[as.character(t)]
      not_allowed = c(unlist(assigned_patients), p_star[[1]])
      
    } else if (p_prime %in% available_kidneys) {
      # This is allowed: we will assign this kidney to this person
      
      # Remove p_prime from kidneys and add t
      available_kidneys = available_kidneys[-which(
        available_kidneys == p_prime)]
      
      final_assignment = append(final_assignment, p_prime)
      
      return(list("final_assignment" = final_assignment,
                  "available_kidneys" = c(available_kidneys, t),
                  "assigned_patients" = c(assigned_patients, t),
                  "preferences" = preferences,
                  "current_assignment" = current_assignment))
      
    } else {
      # Try to find circles

      result = circle_chain_finder(current_assignment, available_kidneys, w)
      circles = result$circles
      
      if (t %in% unique(names(unlist(circles)))) {
        # Only consider cycles when t is in them
        t_circles = circles[sapply(circles, function(x) {t %in% names(x)})]
        circle_lengths = sapply(t_circles, length)
        
        # Find circles that obey the capacity constraint
        correct_index = which(circle_lengths <= q)
        correct_circles = circles[correct_index]
        
        if (length(correct_circles) > 0) {
          final_assignment = circle_assigner(final_assignment, correct_circles)
          
          # And kick them out / update preferences
          assigned_patients = c(assigned_patients, names(unlist(correct_circles)))
          
          update = preference_updater(preferences, assigned_patients,
                                      patient_names, available_kidneys)
          preferences = update$preferences
          current_assignment = update$current_assignment
          
          return(list("final_assignment" = final_assignment,
                      "available_kidneys" = available_kidneys, 
                      "assigned_patients" = assigned_patients,
                      "preferences" = preferences,
                      "current_assignment" = current_assignment))
        } else {
          # Go to step Y; namely stay with t and update preferences
          # I.e. go to beginning of while loop
          
          not_allowed = c(not_allowed, p_prime[[as.character(t)]])
          next
        }
      } else {
        # Go to step Y; namely stay with t and update preferences
        # I.e. go to beginning of while loop
        
        not_allowed = c(not_allowed, p_prime[[as.character(t)]])
        next
      }
    }
  }
  
  # Return variables
  return(list("final_assignment" = final_assignment,
              "available_kidneys" = available_kidneys, 
              "assigned_patients" = assigned_patients,
              "preferences" = preferences,
              "current_assignment" = current_assignment))
}

#'Runs the TTCC algorithm with HPBM method
#'
#'\code{combined} runs the TTCC algorithm with Highest-Priority Breaking Method
#'(HPBM) when all available cycles exceed the capacity constraint \code{q}.
#'
#'@param \code{current_assignment} List with the currently favourite preferences
#'@param \code{preferences} Matrix with all preferences
#'@param \code{final_assignment} List with currently final assignment
#'@param \code{assigned_patients} List with already assigned patients
#'@param \code{patient_names} List with patient names (for generality)
#'@param \code{available_kidneys} List with available kidneys
#'@param \code{f} List with the priority ordering
#'@param \code{w} The numeric encoding for the waiting list
#'@param \code{q} The capacity constraint for length of a cycle
#'
#'@return A list with updated \code{current_assignment}, \code{preferences},
#'  \code{final_assignment}, \code{assigned_patients},
#'  and \code{available_kidneys}.
combined = function(current_assignment, preferences, final_assignment,
                    assigned_patients, patient_names, available_kidneys, f, w,
                    q) {
  
  result = circle_chain_finder(current_assignment, available_kidneys, w)
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
      update = preference_updater(preferences, assigned_patients, patient_names,
                                  available_kidneys)
      preferences = update$preferences
      current_assignment = update$current_assignment
      
      # Update circles variable; remove assigned circles
      circles = circles[-correct_index]
      
    } else {
      # Then there are circles, but these are all too long -> apply HPBM
      hpbm_out = hpbm(circles, preferences, assigned_patients, patient_names,
            current_assignment, final_assignment, f, w, available_kidneys, q)
      
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
  
  update = preference_updater(preferences, assigned_patients, patient_names,
                              available_kidneys)
  current_assignment = update$current_assignment
  
  return(list("current_assignment" = current_assignment,
              "preferences" = preferences,
              "final_assignment" = final_assignment,
              "assigned_patients" = assigned_patients,
              "available_kidneys" = available_kidneys))
}

#### RUN FUNCTIONS ####
data = import_data(file_location = "data/dataset7.xlsx")

# Initialise kidneys: make patients "point" to currently "favourite" kidneys
p = preference_updater(data$preferences,
                       assigned_patients = c(),
                       patient_names = data$patient_names,
                       available_kidneys = c())
current_assignment = p$current_assignment

# Clean up; `p` is not needed anymore in the rest of this function and will only
# take up workspace memort
rm(p)

# Initialise list of input
outcome = list("current_assignment" = current_assignment,
               "preferences" = data$preferences,
               "final_assignment" = list(),
               "assigned_patients" = c(),
               "available_kidneys" = c(),
               "patient_names" = data$patient_names, 
               "f" = data$f,
               "w" = data$w)

number_of_runs = 1
while (length(outcome$final_assignment) != length(data$f)) {
  # Continue running the algorithm until everyone has been assigned
  
  print(paste("Current run:", number_of_runs))
  
  outcome = combined(current_assignment = outcome$current_assignment,
                     preferences = outcome$preferences,
                     final_assignment = outcome$final_assignment,
                     assigned_patients = outcome$assigned_patients,
                     available_kidneys = outcome$available_kidneys,
                     patient_names = data$patient_names,
                     f = data$f,
                     w = data$w,
                     q = 3)
  
  # Remove duplicates from the final_assignment
  outcome$final_assignment = uniquefy_list(outcome$final_assignment)
  
  # Print the final assignment to retain output when the code, unfortunately,
  # does not finish
  print("Current assignment:")
  print(outcome$final_assignment)
  print("----------")
  
  number_of_runs = number_of_runs + 1
}

assignment = uniquefy_list(outcome$final_assignment)
print(assignment)

