# Assignment Games and Economic Models
# Group 7
# With chain selection (e)
#### Clean environment
rm()

#### installing packages ####
library(readxl)
library(bazar)

#### importing the data ####
import_data <- function(file_location) {
  data <- readxl::read_excel(file_location, .name_repair = "minimal")
  colnames(data)[1] <- "Patient"
  
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
  current_assignment <- t(preferences[, 1]) # Initialise to first choice
  names(current_assignment) <- patient_names
  current_assignment <- as.list(current_assignment)
  
  # Initialise the final_assignment to 0
  # TODO: Test if this can be done easier
  final_assignment <- t(as.matrix(rep(0, number_of_patients)))
  names(final_assignment) <- patient_names
  final_assignment <- as.list(final_assignment) 
  
  # Initialise set of currently assigned kidneys
  assigned <- c()
  available_kidneys <- c()
  
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys" = available_kidneys))
}

# Step 2
# a
# Check if circle
circle_finder <- function(data){
  f <- data$f
  w <- data$w
  preferences <- data$preferences
  current_assignment <- data$current_assignment
  final_assignment <- data$final_assignment
  assigned <- data$assigned
  available_kidneys <- data$available_kidneys
  
  rm(data) # Clean up; `data` is not needed in the rest of this function
  
  circle_found <- T # Initialise
  
  while (circle_found) {
    new_assigned <- c()
    no_circle_found_so_far <- c(w)
    
    for (i in 1:length(f)) {
      if (!f[i] %in% no_circle_found_so_far) {
        current_chain <- c()
        j <- f[i]
        
        while (!j %in% no_circle_found_so_far && !j %in% assigned) {
          current_chain <- append(current_chain, j)
          j <- current_assignment[[as.character(j)]]
          if (j == w || j %in% no_circle_found_so_far) {
            no_circle_found_so_far <-
              append(no_circle_found_so_far, current_chain)
          }
          
          if (j %in% current_chain) {
            circle <-
              current_chain[which(current_chain == j):length(current_chain)]
            
            for (k in 1:length(circle)) {
              final_assignment[[as.character(circle[k])]] <-
                current_assignment[[as.character(circle[k])]]
            }
            
            new_assigned <- append(new_assigned, circle)
            no_circle_found_so_far <-
              append(no_circle_found_so_far, current_chain)
          }
        }
      }
    }
    
    ## If circle, kick out all circles
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
      
      if (!bazar::is.empty(current_assignment)) {
        # Reassign arrows and recheck circles
        for (k in 1:length(current_assignment)) {
          index.X <- as.numeric(names(current_assignment)[k])
          index.Y <- !preferences[index.X, ] %in% assigned[which(!assigned %in% available_kidneys)]
          current_assignment[k] <- 
            preferences[index.X, index.Y][1]
        }
      } else {
        circle_found <- F
      }
    } else {
      circle_found <- F
    }
  } ## Else, go to step 3
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys" = available_kidneys))
}

# Step 3
## Check if there are pairs left
### Stop
## Else, find w-chain according to (e)
## Repeat 2 and 3
w_finder <- function(data){
  f <- data$f
  w <- data$w
  preferences <- data$preferences
  current_assignment <- data$current_assignment
  final_assignment <- data$final_assignment
  assigned <- data$assigned
  available_kidneys <- data$available_kidneys
  
  rm(data) # Clean up; `data` is not needed in the rest of this function
  
  #search the w_chain
  w_chain <- c()
  new_assigned <- c()
  Already_checked <- c(w)
  first_of_w_chain <- w
  for (i in 1:length(f)) {
    if (!f[i] %in% Already_checked) {
      current_chain <- c()
      j <- f[i]
      
      while (!j %in% Already_checked && !j %in% assigned) {
        current_chain <- append(current_chain, j)
        j <- current_assignment[[as.character(j)]]
        
        if (j == w || j %in% Already_checked) {
          Already_checked <-
            append(Already_checked, current_chain)
        }
        
        if (j %in% available_kidneys) {
          index <- which(available_kidneys == j)
          available_kidneys[index] <- current_chain[1]
          for (k in 1:length(current_chain)) {
            final_assignment[[as.character(current_chain[k])]] <-
              current_assignment[[as.character(current_chain[k])]]
          }
          new_assigned <- append(new_assigned,current_chain)
          Already_checked <- append(Already_checked, current_chain)
        }
        
        if (j == first_of_w_chain) {
          w_chain <- append(current_chain, w_chain)
          if (w != first_of_w_chain) {
            Already_checked <- append(Already_checked, current_chain)
          }
          first_of_w_chain <- w_chain[1]
        }
      }
    }
  }
  if (!is.null(w_chain)) {
    for (k in 1:length(w_chain)) {
      final_assignment[[as.character(w_chain[k])]] <-
        current_assignment[[as.character(w_chain[k])]]
    }
    if (w != first_of_w_chain) {
      available_kidneys <- append(available_kidneys, first_of_w_chain) 
    }
  }
  new_assigned <- append(new_assigned,w_chain)
  
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
  }  
  
  
  for (k in 1:length(current_assignment)) {
    # TODO: This can be neater (repeated indices)
    index.X <- as.numeric(names(current_assignment)[k])
    index.Y <- !preferences[index.X, ] %in% assigned[
      which(!assigned %in% available_kidneys)]
    current_assignment[k] <- 
      preferences[index.X, index.Y][1]
  }
  
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys" = available_kidneys))
}

Exercise.a <- function(location){
  iterate_data <- import_data(file_location = location)
  f <- iterate_data$f

  while (!bazar::is.empty(f)) {
    iterate_data <- circle_finder(iterate_data)
    f <- iterate_data$f
    
    while (!bazar::is.empty(f)) {
      iterate_data <- circle_finder(iterate_data)
      f <- iterate_data$f
      if (!bazar::is.empty(f)) {
        iterate_data <- w_finder(iterate_data) 
      }
      f <- iterate_data$f
    }
    iterate_data$final_assignment[
      which(iterate_data$final_assignment == iterate_data$w)] <- "w"
    df.final <- data.frame(iterate_data$final_assignment)
    colnames(df.final) <- names(iterate_data$final_assignment)
    return(list("final_assignment" = df.final,
                "remaining.kidneys" = iterate_data$available_kidneys))
  }
  
  iterate_data$final_assignment[
    which(iterate_data$final_assignment == iterate_data$w)] <- "w"
  df.final <- data.frame(iterate_data$final_assignment)
  colnames(df.final) <- names(iterate_data$final_assignment)
  return(list("final_assignment" = df.final,
              "remaining.kidneys" = iterate_data$available_kidneys))
}

result <- Exercise.a(location = "data/dataset7.xlsx")