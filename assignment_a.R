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
  if (colnames(data)[1]!="Patient"){
    colnames(data)[1]<-"Patient"
  }
  
  
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
  current_assignment <- t(preferences[, 1]) # Initialise to first choice
  names(current_assignment) <- patient_names
  current_assignment <- as.list(current_assignment)
  
  # Initialise the final_assignment to 0
  final_assignment <- t(as.matrix(rep(0, number_of_patients)))
  names(final_assignment) <- patient_names
  final_assignment <- as.list(final_assignment) 
  
  # Initialise set of currently assigned kidneys
  assigned <- c()
  available_kidneys<-c()
  
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys"=available_kidneys))
}#return initial values



# Step 2
# a
# Search for cycle
circle_finder <- function(data){#notice we use circle instead of cycle, since cycle is a function in R
  f <- data$f
  w <- data$w
  preferences <- data$preferences
  current_assignment <- data$current_assignment
  final_assignment <- data$final_assignment
  assigned <- data$assigned
  available_kidneys<-data$available_kidneys
  
  rm(data) # Clean up; `data` is not needed in the rest of this function
  
  circle_found <- T # Initialise
  
  while (circle_found) { #as long as cycles exist in the current assignment
    new_assigned <- c() # Initialise
    no_circle_found_so_far <- c(w) # Initialise
    
    for (i in 1:length(f)) { #for every patient
      if (!f[i] %in% no_circle_found_so_far) {#check if the patient is already assigned, run if not assigned
        current_chain <- c() #create an empty chain
        j <- f[i]#becomes the current step
        
        while (!j %in% no_circle_found_so_far && !j %in% assigned) {#as long as j is not already assigned to something, continue
          current_chain <- append(current_chain, j)#append the current chain
          j <- current_assignment[[as.character(j)]]#assign new j
          if (j==w || j %in% no_circle_found_so_far) { #if j is in  a w chain or already assigned, stop
            no_circle_found_so_far <-
              append(no_circle_found_so_far, current_chain) #assign to no_circle_found_so_far, when a new patient point to something that's already checked, stop
          }
          
          if (j %in% current_chain) {# when j is in the current chain we have found a cycle
            circle <-
              current_chain[which(current_chain == j):length(current_chain)]#construct the cycle
            
            for (k in 1:length(circle)) {
              final_assignment[[as.character(circle[k])]] <- current_assignment[[as.character(circle[k])]]
            }#update the final result
            
            new_assigned <- append(new_assigned, circle)#append new_assigned, we need to update this in the current_assignment
            no_circle_found_so_far <-
              append(no_circle_found_so_far, current_chain)#when a patient points to the cycle, we don't need to look further for this chain.
          }
        }
      }
    }
    
    ## If circle, kick out all circles
    if (!is.null(new_assigned)) {#check if we have assigned anything
      assigned <- append(assigned, new_assigned)#append assigned patients
      selection <- c()
      
      # make this to also recognise random numbers
      for (k in 1:length(new_assigned)) {
        selection <-
          append(selection, which(
            as.numeric(names(current_assignment)) == new_assigned[k]))
      }#select patients that are assigned in this loop
      
      selection <- -selection #select all patients that are not assigned in this loop, with the minus sign
      current_assignment <- current_assignment[selection] #drop all assigned patients
      f <- f[selection] #drop all assigned patients
      
      if (!is.empty(current_assignment)){#check if their are any patients left
      # Reassign arrows and recheck circles
      for (k in 1:length(current_assignment)) {#update the current assignment of all remaining patients
        index.X <- as.numeric(names(current_assignment)[k])#only look to the remaining patients
        index.Y<-!preferences[index.X, ] %in% assigned[which( ! assigned %in% available_kidneys)]#not allowed to point to the assigned patients, except the kidneys that remain after the w chain
        current_assignment[k] <- 
          preferences[index.X, index.Y][1]#update the current_assignment
      }
      } else {
        circle_found <- F# if no patients are left, exit the loop
      }
    } else {
      circle_found <- F #if no cycles are found, exit the loop
      ## Check if there are pairs left
      ### Stop
      # Else, go to step 3
    }
  }
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys"=available_kidneys))
}#return updated data points

# Step 3
## Else, find w-chain according to (e)

w_finder <- function(data){
  f <- data$f
  w <- data$w
  preferences <- data$preferences
  current_assignment <- data$current_assignment
  final_assignment <- data$final_assignment
  assigned <- data$assigned
  available_kidneys<-data$available_kidneys
  
    rm(data) # Clean up; `data` is not needed in the rest of this function
  
    #initialize temporary data
    w_chain <- c()
    new_assigned<-c()
    Already_checked<- c(w)
    first_of_w_chain<-w
    for (i in 1:length(f)){#search the w_chain for all patients
      if (!f[i] %in% Already_checked) {#check if we have not yet found the current patient
        current_chain <- c()
        j <- f[i]#similar as in the circle finder, j is the current step in the chain
  
        while (!j %in% Already_checked && !j %in% assigned) {#as long as j is available run the while loop
          current_chain <- append(current_chain, j)#append the current chain with j
          j <- current_assignment[[as.character(j)]]#update j
          
          if (j==w || j %in% Already_checked) {#check if j is still avalable, otherwise update already checked and stop the loop
            Already_checked <-
              append(Already_checked, current_chain)
          }
          
          if(j %in% available_kidneys){#check if j is a remaining kidney, patient is already assigned, but kidney not yet(first entry of w chain with highest priority)
            index<-which(available_kidneys==j)
            available_kidneys[index]<-current_chain[1]#update the remaing kidneys
            for (k in 1:length(current_chain)) {#update final_assignment
              final_assignment[[as.character(current_chain[k])]] <- current_assignment[[as.character(current_chain[k])]]
            }
            new_assigned<-append(new_assigned,current_chain)#update the new_assigned entries
            Already_checked <-
              append(Already_checked, current_chain)#update already checked
          }
          
          if(j == first_of_w_chain){#when j is equal to the entry of the current w_chain with the highest priority, then we expand the w_chain
            w_chain<-append(current_chain, w_chain)
            if(w != first_of_w_chain){
            Already_checked <-
              append(Already_checked, current_chain)
            }
            first_of_w_chain<-w_chain[1]
          }
          }
        }
    }
    if (!is.null(w_chain)){#check if we have found a w_chain
    for (k in 1:length(w_chain)) {
      final_assignment[[as.character(w_chain[k])]] <- current_assignment[[as.character(w_chain[k])]]
    }#update final_assignment
    if (w!=first_of_w_chain){#if w is not equal to the first_of_w_chain
      available_kidneys<-append(available_kidneys, first_of_w_chain) #append the available kidneys
    }
    }
    new_assigned<-append(new_assigned,w_chain)#append the new_assigned
    
    if (!is.null(new_assigned)) {#check if the new_assigned are not empty
      assigned <- append(assigned, new_assigned)
      selection <- c()
      
      #throw out, all already assinged patients
      for (k in 1:length(new_assigned)) {
        selection <-
          append(selection, which(
            as.numeric(names(current_assignment)) == new_assigned[k]))
      }
      
      selection <- -selection
      current_assignment <- current_assignment[selection]
      f <- f[selection]
    }  
    
    
    for (k in 1:length(current_assignment)){#update preferences, patients can only point to the first kidney of the w_chain(s), not to the other patients
        index.X <- as.numeric(names(current_assignment)[k])
        index.Y<-!preferences[index.X, ] %in% assigned[which( ! assigned %in% available_kidneys)]
        current_assignment[k] <- 
        preferences[index.X, index.Y][1]
    }
    
    
  return(list("f" = f,
              "w" = w,
              "preferences" = preferences,
              "current_assignment" = current_assignment,
              "final_assignment" = final_assignment,
              "assigned" = assigned,
              "available_kidneys"=available_kidneys))
}#return updated data

Exercise.a<-function(location){
iterate_data <- import_data(file_location=location)#import data, using the function that is written above
f<-iterate_data$f #all patients that are currently assinged

while (!is.empty(f)) {#as long as patients remain unassigned
  ## Repeat 2 and 3
  iterate_data<- circle_finder(iterate_data)#search for cycles
  f<-iterate_data$f#update f
  if(!is.empty(f)){#check if patients are unassigned
    iterate_data<- w_finder(iterate_data) 
  }
  f<-iterate_data$f #update f
}
iterate_data$final_assignment[which(iterate_data$final_assignment==iterate_data$w)]<-"w"#update the output, for the waiting list
df.final<-data.frame(iterate_data$final_assignment)#create table
colnames(df.final)<-names(iterate_data$final_assignment) #assign the right colnames
return(list("final_assignment"=df.final, "remaining.kidneys"=iterate_data$available_kidneys))
}#return result
result<-Exercise.a(location="data/dataset7.xlsx")#run the algorithm