w_finder = function(f, w, preferences, current_assignment, final_assignment,
                     assigned, available_kidneys, patient_names){
  f_old = f
  f = setdiff(f,assigned)
  #search the w_chain
  w_chain = c()
  new_assigned = c()
  Already_checked = c(w)
  first_of_w_chain = w
  for (i in 1:length(f)) {
    if (!f[i] %in% Already_checked) {
      current_chain = c()
      j = f[i]

      while (!j %in% Already_checked && !j %in% assigned) {
        current_chain = append(current_chain, j)
        j = current_assignment[[as.character(j)]]

        if (j == w || j %in% Already_checked) {
          Already_checked = append(Already_checked, current_chain)
        }

        if (j %in% available_kidneys) {
          index = which(available_kidneys == j)
          available_kidneys[index] = current_chain[1]
          for (k in 1:length(current_chain)) {
            final_assignment[[
              as.character(current_chain[k])]] = current_assignment[[
                as.character(current_chain[k])]]
          }
          new_assigned = append(new_assigned,current_chain)
          Already_checked = append(Already_checked, current_chain)
        }

        if (j == first_of_w_chain) {
          w_chain = append(current_chain, w_chain)
          if (w != first_of_w_chain) {
            Already_checked =
              append(Already_checked, current_chain)
          }
          first_of_w_chain = w_chain[1]
        }
      }
    }
  }
  if (!is.null(w_chain)) {
    for (k in 1:length(w_chain)) {
      final_assignment[[
        as.character(w_chain[k])]] = current_assignment[[
          as.character(w_chain[k])]]
    }
    if (w != first_of_w_chain) {
      available_kidneys = append(available_kidneys, first_of_w_chain)
    }
  }
  new_assigned = append(new_assigned,w_chain)

  if (!is.null(new_assigned)) {
    assigned = append(assigned, new_assigned)
    selection = c()

    # TODO: This currently assumes that the patients are 1:end;
    # make this to also recognise random numbers
    for (k in 1:length(new_assigned)) {
      selection =
        append(selection, which(
          as.numeric(names(current_assignment)) == new_assigned[k]))
    }

    selection = -selection # What does this do?
    current_assignment = current_assignment[selection]
    f = f[selection]
  }


  for (k in 1:length(current_assignment)) {
    # TODO: This can be neater (repeated indices)
    index.X = as.numeric(names(current_assignment)[k])
    index.Y = !preferences[index.X, ] %in% assigned[
      which(!assigned %in% available_kidneys)]
    current_assignment[k] = preferences[index.X, index.Y][1]
  }


  return(list("final_assignment" = final_assignment,
              "available_kidneys" = available_kidneys,
              "assigned_patients" = assigned,
              "preferences" = preferences,
              "patient_names" = patient_names,
              "f" = f_old,
              "w" = w))
}