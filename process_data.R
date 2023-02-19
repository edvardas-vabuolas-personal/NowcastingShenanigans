

process_data <- function(csv_file, no_of_rows, var_col_name) {
  ##### Creating a data frame ######
  # OECD CSV export returns variable names as rows, we need them as columns
  
  # Initiate empty list
  vars_vals <- list()
  for (i in 1:nrow(csv_file)) {
    # Pre-define variable name for optimization
    var_name <- csv_file[i, var_col_name]
    # If variable is not in the list - create a new dict key with initial value
    if (length(vars_vals[[var_name]]) == 0) {
      vars_vals[[var_name]] <- list(csv_file[i, "Value"])
    } else {
      # If variable is in the list - append dict value with new value
      vars_vals[[var_name]] <-
        append(vars_vals[[var_name]], csv_file[i, "Value"])
    }
  }
  
  
  # Initiate data frame with the expected number of rows (time periods)
  df <- as.data.frame(matrix(0, nrow = no_of_rows, ncol = 0))
  # Iterate over dictionary keys
  for (i in ls(vars_vals)) {
    # Ignore variable if there are missing values
    if (length(vars_vals[[i]]) == no_of_rows) {
      # Append data frame with a new column "i" with it's associated values as rows
      df[['Time']] <- unique(csv_file$TIME)
      df[[i]] <- vars_vals[[i]]
    } else {
      print(paste(
        "Variable with missing values: ",
        i,
        " No. of values: ",
        length(vars_vals[[i]])
      ))
    }
  }
  return(df)
}
