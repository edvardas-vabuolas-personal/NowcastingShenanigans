
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

  
  # Initiate data frame with 166 rows (1979Q1-2020Q1 has 166 quarters)
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
  
  
  # "Variable with missing values:  CPIH"       Data starts from 1990: No. of values:  122
  # "Variable with missing values:  CPIH_YTYPCT"      Data starts from 1991: No. of values:  118
  # "Variable with missing values:  EXCHER"       Data starts from 2007: No. of values:  54
  # "Variable with missing values:  PCOREH"       Data starts from 1996: No. of values:  98
  # "Variable with missing values:  PCOREH_YTYPCT"      Data starts from 1997: No. of values:  94
  # "Variable with missing values:  PMGSX"      Data starts from 1998: No. of values:  130
  # "Variable with missing values:  PMNW"       Data starts from 1998: No. of values:  130
  # "Variable with missing values:  PXGSX"      Data starts from 1998: No. of values:  130
  # "Variable with missing values:  PXNW"       Data starts from 1998: No. of values:  130
  # "Variable with missing values:  SHTGSVD"      Data starts from 1980: No. of values:  162
  
  # "Variable with missing values:  Short-term interest rates, Per cent per annum  No. of values:  420" (Starts at 1986)
}
