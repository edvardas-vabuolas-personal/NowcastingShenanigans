######### packages ###############

######### Data import ############
uk_data <- read.csv(file = 'economic_outlook.csv')

##### Creating a data frame ######
# OECD CSV export returns variable names as rows, we need them as columns

# Initiate empty list
vars_vals <- list()
for (i in 1:nrow(uk_data)) {
  # Pre-define variable name for optimization
  var_name <- uk_data[i, "VARIABLE"]
  
  # If variable is not in the list - create a new dict key with initial value
  if (length(vars_vals[[var_name]]) == 0) {
    vars_vals[[var_name]] <- list(uk_data[i, "Value"])
  } else {
    # If variable is in the list - append dict value with new value
    vars_vals[[var_name]] <-
      append(vars_vals[[var_name]], uk_data[i, "Value"])
  }
}

# Initiate data frame with 165 rows (1979-2019 has 165 quarters)
df <- as.data.frame(matrix(0, nrow = 165, ncol = 0))

# Iterate over dictionary keys
for (i in ls(vars_vals)) {
  # Ignore variable if there are missing values
  if (length(vars_vals[[i]]) == 165) {
    # Append data frame with a new column "i" with it's associated values as rows
    df[[i]] <- vars_vals[[i]]
  } else {
    print(paste("Variable with missing values: ", i))
  }
}


# "Variable with missing values:  CPIH"       Data starts from 1990
# "Variable with missing values:  CPIH_YTYPCT"      Data starts from 1991
# "Variable with missing values:  EXCHER"       Data starts from 2007
# "Variable with missing values:  PCOREH"       Data starts from 1996
# "Variable with missing values:  PCOREH_YTYPCT"      Data starts from 1997
# "Variable with missing values:  PMGSX"      Data starts from 1998
# "Variable with missing values:  PMNW"       Data starts from 1998
# "Variable with missing values:  PXGSX"      Data starts from 1998
# "Variable with missing values:  PXNW"       Data starts from 1998
# "Variable with missing values:  SHTGSVD"      Data starts from 1980