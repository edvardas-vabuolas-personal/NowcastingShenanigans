######### packages ###############

######### Data import ############
# OECD does not let download all data in a single file, so we download two files and merge them
uk_data1 <- read.csv(file = 'economic_outlook1.csv', head = T)
uk_data2 <- read.csv(file = 'economic_outlook2.csv', head = T)
uk_data <- merge(uk_data1, uk_data2, all = TRUE)

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

# Initiate data frame with 165 rows (1979Q1-2019Q4 has 165 quarters)
df <- as.data.frame(matrix(0, nrow = 166, ncol = 0))
# Iterate over dictionary keys
for (i in ls(vars_vals)) {
  # Ignore variable if there are missing values
  if (length(vars_vals[[i]]) == 166) {
    # Append data frame with a new column "i" with it's associated values as rows
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