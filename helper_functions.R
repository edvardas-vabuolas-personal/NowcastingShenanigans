source("packages_manager.R")

get_seeds <- function() {
  seeds <- vector(mode = "list", length = 88)
  for (i in 1:527) {
    seeds[[i]] <- sample.int(1000, 15)
  }
  return(seeds)
}

get_intervals <- function() {
  INTERVALS <- hashmap()
  params_2010 = c(
    dataset_end_date = "2010-12-01",
    train_end_date = "2005-12-01",
    test_start_date = "2006-01-01",
    initial_window = 200
  )
  params_2019 = c(
    dataset_end_date = "2019-12-01",
    train_end_date = "2015-12-01",
    test_start_date = "2016-01-01",
    initial_window = 310
  )
  params_2022 = c(
    dataset_end_date = FALSE,
    train_end_date = "2015-12-01",
    test_start_date = "2016-01-01",
    initial_window = 310
  )
  INTERVALS[[2010]] <- params_2010
  INTERVALS[[2019]] <- params_2019
  INTERVALS[[2022]] <- params_2022
  INTERVALS[["2010.break_points"]] <- c(217)
  INTERVALS[["2019.break_points"]] <- c(217)
  INTERVALS[["2022.break_points"]] <- c(217, 361)
  
  return(INTERVALS)
}

export_latex <- function(type, name, year = "", object, width = 7, height = 6, TEX=FALSE) {
  # type should be "plot" or "table"
  # name should be a string, normally a prefix (i.e. ar, var, ml)
  # object should be a ggplot object for plots or a dataframe for tables
  # year = "" For cases when plot/table (the object) is not year-specific
  if (TEX == TRUE) {
    if (type == "plot") {
      tikz(
        paste0("./output/", name, "_plot_", year, ".tex"),
        width = width,
        height = height
      )
      plot(object)
      dev.off()
    } else if (type == "table") {
      print(
        xtable(
          object,
          caption = paste0(name, year, "renameme"),
          label = paste0("tab:", name, year, "renameme")
        ),
        include.rownames = FALSE,
        file = paste0("./output/", name, "_table_", year, ".tex")
      )
    }
  }
}

calculate_msfe <- function(predictions,  oos) {
  # MSFE = SUM( (Yhat - OOS)^2 ) / No. of OOS
  squared_diff <- (as.numeric(predictions) - oos)^2
  sum_squared_diff <- sum(squared_diff)
  msfe <- sum_squared_diff / length(oos)
  return(msfe)
}

lag_data <- function(data, lag) {
  # The purpose of this is to lag explanatory variables
  # i.e. Lag = 2 would be
  # Dependent variable at t+1   Explanatory vars at t and t-1
  # GDP                         L1GDP   L2GDP   ...
  # 0.9                         NA      NA      ...
  # 0.4                         0.9     NA      ...
  # 0.2                         0.4     0.9     ...
  # 0.5                         0.2     0.4     ...
  # ...                         ...     ...     ...
  
  if (lag < 1) {
    return(data)
  }
  
  data_to_lag <- data
  
  # Keep date and t+1 GDP
  data <- data[, c("Date", "GDP")]
  
  # Add t, t-1, ..., t-lag explanatory variables to 'data' variable
  for (lag in 1:lags) {
    lagged_data <- data_to_lag
    
    # -c(1) because we don't want to lag 'Date' column
    lagged_data[, -c(1)] <- lag(data_to_lag[, -c(1)], n=lag)
    
    # The following is just for renaming columns
    col_index = 1
    for (col_name in colnames(lagged_data)) {
      if (col_name != 'Date') {
        names(lagged_data)[col_index] <- glue("L{lag}{col_name}")
      }
      col_index = col_index + 1
    }
    
    # Add t, t-1, ..., t-lag explanatory variables to 'data' variable
    data = merge(data, lagged_data, by = "Date", all = FALSE)
  }
  
  # First and last rows now contain NA values (because we lagged variables)
  data <- na.omit(data)
  
  return(data)
}
