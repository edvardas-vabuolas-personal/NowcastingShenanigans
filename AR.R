source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

# Set this to TRUE for LaTeX exports.
# Note: VERY SLOW and overrides files in output
TEX <- FALSE

# Set this to TRUE to include structural breaks (dummy variables)
STR_BREAKS <- TRUE

# Purely technical. We use STR_B string for file names.
STR_B <- if (STR_BREAKS == TRUE) "w_str_b" else "wout_str_b"

# This loads a dictionary (a hashmap) with intervals and their properties
# such as dataset end date, train end date, test start date etc.
INTERVALS <- get_intervals()

# Iterate through every interval
for (year in c(2010, 2019, 2022)) {
  # Retrieve interval-related parameters
  dataset_end_date <- as.character(INTERVALS[[year]]["dataset_end_date"])
  train_end_date <- as.character(INTERVALS[[year]]["train_end_date"])
  test_start_date <- as.character(INTERVALS[[year]]["test_start_date"])
  structural_breakpoints <- as.list(INTERVALS[[(paste0(year, ".break_points"))]])

  # Reset the list to empty if STR_BREAKS == FALSE
  structural_breakpoints <- if (STR_BREAKS == TRUE) structural_breakpoints else c()

  # Load and non-interpolated dataset. See load_data.R.
  data <- load_data(
    dataset_end_date = dataset_end_date
  )

  columns <- c(1, 2)
  if (STR_BREAKS == TRUE) {
    # Add structural breaks based on interval parameter on structural breaks
    for (i in seq_along(structural_breakpoints)) {
      data[, paste0("Break_", i)] <- ifelse(seq_len(nrow(data)) < structural_breakpoints[i], 0, 1)
      columns <- append(columns, 50 + i)
    }
  }

  # Split data into train and test partitions
  train <-
    subset(data[, columns], subset = data$Date <= train_end_date)
  test <-
    subset(data[, columns], subset = data$Date >= test_start_date)

  # Drop NA values
  train <- na.omit(train)
  test <- na.omit(test)

  # Create lagged GDP values
  train[, "L1GDP"] <- lag(train[, "GDP"], n = 1)
  test[, "L1GDP"] <- lag(test[, "GDP"], n = 1)
  train[, "L2GDP"] <- lag(train[, "GDP"], n = 2)
  test[, "L2GDP"] <- lag(test[, "GDP"], n = 2)

  # Drop NA values
  train <- na.omit(train)
  test <- na.omit(test)

  # For each row in OOS (expanding window)
  for (i in 1:nrow(test)) {
    # Retrain to obtain new coefficients for explanatory variables
    # but keep initial hyperparameters fixed
    if (length(structural_breakpoints) == 2) {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP + Break_1 + Break_2, data = train)
    } else if (length(structural_breakpoints) == 1) {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP + Break_1, data = train)
    } else {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP, data = train)
    }
    # Retrieve prediction (Note -3 is intentional)
    prediction <- temp_model_sb$fitted.values[nrow(train) - 3]

    # Store prediction
    data[data$Date == max(train$Date), "Predictions"] <- prediction

    # Append training data
    train[nrow(train) + 1, ] <- test[i, ]
  }

  # Load data again,  no interpolation
  msfe_df <- load_data(dataset_end_date = dataset_end_date)

  # Append it with predictions
  msfe_df$Predictions <- data$Predictions

  # Removes all columns from datafraame except Date, GDP Growth and GDP Growth predictions
  msfe_df <- subset(msfe_df,
    select = c("Date", "GDP", "Predictions"),
    subset = data$Date >= test_start_date
  )

  # For plots, we want to display non-interpolated GDP values and predictions
  plot_df <- na.omit(msfe_df)

  # For MSFE calculations, we want to use NOCB
  # Replaces NA values with "NOCB" (Next Observation Carried Backwards)
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # Use NOCB-interpolated values for MSFE calculations.
  # see helper_functions.R
  msfe <- calculate_msfe(
    predictions = msfe_df$Predictions,
    oos = msfe_df$GDP
  )

  message(glue("AR(2) MSFE ({year}): {msfe}"))


  # Purely technical. LaTeX graphs and figures.

  plot_df$Date <- as.Date(plot_df$Date)
  # Initiate an array of quarterly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date(min(plot_df$Date)), as.Date(max(plot_df$Date)), by = "month")

  # Plot
  ar_plot <- make_plot(
    dates = plot_df$Date,
    predictions = plot_df$Predictions,
    observations = plot_df$GDP,
    msfe = msfe,
    label = glue("AR(2) {year}; {length(structural_breakpoints)} Structural Break(s)"),
    scale_y = c(-40, 20)
  )
  # ar_plot
  export_latex("plot", glue("ar_{STR_B}"), year, ar_plot, width = 5.7, height = 3, TEX = TEX)
}
