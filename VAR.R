source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

# Set this to TRUE for LaTeX exports.
# Note: VERY SLOW and overrides files in output
TEX <- FALSE

# Set this to TRUE if interested in vintages analysis
RAGGED_PREDS <- FALSE

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

  # Load and linearly interpolate dataset. See load_data.R.
  data <- load_data(
    dataset_end_date = dataset_end_date,
    interpolate = TRUE
  )

  # 1 - Date, 2 - GDP, 4 - Unempl, 19 - CPI
  columns <- c(1, 2, 4, 19)
  if (STR_BREAKS == TRUE) {
    # Add structural breaks based on interval parameter on structural breaks
    for (i in seq_along(structural_breakpoints)) {
      data[, paste0("Break_", i)] <- ifelse(
        seq_len(nrow(data)) < structural_breakpoints[i], 0, 1
      )
      columns <- append(columns, 50 + i)
    }
  }

  # Split dataset to train and test partitions
  train <-
    subset(
      data[, columns],
      subset = data$Date <= train_end_date
    )
  test <-
    subset(
      data[, columns],
      subset = data$Date >= test_start_date
    )

  # We move VAR training and predicting 1 step ahead
  # to a function of its own in the interest of code space
  train_pred <- function(train, exog, exog_p, lag) {
    # Full logic is explained in the Methdology section.

    # In summary:

    # if we want to use 2 dummy variables, but
    # the 2nd dummy variable has variance equal to 0,
    # then we only use 1 dummy variable.

    # if we want to use 1 dummy variable, but
    # it has variance equal to 0,
    # then we do not use any dummy variables

    # In both cases, we eventually start using 2 and 1
    # dummy variables, respectively, once the variance > 0.

    if (length(exog) > 0) {
      # endo - GDP, Unempl, CPI
      endo <- train[, c(2, 3, 4)]

      # exo - dummy variables
      exog <- as.matrix(exog)

      # exog_p - lagged dummy variable used for prediction
      exog_p <- as.matrix(exog_p)

      # This trains VAR model (estimates coefficients)
      var_m <- VAR(y = endo, p = lag, type = "const", exogen = exog)

      # This makes a one step ahead prediction
      pred_obj <- predict(object = var_m, ci = 0.95, n.ahead = 1, dumvar = exog_p)
    } else {
      # If exog is an empty list (intentional), then we
      # estimate VAR coefficients without dummy variables
      var_m <- VAR(y = train[, c(2, 3, 4)], p = lag, type = "const")

      # This makes a one step ahead prediction
      pred_obj <- predict(object = var_m, ci = 0.95, n.ahead = 1)
    }
    return(pred_obj)
  }

  # Set the number of lags. We use VARselect in Diagnostics.R
  # to get AIC and BIC values. Both IC suggested lag = 1.
  lag <- 1

  # exog is dummy variables. -c(...) is for non-endogenous
  exog <- train[, -c(1, 2, 3, 4)]

  # For row in OOS
  for (i in 1:nrow(test)) {
    # Full logic is explained in the Methdology section.

    # In summary:

    # if we want to use 2 dummy variables, but
    # the 2nd dummy variable has variance equal to 0,
    # then we only use 1 dummy variable.

    # if we want to use 1 dummy variable, but
    # it has variance equal to 0,
    # then we do not use any dummy variables

    # In both cases, we eventually start using 2 and 1
    # dummy variables, respectively, once the variance > 0.


    # Obtain coefficients for VAR(1) lag length
    if (length(structural_breakpoints) == 2) {
      if (exog[nrow(train), ncol(exog)] == 1) {
        pred_obj <- train_pred(train, exog[, ], exog[nrow(train) - 1, ], lag)
      } else {
        pred_obj <- train_pred(train, exog[, 1], exog[nrow(train) - 1, 1], lag)
      }
    } else if (length(structural_breakpoints) == 1) {
      if (exog[nrow(train), 1] == 1) {
        pred_obj <- train_pred(train, exog[, 1], exog[nrow(train) - 1, 1], lag)
      } else {
        pred_obj <- train_pred(train, c(), c(), lag)
      }
    } else {
      pred_obj <- train_pred(train, c(), c(), lag)
    }
    prediction <- pred_obj$fcst$GDP[, 1]

    # Store prediction
    data[data$Date == max(train$Date), "Predictions"] <- prediction

    # Append the training set with OOS
    train[nrow(train) + 1, ] <- as.list(test[i, ])

    # Append exog with OOS dummy variables.
    exog[nrow(exog) + 1, ] <- as.list(test[i, -c(1, 2, 3, 4)])
  }

  # Load data again, this time, without interpolation
  msfe_df <- load_data(dataset_end_date = dataset_end_date)

  # Add predictions
  msfe_df$Predictions <- data$Predictions

  # Removes all columns from the dataframe except Date,
  # GDP Growth and GDP Growth predictions
  msfe_df <- subset(msfe_df,
    select = c("Date", "GDP", "Predictions"),
    subset = data$Date >= test_start_date
  )

  # For plots, we want to display non-interpolated GDP values and predictions
  plot_df <- na.omit(msfe_df)

  if (RAGGED_PREDS == TRUE) {
    # The second argument of make_ragged specifies how many months
    # away from the official GDP release. Returns NOCB interpolated
    # dataframe. See helper_functions.R
    msfe_df_2 <- make_ragged(msfe_df, 2)
    msfe_df_1 <- make_ragged(msfe_df, 1)
    msfe_df_0 <- make_ragged(msfe_df, 0)

    # Calculate MSFE. See helper_functions.R
    msfe_2 <- calculate_msfe(msfe_df_2$Predictions, msfe_df_2$GDP)
    msfe_1 <- calculate_msfe(msfe_df_1$Predictions, msfe_df_1$GDP)
    msfe_0 <- calculate_msfe(msfe_df_0$Predictions, msfe_df_0$GDP)

    # Purely technical. LaTeX graphs and figures.
    width <- 5.3
    height <- 3

    msfe_comparison_df <- data.frame(
      "Distance" = c(-2, -1, 0),
      "VAR MSFE" = c(msfe_2, msfe_1, msfe_0)
    )
    write.csv(msfe_comparison_df, glue("./output/var_msfe_comp_{STR_B}_{year}.csv"), row.names = FALSE)
    export_latex("table", glue("var_msfes_{STR_B}"), year, msfe_comparison_df, width = width, height = height, TEX = TEX)
  }

  # For MSFE calculations, we want to use NOCB
  # Replaces NA values with "NOCB" (Next Observation Carried Backwards)
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # Use NOCB-interpolated values for MSFE calculations
  # see helper_functions.R
  msfe <- calculate_msfe(
    predictions = msfe_df$Predictions,
    oos = msfe_df$GDP
  )

  message(glue("VAR({lag}) MSFE ({year}): {msfe}"))


  # Purely technical. LaTeX graphs and figures.

  plot_df$Date <- as.Date(plot_df$Date)
  # Initiate an array of quarterly dates from 2011 to 2018
  dates_for_plot <-
    seq(
      as.Date(min(plot_df$Date)),
      as.Date(max(plot_df$Date)),
      by = "quarter"
    )
  scale_y <- c(-40, 25)
  # Plot
  var_plot <- make_plot(
    dates = dates_for_plot,
    predictions = plot_df$Predictions,
    observations = plot_df$GDP,
    msfe = msfe,
    label = glue("VAR({lag}) {year}; {length(structural_breakpoints)} Structural Break(s)"),
    scale_y = scale_y
  )
  # ar_plot
  export_latex("plot", glue("var_{STR_B}"), year, var_plot, width = 5.7, height = 3, TEX = TEX)
}
