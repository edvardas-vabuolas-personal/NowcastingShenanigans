source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

# Set this to TRUE for LaTeX exports.
# Note: VERY SLOW and overrides files in output
TEX <- FALSE

# Set this to TRUE only if LSTM data exists in output
LSTM <- TRUE

# Set this to TRUE if interested in vintages analysis
RAGGED_PREDS <- TRUE

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
  initial_window <- as.character(INTERVALS[[year]]["initial_window"])
  structural_breakpoints <- as.list(INTERVALS[[(paste0(year, ".break_points"))]])

  # Reset the list to empty if STR_BREAKS == FALSE
  structural_breakpoints <- if (STR_BREAKS == TRUE) structural_breakpoints else c()

  # Load and linearly interpolate dataset. See load_data.R.
  data <- load_data(
    dataset_end_date = dataset_end_date,
    interpolate = TRUE
  )

  if (STR_BREAKS == TRUE) {
    # Add structural breaks based on interval parameter on structural breaks
    for (i in seq_along(structural_breakpoints)) {
      data[, paste0("Break_", i)] <- ifelse(
        seq_len(nrow(data)) < structural_breakpoints[i], 0, 1
      )
    }
  }

  # Set the number of lags
  lags <- 2

  # Lag all explanatory variables by <lags>, see helper_functions.R
  data <- lag_data(data, lags)

  # Split to train and test sets
  train_set <-
    subset(
      data,
      subset = data$Date <= train_end_date
    )
  test_set <-
    subset(
      data,
      subset = data$Date >= test_start_date
    )

  # Creating sampling seeds for reproducibility
  set.seed(123)
  seeds <- get_seeds()

  # Enable multi-threading with three cores
  registerDoParallel(cores = 3)

  # Enable expanding window configuration used by Caret packaage
  my_time_control <- trainControl(
    method = "timeslice",
    initialWindow = initial_window,
    horizon = 1,
    fixedWindow = FALSE,
    allowParallel = TRUE,
    savePredictions = "final",
    verbose = FALSE,
    seeds = seeds
  )

  # Intial runs to obtain hyperparameters
  elastic_net <- train(
    GDP ~ .,
    data = data[, -c(1)],
    method = "glmnet",
    family = "gaussian",
    trControl = my_time_control,
    tuneLength = 15,
    metric = "RMSE"
  )
  ridge <- train(
    GDP ~ .,
    data = data[, -c(1)],
    method = "glmnet",
    family = "gaussian",
    trControl = my_time_control,
    tuneGrid = expand.grid(
      alpha = 0,
      lambda = 2^runif(15, min = -10, 3)
    ),
    metric = "RMSE"
  )
  lasso <- train(
    GDP ~ .,
    data = data[, -c(1)],
    method = "glmnet",
    family = "gaussian",
    trControl = my_time_control,
    tuneGrid = expand.grid(
      alpha = 1,
      lambda = 2^runif(15, min = -10, 3)
    ),
    metric = "RMSE"
  )

  # No. of trees for random forest
  ntrees <- 100

  # Min node size for random forest
  nodesize <- 5

  message("Hyperparameters successfully obtained")

  # For each row in OOS (expanding window)
  for (i in 1:nrow(test_set)) {
    # Retrain to obtain new coefficients for explanatory variables
    # but keep initial hyperparameters fixed
    elastic_net_temp <- train(
      GDP ~ .,
      data = train_set[, -c(1)],
      method = "glmnet",
      family = "gaussian",
      tuneLength = 1,
      tuneGrid = expand.grid(
        alpha = elastic_net$bestTune$alpha,
        lambda = elastic_net$bestTune$lambda
      ),
      metric = "RMSE"
    )

    ridge_temp <- train(
      GDP ~ .,
      data = train_set[, -c(1)],
      method = "glmnet",
      family = "gaussian",
      tuneLength = 1,
      tuneGrid = expand.grid(
        alpha = ridge$bestTune$alpha,
        lambda = ridge$bestTune$lambda
      ),
      metric = "RMSE"
    )

    lasso_temp <- train(
      GDP ~ .,
      data = train_set[, -c(1)],
      method = "glmnet",
      family = "gaussian",
      tuneLength = 1,
      tuneGrid = expand.grid(
        alpha = lasso$bestTune$alpha,
        lambda = lasso$bestTune$lambda
      ),
      metric = "RMSE"
    )

    rf_temp <- train(
      GDP ~ .,
      data = train_set[, -c(1)],
      method = "rf",
      metric = "RMSE",
      ntree = ntrees,
      nodesize = nodesize
    )

    # Make a one-step ahead forecast
    en_prediction <- predict(
      object = elastic_net_temp,
      newdata = test_set[i, ]
    )

    r_prediction <- predict(
      object = ridge_temp,
      newdata = test_set[i, ]
    )

    l_prediction <- predict(
      object = lasso_temp,
      newdata = test_set[i, ]
    )

    rf_prediction <- predict(
      object = rf_temp,
      newdata = test_set[i, ]
    )

    # Store predictions
    data[data$Date == max(train_set$Date), "EN Predictions"] <- en_prediction
    data[data$Date == max(train_set$Date), "R Predictions"] <- r_prediction
    data[data$Date == max(train_set$Date), "L Predictions"] <- l_prediction
    data[data$Date == max(train_set$Date), "RF Predictions"] <- rf_prediction

    # Append the training set with OOS
    train_set[nrow(train_set) + 1, ] <- test_set[i, ]

    message(glue("{year}. No. of OOS observations left: {nrow(test_set) - i}"))
  }

  if (LSTM == TRUE) {
    # This output is produced by LSTM.ipynb in LSTM folder (requires Python)
    lstm_df <- read_csv(glue("./output/LSTM_{year}_{STR_B}.csv"), show_col_types = FALSE)

    # Remove training observations
    data[data$Date >= train_end_date, "LSTM Predictions"] <- lstm_df$`LSTM Predictions`
  } else {
    # Handles any potential errors
    data[data$Date >= train_end_date, "LSTM Predictions"] <- 0
  }


  # Load data again, this time, without interpolation
  msfe_df <- load_data(dataset_end_date = dataset_end_date)
  msfe_df <- msfe_df[(lags + 1):nrow(msfe_df), ]
  msfe_df$`EN Predictions` <- data$`EN Predictions`
  msfe_df$`R Predictions` <- data$`R Predictions`
  msfe_df$`L Predictions` <- data$`L Predictions`
  msfe_df$`RF Predictions` <- data$`RF Predictions`
  msfe_df$`LSTM Predictions` <- data$`LSTM Predictions`

  # Remove training observations, drop explanatory variables
  msfe_df <- subset(msfe_df,
    select = c(
      "Date",
      "GDP",
      "EN Predictions",
      "R Predictions",
      "L Predictions",
      "RF Predictions",
      "LSTM Predictions"
    ),
    subset = data$Date >= test_start_date
  )
  if (RAGGED_PREDS == TRUE) {
    # The second argument of make_ragged specifies how many months
    # away from the official GDP release. Returns NOCB interpolated
    # dataframe. See helper_functions.R
    msfe_df_2 <- make_ragged(msfe_df, 2)
    msfe_df_1 <- make_ragged(msfe_df, 1)
    msfe_df_0 <- make_ragged(msfe_df, 0)

    # Calculate MSFE. See helper_functions.R
    en_msfe_2 <- calculate_msfe(msfe_df_2$`EN Predictions`, msfe_df_2$GDP)
    en_msfe_1 <- calculate_msfe(msfe_df_1$`EN Predictions`, msfe_df_1$GDP)
    en_msfe_0 <- calculate_msfe(msfe_df_0$`EN Predictions`, msfe_df_0$GDP)

    r_msfe_2 <- calculate_msfe(msfe_df_2$`R Predictions`, msfe_df_2$GDP)
    r_msfe_1 <- calculate_msfe(msfe_df_1$`R Predictions`, msfe_df_1$GDP)
    r_msfe_0 <- calculate_msfe(msfe_df_0$`R Predictions`, msfe_df_0$GDP)

    l_msfe_2 <- calculate_msfe(msfe_df_2$`L Predictions`, msfe_df_2$GDP)
    l_msfe_1 <- calculate_msfe(msfe_df_1$`L Predictions`, msfe_df_1$GDP)
    l_msfe_0 <- calculate_msfe(msfe_df_0$`L Predictions`, msfe_df_0$GDP)

    rf_msfe_2 <- calculate_msfe(msfe_df_2$`RF Predictions`, msfe_df_2$GDP)
    rf_msfe_1 <- calculate_msfe(msfe_df_1$`RF Predictions`, msfe_df_1$GDP)
    rf_msfe_0 <- calculate_msfe(msfe_df_0$`RF Predictions`, msfe_df_0$GDP)

    lstm_msfe_2 <- calculate_msfe(msfe_df_2$`LSTM Predictions`, msfe_df_2$GDP)
    lstm_msfe_1 <- calculate_msfe(msfe_df_1$`LSTM Predictions`, msfe_df_1$GDP)
    lstm_msfe_0 <- calculate_msfe(msfe_df_0$`LSTM Predictions`, msfe_df_0$GDP)

    # Purely technical. LaTeX graphs and figures.
    width <- 5.3
    height <- 3

    msfe_comparison_df <- data.frame(
      "Distance" = c(-2, -1, 0),
      "EN MSFE" = c(en_msfe_2, en_msfe_1, en_msfe_0),
      "R MSFE" = c(r_msfe_2, r_msfe_1, r_msfe_0),
      "L MSFE" = c(l_msfe_2, l_msfe_1, l_msfe_0),
      "RF MSFE" = c(rf_msfe_2, rf_msfe_1, rf_msfe_0),
      "LSTM MSFE" = c(lstm_msfe_2, lstm_msfe_1, lstm_msfe_0)
    )
    write.csv(msfe_comparison_df, glue("./output/msfe_comp_{STR_B}_{year}.csv"), row.names = FALSE)
    export_latex("table", glue("ml_msfes_{STR_B}"), year, msfe_comparison_df, width = width, height = height, TEX = TEX)

    comparison_figure <- make_msfe_plot(msfe_comparison_df)
    export_latex("plot", glue("ml_msfes_{STR_B}"), year, comparison_figure, width = width, height = height, TEX = TEX)
  }

  # For plots, we want to display non-interpolated GDP values and predictions
  plot_df <- na.omit(msfe_df)

  # For MSFE calculations, we want to use NOCB
  # Replaces NA values with "NOCB" (Next Observation Carried Backwards)
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # Use NOCB-interpolated values for MSFE calculations
  # see helper_functions.R
  en_msfe <- calculate_msfe(
    predictions = msfe_df$`EN Predictions`,
    oos = msfe_df$GDP
  )
  r_msfe <- calculate_msfe(
    predictions = msfe_df$`R Predictions`,
    oos = msfe_df$GDP
  )
  l_msfe <- calculate_msfe(
    predictions = msfe_df$`L Predictions`,
    oos = msfe_df$GDP
  )
  rf_msfe <- calculate_msfe(
    predictions = msfe_df$`RF Predictions`,
    oos = msfe_df$GDP
  )

  if (LSTM == TRUE) {
    lstm_msfe <- calculate_msfe(
      predictions = msfe_df$`LSTM Predictions`,
      oos = msfe_df$GDP
    )
  }

  message(glue("Elastic Net MSFE ({year}): {en_msfe}"))
  message(glue("Ridge MSFE ({year}): {r_msfe}"))
  message(glue("Lasso MSFE ({year}): {l_msfe}"))
  message(glue("Random Forest MSFE ({year}): {rf_msfe}"))

  if (LSTM == TRUE) {
    message(glue("LSTM MSFE ({year}): {lstm_msfe}"))
  }



  # Purely technical. LaTeX graphs and figures.

  dates_for_plot <-
    seq(
      as.Date(min(plot_df$Date)),
      as.Date(max(plot_df$Date)),
      by = "quarter"
    )

  scale_y <- c(-30, 25)

  # A4 page is 8.3x11.7, Overleaf margins are 1 inch. Adjust as needed.
  height <- round(9.7 / 5, digits = 2)
  width <- 5.3

  en_plot <- make_plot(
    dates = dates_for_plot,
    predictions = plot_df$`EN Predictions`,
    observations = plot_df$GDP,
    msfe = en_msfe,
    label = glue(
      "Elastic Net ($\\alpha = {round(elastic_net$bestTune$alpha, digits=3)}, \\lambda = {round(elastic_net$bestTune$lambda, digit=3)}$). {year} with {length(structural_breakpoints)} Structural Break(s)"
    ),
    scale_y = scale_y
  )

  export_latex("plot", glue("en_{STR_B}"), year, en_plot, height = height, width = width, TEX = TEX)

  r_plot <- make_plot(
    dates = dates_for_plot,
    predictions = plot_df$`R Predictions`,
    observations = plot_df$GDP,
    msfe = r_msfe,
    label = glue(
      "Ridge ($\\alpha = {round(ridge$bestTune$alpha, digit=3)}, \\lambda = {round(ridge$bestTune$lambda, digit=3)}$). {year} with {length(structural_breakpoints)} Structural Break(s);"
    ),
    scale_y = scale_y
  )

  export_latex("plot", glue("r_{STR_B}"), year, r_plot, height = height, width = width, TEX = TEX)

  l_plot <- make_plot(
    dates = dates_for_plot,
    predictions = plot_df$`L Predictions`,
    observations = plot_df$GDP,
    msfe = l_msfe,
    label = glue(
      "Lasso ($\\alpha = {round(lasso$bestTune$alpha, digit=3)}, \\lambda = {round(lasso$bestTune$lambda, digit=3)}$). {year} with {length(structural_breakpoints)} Structural Break(s)"
    ),
    scale_y = scale_y
  )

  export_latex("plot", glue("l_{STR_B}"), year, l_plot, height = height, width = width, TEX = TEX)

  rf_plot <- make_plot(
    dates = dates_for_plot,
    predictions = plot_df$`RF Predictions`,
    observations = plot_df$GDP,
    msfe = rf_msfe,
    label = glue(
      "Random Forest ({ntrees} trees, {nodesize} min. node size). {year} with {length(structural_breakpoints)} Structural Break(s)"
    ),
    scale_y = scale_y
  )

  export_latex("plot", glue("rf_{STR_B}"), year, rf_plot, height = height, width = width, TEX = TEX)

  if (LSTM == TRUE) {
    lstm_plot <- make_plot(
      dates = dates_for_plot,
      predictions = plot_df$`LSTM Predictions`,
      observations = plot_df$GDP,
      msfe = lstm_msfe,
      label = glue(
        "LSTM. {year} with {length(structural_breakpoints)} Structural Break(s)"
      ),
      scale_y = scale_y
    )

    export_latex("plot", glue("lstm_{STR_B}"), year, lstm_plot, height = height, width = width, TEX = TEX)
  }
}

# Purely technical. LaTeX graphs and plots.

en_var_plot <- plot_var_imp(elastic_net_temp, 50, "Elastic Net: Selected variables")
r_var_plot <- plot_var_imp(ridge_temp, 50, "Ridge: Selected variables")
l_var_plot <- plot_var_imp(lasso_temp, 50, "Lasso: Selected variables")
rf_var_plot <- plot_var_imp(rf_temp_temp, 50, "Random Forest: Selected variables")

# A4 page is 8.3 on 11.7, Overleaf margins are 1 inch each side. Adjust as needed
height <- 9.7
width <- 6.3

export_latex("plot", glue("en_importance_{STR_B}"), year, en_var_plot, height = height, width = width, TEX = TEX)
export_latex("plot", glue("r_importance_{STR_B}"), year, r_var_plot, height = height, width = width, TEX = TEX)
export_latex("plot", glue("l_importance_{STR_B}"), year, l_var_plot, height = height, width = width, TEX = TEX)
export_latex("plot", glue("rf_importance_{STR_B}"), year, rf_var_plot, height = height, width = width, TEX = TEX)
