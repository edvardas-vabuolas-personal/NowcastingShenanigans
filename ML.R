####### Load Packages ##########
source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

###### Load Data ########
TEX <- FALSE

LSTM <- TRUE

RAGGED_PREDS <- TRUE

INTERVALS <- get_intervals()
predictions <- data.frame(
  seq(as.Date("2006-01-01"), as.Date("2022-09-01"),
    by = "month"
  )
)
names(predictions)[1] <- "Date"

for (year in c(2022)) {
  dataset_end_date <- as.character(INTERVALS[[year]]["dataset_end_date"])
  train_end_date <- as.character(INTERVALS[[year]]["train_end_date"])
  test_start_date <- as.character(INTERVALS[[year]]["test_start_date"])
  initial_window <- as.character(INTERVALS[[year]]["initial_window"])
  structural_breakpoints <- as.list(INTERVALS[[(paste0(year, ".break_points"))]])

  data <- load_data(
    dataset_end_date = dataset_end_date,
    interpolate = TRUE
  )
  for (i in seq_along(structural_breakpoints)) {
    data[, paste0("Break_", i)] <- ifelse(
      seq_len(nrow(data)) < structural_breakpoints[i], 0, 1
    )
  }

  lags <- 2
  data <- lag_data(data, lags)

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

  #### Creating sampling seeds for reproducibility ####
  set.seed(123)
  seeds <- get_seeds()

  # Enable multi-threading with three cores
  registerDoParallel(cores = 5)

  my_time_control <- trainControl(
    method = "timeslice",
    initialWindow = initial_window,
    horizon = 1,
    fixedWindow = FALSE,
    allowParallel = TRUE,
    savePredictions = "final",
    verbose = TRUE,
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
  ntrees <- 1 # manually setting no. trees - proportionate to computation time
  nodesize <- 32 # min node size - no. features/3, rule of thumb for regression
  mtry <- 33

  message("Hyperparameters successfully obtained")

  # For each row in the test sub sample (expanding window)
  for (i in 1:nrow(test_set)) {
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
    data[data$Date == max(train_set$Date), "EN Predictions"] <- en_prediction
    data[data$Date == max(train_set$Date), "R Predictions"] <- r_prediction
    data[data$Date == max(train_set$Date), "L Predictions"] <- l_prediction
    data[data$Date == max(train_set$Date), "RF Predictions"] <- rf_prediction
    train_set[nrow(train_set) + 1, ] <- test_set[i, ]
    message(glue("{year}. No. of OOS observations left: {nrow(test_set) - i}"))
  }

  if (LSTM == TRUE) {
    lstm_df <- read_csv(glue("./output/LSTM_{year}.csv"), show_col_types = FALSE)
    data[data$Date >= train_end_date, "LSTM Predictions"] <- lstm_df$`LSTM Predictions`
  } else {
    # Handles any potential errors
    data[data$Date >= train_end_date, "LSTM Predictions"] <- 0
  }


  # Create new dataframe called msfe_df and import dataset
  msfe_df <- load_data(dataset_end_date = dataset_end_date)
  msfe_df <- msfe_df[(lags + 1):nrow(msfe_df), ]
  msfe_df$`EN Predictions` <- data$`EN Predictions`
  msfe_df$`R Predictions` <- data$`R Predictions`
  msfe_df$`L Predictions` <- data$`L Predictions`
  msfe_df$`RF Predictions` <- data$`RF Predictions`
  msfe_df$`LSTM Predictions` <- data$`LSTM Predictions`

  # Appends the predictions column from data to msfe_df
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
  if (ragged_edges_pred == TRUE) {
    msfe_df_2 <- make_ragged(msfe_df, 2)
    msfe_df_1 <- make_ragged(msfe_df, 1)
    msfe_df_0 <- make_ragged(msfe_df, 0)

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

    en_ragged_plot <- plot_ragged(
      msfe_df_2,
      msfe_df_1,
      msfe_df_0,
      msfe_2,
      msfe_1,
      msfe_0,
      msfe_df
    )

    msfe_comparison_df <- data.frame(
      "Distance" = c(-2, -1, 0),
      "EN MSFE" = c(en_msfe_2, en_msfe_1, en_msfe_0),
      "R MSFE" = c(r_msfe_2, r_msfe_1, r_msfe_0),
      "L MSFE" = c(l_msfe_2, l_msfe_1, l_msfe_0),
      "RF MSFE" = c(rf_msfe_2, rf_msfe_1, rf_msfe_0),
      "LSTM MSFE" = c(lstm_msfe_2, lstm_msfe_1, lstm_msfe_0)
    )

    comparison_figure <- make_msfe_plot(msfe_comparison_df)
  }


  plot_df <- na.omit(msfe_df)
  # Replaces NA values with "NOCB" (Next Observation Carried Backwards)
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # Use NOCB-interpolated values for MSFE calculations
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

  #### Plot predictions and observations ####

  dates_for_plot <-
    seq(
      as.Date(min(plot_df$Date)),
      as.Date(max(plot_df$Date)),
      by = "quarter"
    )

  scale_y <- c(-30, 25)

  # A4 page is 8.3x11.7, Overleaf margins are 1 inch. Adjust as needed.
  height <- round(9.7 / 5, digits = 2)
  width <- 6.3

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

  export_latex("plot", "en", year, en_plot, height = height, width = width, TEX = TEX)

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

  export_latex("plot", "r", year, r_plot, height = height, width = width, TEX = TEX)

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

  export_latex("plot", "l", year, l_plot, height = height, width = width, TEX = TEX)

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

  export_latex("plot", "rf", year, rf_plot, height = height, width = width, TEX = TEX)

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

    export_latex("plot", "lstm", year, lstm_plot, height = height, width = width, TEX = TEX)
  }

  predictions <- merge(predictions, msfe_df[, -c(2)], by = "Date", all = TRUE)
}

# Plot importance of variables

en_var_plot <- plot_var_imp(elastic_net, 50, "Elastic Net: Selected variables")
r_var_plot <- plot_var_imp(ridge, 50, "Ridge: Selected variables")
l_var_plot <- plot_var_imp(lasso, 50, "Lasso: Selected variables")
rf_var_plot <- plot_var_imp(rf_temp, 50, "Random Forest: Selected variables")

# A4 page is 8.3 on 11.7, Overleaf margins are 1 inch each side. Adjust as needed
height <- 9.7
width <- 6.3

export_latex("plot", "en_importance", "", en_var_plot, height = height, width = width, TEX = TEX)
export_latex("plot", "r_importance", "", r_var_plot, height = height, width = width, TEX = TEX)
export_latex("plot", "l_importance", "", l_var_plot, height = height, width = width, TEX = TEX)
export_latex("plot", "rf_importance", "", rf_var_plot, height = height, width = width, TEX = TEX)
