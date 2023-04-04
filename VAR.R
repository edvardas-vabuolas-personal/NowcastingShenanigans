####### Load Packages ##########
source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

# Set TRUE to enable Latex export (very slow)
TEX <- TRUE

RAGGED_PREDS <- TRUE

STR_BREAKS <- TRUE

STR_B <- if (STR_BREAKS == TRUE) "w_str_b" else "wout_str_b"

INTERVALS <- get_intervals()
predictions <- data.frame(
  seq(as.Date("2006-01-01"), as.Date("2022-09-01"),
    by = "month"
  )
)
names(predictions)[1] <- "Date"

for (year in c(2010, 2019, 2022)) {
  dataset_end_date <- as.character(INTERVALS[[year]]["dataset_end_date"])
  train_end_date <- as.character(INTERVALS[[year]]["train_end_date"])
  test_start_date <- as.character(INTERVALS[[year]]["test_start_date"])
  structural_breakpoints <- as.list(INTERVALS[[(paste0(year, ".break_points"))]])

  structural_breakpoints <- if (STR_BREAKS == TRUE) structural_breakpoints else c()

  ###### Load Data ########
  data <- load_data(
    dataset_end_date = dataset_end_date,
    interpolate = TRUE
  )

  columns <- c(1, 2, 4, 11, 19, 36)
  if (STR_BREAKS == TRUE) {
    for (i in seq_along(structural_breakpoints)) {
      data[, paste0("Break_", i)] <- ifelse(
        seq_len(nrow(data)) < structural_breakpoints[i], 0, 1
      )
      columns <- append(columns, 50 + i)
    }
  }
  ###### Process data ######
  # Split dataset to train and test sub samples
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

  train_pred <- function(train, exog, exog_p, lag) {
    if (length(exog) > 0) {
      endo <- train[, c(2, 3, 4, 5, 6)]
      exog <- as.matrix(exog)
      exog_p <- as.matrix(exog_p)
      var_m <- VAR(y = endo, p = lag, type = "const", exogen = exog)
      pred_obj <- predict(object = var_m, ci = 0.95, n.ahead = 1, dumvar = exog_p)
    } else {
      var_m <- VAR(y = train[, c(2, 3, 4, 5, 6)], p = lag, type = "const")
      pred_obj <- predict(object = var_m, ci = 0.95, n.ahead = 1)
    }
    return(pred_obj)
  }

  lag <- 2
  exog <- train[, -c(1, 2, 3, 4, 5, 6)]
  for (i in 1:nrow(test)) {
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
    # Append train sub sample with one observation from the test sub sample
    data[data$Date == max(train$Date), "Predictions"] <- prediction
    train[nrow(train) + 1, ] <- as.list(test[i, ])
    exog[nrow(exog) + 1, ] <- as.list(test[i, -c(1, 2, 3, 4, 5, 6)])
  }

  # # Create new dataframe called msfe_df and import dataset
  msfe_df <- load_data(dataset_end_date = dataset_end_date)

  # # Appends the predictions column from data to msfe_df
  msfe_df$Predictions <- data$Predictions

  # # Removes all columns from datafraame except Date, GDP Growth and GDP Growth predictions
  msfe_df <- subset(msfe_df,
    select = c("Date", "GDP", "Predictions"),
    subset = data$Date >= test_start_date
  )
  plot_df <- na.omit(msfe_df)

  if (RAGGED_PREDS == TRUE) {
    msfe_df_2 <- make_ragged(msfe_df, 2)
    msfe_df_1 <- make_ragged(msfe_df, 1)
    msfe_df_0 <- make_ragged(msfe_df, 0)

    msfe_2 <- calculate_msfe(msfe_df_2$Predictions, msfe_df_2$GDP)
    msfe_1 <- calculate_msfe(msfe_df_1$Predictions, msfe_df_1$GDP)
    msfe_0 <- calculate_msfe(msfe_df_0$Predictions, msfe_df_0$GDP)

    width <- 5.3
    height <- 3

    msfe_comparison_df <- data.frame(
      "Distance" = c(-2, -1, 0),
      "VAR MSFE" = c(msfe_2, msfe_1, msfe_0)
    )
    write.csv(msfe_comparison_df, glue("./output/var_msfe_comp_{STR_B}_{year}.csv"), row.names = FALSE)
    export_latex("table", glue("var_msfes_{STR_B}"), year, msfe_comparison_df, width = width, height = height, TEX = TEX)

    # comparison_figure <- make_msfe_plot(msfe_comparison_df)
    # export_latex("plot", glue("var_msfes_{STR_B}"), year, comparison_figure, width=width, height=height, TEX = TEX)
  }

  # Replaces NA values in GDP column with the next non-missing value
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # # Uses the new complete panel to calculated MSFE for VAR model
  msfe <- calculate_msfe(
    predictions = msfe_df$Predictions,
    oos = msfe_df$GDP
  )
  message(glue("VAR(2) MSFE ({year}): {msfe}"))

  # ##### Plot predictions and observations #####

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

  names(msfe_df)[3] <- c(glue("VAR({lag}}) {year}"))

  predictions <- merge(predictions, msfe_df[, c(1, 3)], by = "Date", all = TRUE)
}
predictions$Date <- format(predictions$Date, "%d-%m-%Y")
export_latex("table", glue("var_{STR_B}"), year, predictions, TEX = TEX)
