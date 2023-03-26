####### Load Packages ##########
source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")

INTERVALS <- get_intervals()
predictions <- data.frame(seq(as.Date("2006-01-01"), as.Date("2022-09-01"), by = "month"))
names(predictions)[1] <- "Date"

for (year in c(2010, 2019, 2022)) {
  dataset_end_date <- as.character(INTERVALS[paste0(year, ".dataset_end_date")])
  train_end_date <- as.character(INTERVALS[paste0(year, ".train_end_date")])
  test_start_date <- as.character(INTERVALS[paste0(year, ".test_start_date")])

  ###### Load Data ########
  nowcasting_dataset <- load_data(
    dataset_end_date = dataset_end_date,
    interpolate = TRUE
  )

  ###### Process data ######

  # Split dataset to train and test sub samples
  train <-
    subset(
      nowcasting_dataset[, c(2, 4, 11, 19, 36)],
      subset = nowcasting_dataset$Date <= train_end_date
    )
  test <-
    subset(
      nowcasting_dataset[, c(2, 4, 11, 19, 36)],
      subset = nowcasting_dataset$Date >= test_start_date
    )

  # The output of VARselect tells us what lag length we should use
  var_select <- VARselect(train, lag.max = 10, type = "const")

  ###### One step ahead forecast of test sub sample  ######

  # Initiate an empty list for predictions
  list_of_predictions <- list()

  # For each row in the test sub sample
  for (i in 1:nrow(test)) {
    # Obtain coefficients for VAR(1) lag length
    temp_model <- VAR(train, p = 1, type = "const")

    # Forecast one step ahead; feed one observation from test sub sample
    one_step_ahead_forecast_object <-
      predict(temp_model, test[i, ], n.ahead = 1)
    prediction <- one_step_ahead_forecast_object$fcst$GDP_QNA_RG[, 1]
    # Append train sub sample with one observation from the test sub sample
    nowcasting_dataset[nrow(train) + 1, "Predictions"] <- prediction
    train[nrow(train) + 1, ] <- as.list(test[i, ])
    # Append the list of predictins with the one ahead forecast
    list_of_predictions <-
      append(list_of_predictions, prediction)
  }

  # Create new dataframe called msfe_df and import dataset
  msfe_df <- load_data(dataset_end_date = dataset_end_date)

  # Appends the predictions column from nowcasting_dataset to msfe_df
  msfe_df$Predictions <- nowcasting_dataset$Predictions

  # Removes all columns from datafraame except Date, GDP Growth and GDP Growth predictions
  msfe_df <- subset(msfe_df,
    select = c("Date", "GDP_QNA_RG", "Predictions"),
    subset = nowcasting_dataset$Date >= test_start_date
  )

  # Replaces NA values in GDP column with the next non-missing value
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # Uses the new complete panel to calculated MSFE for VAR model
  msfe <-
    sum((as.numeric(msfe_df$Predictions) - msfe_df$GDP_QNA_RG)^2) / nrow(msfe_df)

  ##### Plot predictions and observations #####

  # Initiate an array of monthly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date(test_start_date), as.Date(if (dataset_end_date != FALSE) dataset_end_date else "2022-09-01"), by = "month")

  # Put predictions and an array of dates into a dataframe
  predictions_df <- data.frame(list_of_predictions, dates_for_plot)
  temp_preds <- data.frame(dates_for_plot, as.data.frame(as.numeric(list_of_predictions)))
  names(temp_preds)[1] <- "Date"
  names(temp_preds)[2] <- paste0("ar_pred_", year)

  predictions <- merge(predictions, temp_preds, by = "Date", all = TRUE)

  # Color selection
  colors <-
    c(
      "Predictions" = "dark green",
      "Observations" = "steelblue"
    )

  # Plot
  var_plot <- ggplot() +

    # Draw predictions line
    geom_line(
      data = temp_preds,
      aes(
        x = as.Date(dates_for_plot),
        y = as.numeric(list_of_predictions),
        color = "Predictions"
      ),
      size = 1
    ) +

    # Draw observations line
    geom_line(
      data = test[, 1],
      aes(
        x = as.Date(dates_for_plot),
        y = test$GDP_QNA_RG,
        color = "Observations"
      ),
      linewidth = 1
    ) +

    # Change x and y titles
    labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +

    # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +

    # Apply colours
    scale_color_manual(values = colors) +

    # Rotate x axis labels by 45 degrees
    theme(axis.text.x = element_text(angle = 45)) +

    # Add MSFE to the graph
    annotate(
      geom = "text",
      x = as.Date(test_start_date) + 180,
      y = -30,
      label = paste0("MSFE: ", round(msfe, digits = 4))
    ) +

    # sets a standard scale for the y-axis
    scale_y_continuous(limits = c(-40, 20))
  ggsave(paste0("var_plot_", year, ".png"), var_plot)
}
