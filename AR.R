###### Load Data ########

source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")

###### Load Data ########
INTERVALS <- get_intervals()
predictions <- data.frame(
  seq(as.Date("2006-01-01"), as.Date("2022-09-01"),
    by = "month"
  )
)
names(predictions)[1] <- "Date"

for (year in c(2010, 2019, 2022)) {
  dataset_end_date <- as.character(INTERVALS[paste0(year, ".dataset_end_date")])
  train_end_date <- as.character(INTERVALS[paste0(year, ".train_end_date")])
  test_start_date <- as.character(INTERVALS[paste0(year, ".test_start_date")])

  data <- load_data(
    dataset_end_date = dataset_end_date
  )

  # Split data into train and test partitions
  train <-
    subset(data[, c(1, 2)], subset = data$Date <= train_end_date)
  test <-
    subset(data[, c(1, 2)], subset = data$Date >= test_start_date)

  # Drop NA values
  train <- na.omit(train)
  test <- na.omit(test)

  # Create dummy variables corresponding to each breakpoint identified by strucchange
  break_1 <- 15
  train$break1 <- ifelse(seq_len(nrow(train)) < break_1, 0, 1)
  break_2 <- 73
  train$break2 <- ifelse(seq_len(nrow(train)) < break_2, 0, 1)
  break_3 <- 88
  train$break3 <- ifelse(seq_len(nrow(train)) < break_3, 0, 1)

  # Adds the dummy variables to the testing data
  test$break1 <- 1
  test$break2 <- 1
  test$break3 <- 1

  # START: One step ahead forecast of test sub sample

  # Initiate an empty list for predictions
  list_of_predictions <- list()

  ###### NON-STRUCTURAL BREAAK MODEL ######
  # # For each row in the test sub sample
  # for (i in 1:nrow(test)) {
  #   # Obtain coefficients AR(2) using train sub sample
  #   temp_model <- arima(train$GDP, order = c(2, 0, 0), method = "ML")
  #
  #   # Forecast one step ahead
  #   one_step_ahead_forecast <- predict(temp_model, n.ahead = 1)
  #
  #   # Update train sub sample with one row from test sub sample
  #   train[nrow(train) + 1, ] <- test[i, ]
  #
  #   # Store prediction in the predictions list
  #   list_of_predictions <-
  #     append(list_of_predictions, one_step_ahead_forecast$pred)
  # }

  ###### STRUCTURAL BREAK MODEL ######
  for (i in 1:nrow(test)) {
    temp_model_sb <- lm(GDP ~ lag(GDP, n = 2) + break1 + break2 + break3, data = train)

    train[nrow(train) + 1, ] <- test[i, ]

    list_of_predictions <-
      append(list_of_predictions, temp_model_sb$fitted.values[nrow(train) - 3])
  }

  # Calculate MSFE. SUM(residuals^2) / N
  msfe <-
    sum((as.numeric(list_of_predictions) - test$GDP)^2) / nrow(test)
  ## Accuracy library?

  # END: One step ahead forecast of test sub sample

  ### Plot predictions and observations ###

  # Initiate an array of quarterly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date(test_start_date), as.Date(if (dataset_end_date != FALSE) dataset_end_date else "2022-09-01"), by = "quarter")

  # Put predictions and an array of dates into a dataframe
  predictions_df <- (data.frame(dates_for_plot, list_of_predictions))
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
  ar_plot <- ggplot() +

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
      data = test,
      aes(
        x = as.Date(dates_for_plot),
        y = GDP,
        color = "Observations"
      ),
      size = 1
    ) +

    # Change x and y titles
    labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +

    # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +

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
  export_latex("plot", "ar", year, ar_plot)
}
predictions$Date <- format(predictions$Date, "%d-%m-%Y")

export_latex("table", "ar", year, predictions)
