###### Load Data ########

source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

###### Load Data ########

# Set TRUE to enable Latex export (very slow)
TEX = TRUE

INTERVALS <- get_intervals()
predictions <- data.frame(
  seq(as.Date("2006-01-01"), as.Date("2022-09-01"),
    by = "month"
  )
)
names(predictions)[1] <- "Date"
year <- 2022
for (year in c(2010, 2019, 2022)) {
  dataset_end_date <- as.character(INTERVALS[[year]]["dataset_end_date"])
  train_end_date <- as.character(INTERVALS[[year]]["train_end_date"])
  test_start_date <- as.character(INTERVALS[[year]]["test_start_date"])
  structural_breakpoints <- as.list(INTERVALS[[(paste0(year, ".break_points_quarterly"))]])

  data <- load_data(
    dataset_end_date = dataset_end_date
  )
  
  columns = c(1,2)
  for (i in seq_along(structural_breakpoints)) {
    data[, paste0("Break_", i)] <- ifelse(seq_len(nrow(data)) < structural_breakpoints[i], 0, 1)
    columns <- append(columns, 50+i)
  }

  # Split data into train and test partitions
  train <-
    subset(data[, columns], subset = data$Date <= train_end_date)
  test <-
    subset(data[, columns], subset = data$Date >= test_start_date)

  # Drop NA values
  train <- na.omit(train)
  test <- na.omit(test)
  
  train[, "L1GDP"] <- lag(train[, "GDP"], n = 1)
  test[, "L1GDP"] <- lag(test[, "GDP"], n = 1)
  train[, "L2GDP"] <- lag(train[, "GDP"], n = 2)
  test[, "L2GDP"] <- lag(test[, "GDP"], n = 2)
  
  train <- na.omit(train)
  test <- na.omit(test)
  
  # START: One step ahead forecast of test sub sample

  # Initiate an empty list for predictions
  pred_list <- list()

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
  #   pred_list <-
  #     append(pred_list, one_step_ahead_forecast$pred)
  # }

  ###### STRUCTURAL BREAK MODEL ######
  for (i in 1:nrow(test)) {
    
    if (length(structural_breakpoints) == 2) {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP+ Break_1 + Break_2, data = train)
    } else if (length(structural_breakpoints) == 1) {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP+ Break_1, data = train)
    }
    
    train[nrow(train) + 1, ] <- test[i, ]
    pred_list <-
      append(pred_list, temp_model_sb$fitted.values[nrow(train) - 3])
  }

  # Calculate MSFE. SUM(residuals^2) / N
  msfe <- calculate_msfe(pred_list, test$GDP)
  print(msfe)

  # END: One step ahead forecast of test sub sample

  test$Date <- as.Date(test$Date)
  # Initiate an array of quarterly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date(min(test$Date)), as.Date(max(test$Date)), by = "quarter")

  # Plot
  ar_plot <- make_plot(
    dates = dates_for_plot,
    predictions = pred_list,
    observations = test$GDP,
    msfe = msfe,
    label = glue('AR(2) {year}; {length(structural_breakpoints)} Structural Break(s)'),
    scale_y = c(-40, 20)
  )
  # ar_plot
  export_latex("plot", "ar", year, ar_plot, height = 3, TEX=TEX)
  
  temp_preds <- data.frame(dates_for_plot, as.data.frame(as.numeric(pred_list)))
  names(temp_preds) <- c("Date", paste0("ar_pred_", year))
  
  predictions <- merge(predictions, temp_preds, by = "Date", all = TRUE)
}

predictions$Date <- format(predictions$Date, "%d-%m-%Y")
export_latex("table", "ar", year, predictions, TEX=TEX)
