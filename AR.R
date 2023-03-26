###### Load Data ########

source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")

###### Load Data ########
INTERVALS <- get_intervals()
predictions <- data.frame(seq(as.Date("2006-01-01"), as.Date("2022-09-01"), by = "month"))
names(predictions)[1] <- "Date"

for (year in c(2010, 2019, 2022)) {
  dataset_end_date <- as.character(INTERVALS[paste0(year, ".dataset_end_date")])
  train_end_date <- as.character(INTERVALS[paste0(year, ".train_end_date")])
  test_start_date <- as.character(INTERVALS[paste0(year, ".test_start_date")])
  
  nowcasting_dataset <- load_data(
    dataset_end_date = if (dataset_end_date != FALSE) dataset_end_date else FALSE,
  )
  data <- load_data(
    dataset_end_date = dataset_end_date
  )
  
  # Split data into train and test partitions
  train <-
    subset(data[, c(1, 2)], subset = data$Date <= train_end_date)
  test <-
    subset(data[, c(1, 2)], subset = data$Date >= test_start_date)
  
  # Drop NA values
  train_omitted <- na.omit(train)
  test_omitted <- na.omit(test)
  
  # START: Diagnostic checks, lag selection and structural break identification
  
  # Plot time-series of GDP growth
  plot_of_GDP <- ggplot(data = train_omitted, aes(x = train_omitted$Date, y = train_omitted$GDP_QNA_RG)) +
    geom_line()
  ggsave(paste0('plot_of_GDP_', year, '.png'), plot_of_GDP)
  
  # Plot ACF function, needs to be decreasing
  acf_plot <- acf(train_omitted$GDP_QNA_RG, lag.max = 20, main = "ACF")
  
  # Perform ADF test, p-value needs to be less than 0.05 for stationarity
  adf.test(ts(train_omitted$GDP_QNA_RG))
  
  # Perform PP test, p-value needs to be less than 0.05 for stationarity
  gdp.pp <- ur.pp(train_omitted$GDP_QNA_RG, type = "Z-tau", model = "constant", lags = "short", use.lag = NULL)
  summary(gdp.pp)
  
  # Perform Zandrews test to identify and accomodate for a structural break
  gdp.za <- ur.za(train_omitted$GDP_QNA_RG, model = c("intercept"), lag = 1)
  summary(gdp.za)
  
  # Identify structural breaks
  attach(train_omitted)
  x <- Fstats(GDP_QNA_RG ~ 1, from = 0.01) # uses the chow test to generate critical values
  sctest(x) # tests for the existence of structural change with H0 = there is no structural change
  strucchange::breakpoints(GDP_QNA_RG ~ 1) # identifies the number of breakpoints with corresponding observation number
  
  # Create dummy variables corresponding to each breakpoint identified by strucchange
  break_1 <- 15
  train_omitted$break1 <- ifelse(seq_len(nrow(train_omitted)) < break_1, 0, 1)
  break_2 <- 73
  train_omitted$break2 <- ifelse(seq_len(nrow(train_omitted)) < break_2, 0, 1)
  break_3 <- 88
  train_omitted$break3 <- ifelse(seq_len(nrow(train_omitted)) < break_3, 0, 1)
  
  # Adds the dummy variables to the testing data
  test_omitted$break1 <- 1
  test_omitted$break2 <- 1
  test_omitted$break3 <- 1
  
  # Initiate a matrix that will store AIC and BIC for each AR lag
  info_critera <- matrix(NA, nrow = 10, ncol = 2)
  
  for (p in 1:10) {
    ar_model <- arima(train_omitted$GDP_QNA_RG, order = c(p, 0, 0))
    info_critera[p, ] <- c(ar_model$aic, ar_model$bic)
  }
  
  colnames(info_critera) <- c("AIC", "BIC")
  rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))
  
  info_critera # Displays info criterion table for optimal lag length
  
  # END: Diagnostic checks and lag selection
  
  # START: One step ahead forecast of test sub sample
  
  # Initiate an empty list for predictions
  list_of_predictions <- list()
  
  ###### NON-STRUCTURAL BREAAK MODEL ######
  # # For each row in the test sub sample
  # for (i in 1:nrow(test_omitted)) {
  #   # Obtain coefficients AR(2) using train sub sample
  #   temp_model <- arima(train_omitted$GDP_QNA_RG, order = c(2, 0, 0), method = "ML")
  # 
  #   # Forecast one step ahead
  #   one_step_ahead_forecast <- predict(temp_model, n.ahead = 1)
  # 
  #   # Update train sub sample with one row from test sub sample
  #   train_omitted[nrow(train_omitted) + 1, ] <- test_omitted[i, ]
  # 
  #   # Store prediction in the predictions list
  #   list_of_predictions <-
  #     append(list_of_predictions, one_step_ahead_forecast$pred)
  # }

  ###### STRUCTURAL BREAK MODEL ######
  for (i in 1:nrow(test_omitted)) {
    temp_model_sb <- lm(GDP_QNA_RG  ~ lag(GDP_QNA_RG, n = 2) + break1 + break2 + break3, data = train_omitted)

    train_omitted[nrow(train_omitted) + 1, ] <- test_omitted[i, ]

    list_of_predictions <-
          append(list_of_predictions, temp_model_sb$fitted.values[nrow(train_omitted)-3])
  }
  
  # Calculate MSFE. SUM(residuals^2) / N
  msfe <-
    sum((as.numeric(list_of_predictions) - test_omitted$GDP_QNA_RG)^2) / nrow(test_omitted)
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
      data = test_omitted,
      aes(
        x = as.Date(dates_for_plot),
        y = GDP_QNA_RG,
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
    
    #sets a standard scale for the y-axis
    scale_y_continuous(limits = c(-40, 20))
  ggsave(paste0('ar_plot_', year, '.png'), ar_plot, width = 7, height = 7) 
}
  
