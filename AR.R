####### Load Packages ##########
# library(dplyr) #data manipulation
library(tidyverse) #data manipulation
library(tidyr) #data manipulation
library(ggplot2) #data visualisation
library(caret) #ML training
library(forecast) #time series forecasting, stationarity testing
library(tseries)
library(readxl) #data import
library(readr) #data import
library(strucchange) #structural break test
library(gapminder)
library(xts)

###### Load Data ########

nowcasting_dataset <- read_excel(
  "230312 Nowcasting Dataset.xls",
  col_types = c(
    "numeric",
    "date",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)
rownames(nowcasting_dataset) <- nowcasting_dataset$Date

<<<<<<< HEAD
<<<<<<< HEAD
# Split data into train and test partitions
train <-
  subset(nowcasting_dataset[, c(1,2)], subset = nowcasting_dataset$Date <= '2015-12-01')
test <-
  subset(nowcasting_dataset[, c(1,2)], subset = nowcasting_dataset$Date >= '2016-01-01')

# Drop NA values
train_omitted <- na.omit(train)
test_omitted <- na.omit(test)

# START: Diagnostic checks, lag selection and structural break identification

# Plot time-series of GDP growth
ggplot(data = train_omitted, aes(x = train_omitted$Date, y = train_omitted$GDP_QNA_RG)) +
  geom_line()

# Plot ACF function, needs to be decreasing
acf(train_omitted, lag.max = 20, main = 'ACF')

# Perform ADF test, p-value needs to be less than 0.05 for stationarity
adf.test(ts(train_omitted$GDP_QNA_RG))

# Perform PP test, p-value needs to be less than 0.05 for stationarity
gdp.pp <- ur.pp(train_omitted$GDP_QNA_RG, type = "Z-tau", model = "constant", lags = "short", use.lag = NULL)
summary(gdp.pp)

# Perform Zandrews test to identify and accomodate for a structural break
gdp.za <- ur.za(train_omitted$GDP_QNA_RG, model=c("intercept"), lag=1)
summary(gdp.za)

# Identify structural breaks
attach(train_omitted)
x = Fstats(GDP_QNA_RG~1, from = 0.01) # uses the chow test to generate critical values 
sctest(x) # tests for the existence of structural change with H0 = there is no structural change
strucchange::breakpoints(GDP_QNA_RG~1) # identifies the number of breakpoints with corresponding observation number

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
  ar_model = arima(train_omitted, order = c(p, 0, 0))
  info_critera[p, ] <- c(ar_model$aic, ar_model$bic)
=======
# Before running the code, change this to False for quarterly forecasting
# Keep "interpolate <- TRUE" for monthly forecasting
interpolate <- TRUE

=======
# Before running the code, change this to False for quarterly forecasting
# Keep "interpolate <- TRUE" for monthly forecasting
interpolate <- TRUE

>>>>>>> 7c5913b (deleted Python env)
#### Without Interpolation ####
if (interpolate == FALSE) {
  # Split data into train and test partitions
  train <-
    subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date <= '2010-12-01')
  test <-
    subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date >= '2011-01-01')
  
  # Drop NA values
  train_omitted <- na.omit(train)
  test_omitted <- na.omit(test)
  
  # START: Diagnostic checks and lag selection
  
  # Peform ADF test, p-value needs to be less than 0.05 for stationarity
  adf.test(ts(train_omitted))
  
  # Plot ACF function, needs to be decreasing
  acf(train_omitted, lag.max = 20, main = 'ACF')
  
  # Initiate a matrix that will store AIC and BIC for each AR lag
  info_critera <- matrix(NA, nrow = 10, ncol = 2)
  
  for (p in 1:10) {
    ar_model = arima(train_omitted, order = c(p, 0, 0))
    info_critera[p, ] <- c(ar_model$aic, ar_model$bic)
  }
  
  colnames(info_critera) <- c("AIC", "BIC")
  rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))
  
  # END: Diagnostic checks and lag selection
  
  # START: One step ahead forecast of test sub sample
  
  # Initiate an empty list for predictions
  list_of_predictions <- list()
  
  # For each row in the test sub sample
  for (i in 1:nrow(test_omitted)) {
    # Obtain coefficients AR(2) using train sub sample
    temp_model <- arima(train_omitted, order = c(2, 0, 0))
    
    # Forecast one step ahead
    one_step_ahead_forecast <- predict(temp_model, n.ahead = 1)
    
    # Update train sub sample with one row from test sub sample
    train_omitted[nrow(train_omitted) + 1,] = test_omitted[i,]
    
    # Store prediction in the predictions list
    list_of_predictions <-
      append(list_of_predictions, one_step_ahead_forecast$pred)
  }
  
  # Calculate MSFE. SUM(residuals^2) / N
  msfe <-
    sum((as.numeric(list_of_predictions) - test_omitted$UK_GDP_4) ^ 2) / nrow(test_omitted)
  ## Accuracy library?
  
  # END: One step ahead forecast of test sub sample
  
  ### Plot predictions and observations ###
  
  # Initiate an array of quarterly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by = "quarter")
  
  # Put predictions and an array of dates into a dataframe
  predictions_df <- data.frame(list_of_predictions, dates_for_plot)
  
  # Color selection
  colors <-
    c("Predictions" = "dark green",
      "Observations" = "steelblue")
  
  # Plot
  ggplot() +
    
    # Draw predictions line
    geom_line(
      data = predictions_df,
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
        y = UK_GDP_4,
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
    theme(axis.text.x = element_text(angle = 45))
  
  
  #### With Interpolation ####
} else {
  # Interpolate GDP linearly
  nowcasting_dataset$UK_GDP_4 <-
    na.approx(nowcasting_dataset$UK_GDP_4)
  
  # Split data into train and test partitions
  train <-
    subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date <= '2010-12-01')
  test <-
    subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date >= '2011-01-01')
  
  # START: Diagnostic checks and lag selection
  
  # Peform ADF test, p-value needs to be less than 0.05 for stationarity
  adf.test(ts(train))
  
  # Plot ACF function, needs to be decreasing
  acf(train, lag.max = 100, main = 'ACF')
  
  # Initiate a matrix that will store AIC and BIC for each AR lag
  info_critera <- matrix(NA, nrow = 20, ncol = 2)
  
  for (p in 1:20) {
    ar_model = arima(train, order = c(p, 0, 0))
    info_critera[p, ] <- c(ar_model$aic, ar_model$bic)
  }
  
  colnames(info_critera) <- c("AIC", "BIC")
  rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))
  
  ## According to AIC and BIC, best model with interpolated values is AR11
  
  # END: Diagnostic checks and lag selection
  
  # START: One step ahead forecast of test sub sample
  
  # Initiate an empty list for predictions
  list_of_predictions <- list()
  
  # For each row in the test sub sample
  for (i in 1:nrow(test)) {
    # Obtain coefficients AR(11) using train sub sample
    temp_model <- arima(train, order = c(11, 0, 0))
    
    # Forecast one step ahead
    one_step_ahead_forecast <- predict(temp_model, n.ahead = 1)
    
    # Update train sub sample with one row from test sub sample
    train[nrow(train) + 1,] = test[i,]
    
    # Store prediction in the predictions list
    list_of_predictions <-
      append(list_of_predictions, one_step_ahead_forecast$pred)
  }
  msfe <-
    sum((as.numeric(list_of_predictions) - test$UK_GDP_4) ^ 2) / nrow(test)
  
  # END: One step ahead forecast of test sub sample
  
  
  ### Plot predictions and observations ###
  
  # Initiate an array of monthly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by = "month")
  
  # Put predictions and an array of dates into a dataframe
  predictions_df <- data.frame(list_of_predictions, dates_for_plot)
  
  # Color selection
  colors <-
    c("Predictions" = "dark green",
      "Observations" = "steelblue")
  
  # Plot
  ggplot() +
    
    # Draw predictions line
    geom_line(
      data = predictions_df,
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
        y = UK_GDP_4,
        color = "Observations"
      ),
      size = 1
    ) +
    # Change x and y titles
    labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
    # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +
    
    # Apply colours
    scale_color_manual(values = colors) +
    
    # Rotate x axis label by 45 degrees
    theme(axis.text.x = element_text(angle = 45))
<<<<<<< HEAD
>>>>>>> 7c5913b (deleted Python env)
=======
>>>>>>> 7c5913b (deleted Python env)
}