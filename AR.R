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

###### Construct Data Set (UK-MD) ########

balanced_uk_md <- read_csv("balanced_uk_md.csv") # Balanced UK-MD data from 1998

tr_uk_md <- read_csv("tr_uk_md.csv") # Complete transformed UK-MD data from 1948

head(balanced_uk_md) 

###### Construct Data Set (Uncertain Kingdom) #######


nowcasting_dataset <- read_excel("230312 Nowcasting Dataset.xls", 
                                 col_types = c("numeric", "date", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric"))
rownames(nowcasting_dataset) <- nowcasting_dataset$Date

interpolate <- TRUE
if (interpolate == FALSE) {
  train <- subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date <= '2010-12-01')
  test <- subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date >= '2011-01-01')
  train_omitted <- na.omit(train)
  test_omitted <- na.omit(test)
  
  # Peform ADF test, p-value needs to be less than 0.05 for stationarity
  adf.test(ts(train_omitted))
  
  # Plot ACF function, needs to be decreasing
  acf(train_omitted, lag.max = 20, main='ACF')
  
  # Initiate a matrix that will store AIC and BIC for each AR lag
  info_critera <- matrix(NA, nrow=10, ncol=2)
  
  for (p in 1:10) {
    ar_model = arima(train_omitted, order = c(p, 0, 0))
    info_critera[p,] <- c(ar_model$aic, ar_model$bic)
  }
  
  colnames(info_critera) <- c("AIC", "BIC")
  rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))
  
  list_of_predictions <- list()
  for (i in 1:nrow(test_omitted)) {
    # create a new time series object with the updated data
    temp_model <- arima(train_omitted, order = c(11, 0, 0))
    one_step_ahead_forecast <- predict(temp_model, n.ahead = 1)
    train_omitted[nrow(train_omitted) + 1, ] = test_omitted[i, ]
    list_of_predictions <- append(list_of_predictions, one_step_ahead_forecast$pred)
  }
  msfe <- sum((as.numeric(list_of_predictions) - test_omitted$UK_GDP_4)^2) / nrow(test_omitted)
  
  dates_for_plot <- seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by="quarter")
  
  predictions_df <- data.frame(list_of_predictions, dates_for_plot)
  
  ### Plot predictions and observations ###
  colors <- c("Predictions" = "dark green", "Observations" = "steelblue")
  ggplot() +
    # Draw line
    geom_line(data = predictions_df, 
              aes(
                x = as.Date(dates_for_plot),
                y = as.numeric(list_of_predictions), 
                color = "Predictions"),
              size = 1) +
    
    geom_line(data = test_omitted, aes(x = as.Date(dates_for_plot),
                                   y = UK_GDP_4, color = "Observations"), 
              size = 1) +
    # Change x axis title
    labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
    # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +
    scale_color_manual(values = colors) + 
    theme(axis.text.x = element_text(angle = 45))
} else {
  nowcasting_dataset$UK_GDP_4 <- na.approx(nowcasting_dataset$UK_GDP_4)
  
  train <- subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date <= '2010-12-01')
  test <- subset(nowcasting_dataset[, 3], subset = nowcasting_dataset$Date >= '2011-01-01')
  
  # Peform ADF test, p-value needs to be less than 0.05 for stationarity
  adf.test(ts(train))

  # Plot ACF function, needs to be decreasing
  acf(train, lag.max = 100, main='ACF')
  
  # Initiate a matrix that will store AIC and BIC for each AR lag
  info_critera <- matrix(NA, nrow=20, ncol=2)

  for (p in 1:20) {
    ar_model = arima(train, order = c(p, 0, 0))
    info_critera[p,] <- c(ar_model$aic, ar_model$bic)
  }

  colnames(info_critera) <- c("AIC", "BIC")
  rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))

  ## According to AIC and BIC, best model with interpolated values is AR11
  
  list_of_predictions <- list()
  for (i in 1:nrow(test)) {
    # create a new time series object with the updated data
    temp_model <- arima(train, order = c(11, 0, 0))
    one_step_ahead_forecast <- predict(temp_model, n.ahead = 1)
    train[nrow(train) + 1, ] = test[i, ]
    list_of_predictions <- append(list_of_predictions, one_step_ahead_forecast$pred)
  }
  msfe <- sum((as.numeric(list_of_predictions) - test$UK_GDP_4)^2) / nrow(test)
  
  dates_for_plot <- seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by="month")
  
  predictions_df <- data.frame(list_of_predictions, dates_for_plot)
  
  ### Plot predictions and observations ###
  colors <- c("Predictions" = "dark green", "Observations" = "steelblue")
  ggplot() +
    # Draw line
    geom_line(data = predictions_df, 
              aes(
                x = as.Date(dates_for_plot),
                y = as.numeric(list_of_predictions), 
                color = "Predictions"),
              size = 1) +
    
    geom_line(data = test[,1], aes(x = as.Date(dates_for_plot),
                                   y = UK_GDP_4, color = "Observations"), 
              size = 1) +
    # Change x axis title
    labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
    # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +
    scale_color_manual(values = colors) + 
    theme(axis.text.x = element_text(angle = 45))
  

}
