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
library(vars)

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
  train <- subset(uk_nowcast[, 34], subset = uk_nowcast$`...1` <= '2010-12-01')
  test <- subset(uk_nowcast[, 34], subset = uk_nowcast$`...1` >= '2011-01-01')
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
  
  best_model <- arima(train, order=c(2,0,0))
  list_of_msfes <- list()
  for (i in 1:nrow(test_omitted)) {
    train_omitted[nrow(train_omitted) + 1, ] = as.list(test_omitted[i,])
    # create a new time series object with the updated data
    temp_model <- arima(train_omitted, order = c(2, 0, 0))
    fitted_values <- fitted(temp_model)
    square_diff <- (as.numeric(fitted_values) - train_omitted)^2
    msfe <- sum(square_diff) / nrow(train_omitted)
    list_of_msfes <- append(list_of_msfes, msfe)
  }
  
  total_msfe <- sum(as.data.frame(list_of_msfes))
  mean_msfe <- mean(as.numeric(list_of_msfes))
  print(mean_msfe)
  dates_for_plot <- seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by="quarter")

  plot(x = dates_for_plot, y = list_of_msfes, xlab = 'Testing dates', ylab = 'MSFE')

  ggplot(data.frame(list_of_msfes, dates_for_plot), aes(x = as.Date(dates_for_plot),
                 y = as.numeric(list_of_msfes))) +
  #   # Draw line
    geom_point(size=3) +
  #   # Change x axis title
    labs(x = "Testing date", y = "MSFE") +
  #   # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
    theme(axis.text.x = element_text(angle = 45))
} else {
  nowcasting_dataset$UK_GDP_4 <- na.approx(nowcasting_dataset$UK_GDP_4)
  
  train <- subset(nowcasting_dataset[, c(3, 5, 12, 20, 37)], subset = nowcasting_dataset$Date <= '2010-12-01')
  test <- subset(nowcasting_dataset[, c(3, 5, 12, 20, 37)], subset = nowcasting_dataset$Date >= '2011-01-01')
  
  VARselect(train, lag.max = 10, type = 'const')
  
  list_of_msfes <- list()
  for (i in 1:nrow(test)) {
    train[nrow(train) + 1, ] = as.list(test[i,])
    # create a new time series object with the updated data
    temp_model <- VAR(train, p = 1, type='const')
    fitted_values <- fitted(temp_model)
    square_diff <- (as.numeric(fitted_values) - train)^2
    msfe <- sum(square_diff) / nrow(train)

    list_of_msfes <- append(list_of_msfes, msfe)
  }

  total_msfe <- sum(as.data.frame(list_of_msfes))
  mean_msfe <- mean(as.numeric(list_of_msfes))
  dates_for_plot <- seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by="month")
  length(dates_for_plot)
  length(list_of_msfes)
  plot(x = dates_for_plot, y = list_of_msfes, xlab = 'Testing dates', ylab = 'MSFE')

  ggplot(data.frame(list_of_msfes, dates_for_plot), aes(x = as.Date(dates_for_plot),
                 y = as.numeric(list_of_msfes))) +
    # Draw line
    geom_point(size=3) +
    # Change x axis title
    labs(x = "Testing date", y = "MSFE") +
    # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") +
    theme(axis.text.x = element_text(angle = 45))

}
