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

nowcasting_dataset$UK_GDP_4 <- na.approx(nowcasting_dataset$UK_GDP_4)

train <- subset(nowcasting_dataset[, c(3, 5, 12, 20, 37)], subset = nowcasting_dataset$Date <= '2010-12-01')
test <- subset(nowcasting_dataset[, c(3, 5, 12, 20, 37)], subset = nowcasting_dataset$Date >= '2011-01-01')

# VARselect(train, lag.max = 10, type = 'const')

list_of_predictions <- list()
for (i in 1:nrow(test)) {
  # create a new time series object with the updated data
  temp_model <- VAR(train, p = 1, type='const')
  one_step_ahead_forecast <- predict(temp_model, test[i, ], n.ahead = 1)
  train[nrow(train) + 1, ] = as.list(test[i,])
  list_of_predictions <- append(list_of_predictions, one_step_ahead_forecast$fcst$UK_GDP_4[, 1])
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
                         y = test$UK_GDP_4, color = "Observations"), 
             size = 1) +
  # Change x axis title
  labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +
  scale_color_manual(values = colors) + 
  theme(axis.text.x = element_text(angle = 45))
