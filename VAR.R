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

###### Load Data ########
nowcasting_dataset <- read_excel(
  "230315 Nowcasting Dataset.xlsx", sheet = "Nowcasting Dataset",
  col_types = c(
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
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)
nowcasting_dataset <- nowcasting_dataset[,-c(2,3,5)]
nowcasting_dataset$Predictions <- as.double(0)
class(nowcasting_dataset$Predictions)
# rownames(nowcasting_dataset) <- nowcasting_dataset$Date

###### Process data ######

# Linearly interpolate GDP values
nowcasting_dataset$GDP_QNA_RG <-
  na.approx(nowcasting_dataset$GDP_QNA_RG)

# Split dataset to train and test sub samples
train <-
  subset(nowcasting_dataset[, c(2, 4, 11, 19, 36)], subset = nowcasting_dataset$Date <= '2010-12-01')
test <-
  subset(nowcasting_dataset[, c(2, 4, 11, 19, 36)], subset = nowcasting_dataset$Date >= '2011-01-01')

# The output of VARselect tells us what lag length we should use
VARselect(train, lag.max = 10, type = 'const')

###### One step ahead forecast of test sub sample  ######

# Initiate an empty list for predictions
list_of_predictions <- list()

# For each row in the test sub sample
for (i in 1:nrow(test)) {
  # Obtain coefficients for VAR(1) lag length
  temp_model <- VAR(train, p = 1, type = 'const')
  
  # Forecast one step ahead; feed one observation from test sub sample
  one_step_ahead_forecast_object <-
    predict(temp_model, test[i,], n.ahead = 1)
  prediction <- one_step_ahead_forecast_object$fcst$GDP_QNA_RG[, 1]
  # Append train sub sample with one observatin from the test sub sample
  train[nrow(train) + 1,] = as.list(test[i, ])
  nowcasting_dataset[nrow(train) + 1, 'Predictions'] = test[prediction,]
  # Append the list of predictins with the one ahead forecast
  list_of_predictions <-
    append(list_of_predictions, prediction)
}
nowcasting_dataset[nrow(train) + 1, 'Predictions'] = test[prediction,]
# Calculate MSFE. SUM(residuals^2) / N
msfe <-
  sum((as.numeric(list_of_predictions) - test$UK_GDP_4) ^ 2) / nrow(test)

##### Plot predictions and observations #####

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
      y = test$UK_GDP_4,
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
  
  # Rotate x axis labels by 45 degrees
  theme(axis.text.x = element_text(angle = 45))
