####### Load Packages ##########
source("packages_manager.R")

###### Load Data ########
nowcasting_dataset <- read_excel(
  "230315 Nowcasting Dataset.xlsx",
  sheet = "Nowcasting Dataset",
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
nowcasting_dataset <- nowcasting_dataset[, -c(2, 3, 5)]
nowcasting_dataset$Predictions <- as.double(0)
class(nowcasting_dataset$Predictions)

##### VAR Specification Tests (stationarity, stability) ######

GDP_ADF <- nowcasting_dataset %>% # subset GDP time series
  select(GDP_QNA_RG, Date)

GDP_ADF <- drop_na(GDP_ADF) # remove na's

adf.test(GDP_ADF$GDP_QNA_RG) # test for stationarity = reject null of non stationarity

normality.test(temp_model) # Jarque-Bera, skewness and kurtosis test

stab.temp <- stability(temp_model, type = "OLS-MOSUM", h = 0.95) # stability testing VAR
plot(stab.temp)

arch.test(temp_model)

# Running ADF tests for stationarity using manually selected VAR components
# As data is detrended, excluding constant/drift

var_adf_nc <- list(
  GDP = ur.df(nowcasting_dataset$GDP_QNA_RG, type = "none", selectlags = c("BIC")),
  CPI = ur.df(nowcasting_dataset$CPI_ALL, type = "none", selectlags = c("BIC")),
  BANK = ur.df(nowcasting_dataset$BANK_RATE, type = "none", selectlags = c("BIC")),
  UNE = ur.df(nowcasting_dataset$UNEMP_RATE, type = "none", selectlags = c("BIC")),
  IOP = ur.df(nowcasting_dataset$IOP_PROD, type = "none", selectlags = c("BIC"))
)

summary(var_adf_nc$GDP)
summary(var_adf_nc$CPI)
summary(var_adf_nc$BANK)
summary(var_adf_nc$UNE)
summary(var_adf_nc$IOP)


###### Process data ######

# Identify the column with missing values
colSums(is.na(nowcasting_dataset))

# Calculate the mean of the column
column_mean <- mean(nowcasting_dataset$LIBOR_3mth, na.rm = TRUE)

# Replace the missing values with the mean
nowcasting_dataset$LIBOR_3mth <- ifelse(is.na(nowcasting_dataset$LIBOR_3mth),
  column_mean, nowcasting_dataset$LIBOR_3mth
)

# Linearly interpolate GDP values
nowcasting_dataset$GDP_QNA_RG <-
  na.approx(nowcasting_dataset$GDP_QNA_RG)

# Split dataset to train and test sub samples
train <-
  subset(nowcasting_dataset[, c(2, 4, 11, 19, 36)], subset = nowcasting_dataset$Date <= "2015-12-01")
test <-
  subset(nowcasting_dataset[, c(2, 4, 11, 19, 36)], subset = nowcasting_dataset$Date >= "2016-01-01")

# The output of VARselect tells us what lag length we should use
VARselect(train, lag.max = 10, type = "const")

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
# nowcasting_dataset[nrow(train) + 1, 'Predictions'] = prediction,]

# Calculate MSFE. SUM(residuals^2) / N

# Create new dataframe called msfe_df and import dataset
msfe_df <- read_excel(
  "230315 Nowcasting Dataset.xlsx",
  sheet = "Nowcasting Dataset",
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
msfe_df <- msfe_df[, -c(2, 3, 5)]

# Removes last row of nowcasting_dataset (not sure why there was a prediction past the final date)
# nowcasting_dataset <- nowcasting_dataset[-nrow(nowcasting_dataset),]

# Appends the predictions column from nowcasting_dataset to msfe_df
msfe_df$Predictions <- nowcasting_dataset$Predictions

# Removes all columns from datafraame except Date, GDP Growth and GDP Growth predictions
msfe_df <- subset(msfe_df,
  select = c("Date", "GDP_QNA_RG", "Predictions"),
  subset = nowcasting_dataset$Date >= "2016-01-01"
)

# Replaces NA values in GDP column with the next non-missing value
msfe_df <- na.locf(msfe_df, fromLast = TRUE)

# Uses the new complete panel to calculated MSFE for VAR model
msfe <-
  sum((as.numeric(msfe_df$Predictions) - msfe_df$GDP_QNA_RG)^2) / nrow(msfe_df)

##### Plot predictions and observations #####

# Initiate an array of monthly dates from 2011 to 2018
dates_for_plot <-
  seq(as.Date("2016-01-01"), as.Date("2022-09-01"), by = "month")

# Put predictions and an array of dates into a dataframe
predictions_df <- data.frame(list_of_predictions, dates_for_plot)

# Color selection
colors <-
  c(
    "Predictions" = "dark green",
    "Observations" = "steelblue"
  )

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
  theme(axis.text.x = element_text(angle = 45))
