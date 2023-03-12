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


uk_nowcast <- read_excel("uncertain-kingdom-nowcasting-gdp-and-its-revisions-dataset.xlsx", 
                         sheet = "staticVintage", na = 'NA', range = "A2:AJ350", col_types = c("date", 
                                                                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                                   "numeric", "numeric", "numeric", "numeric", "numeric"))
rownames(uk_nowcast) <- uk_nowcast$...1

train <- subset(uk_nowcast[, 34], subset = uk_nowcast$`...1` <= '2010-12-01')
test <- subset(uk_nowcast[, 34], subset = uk_nowcast$`...1` >= '2011-01-01')

train_omitted <- na.omit(train)
test_omitted <- na.omit(test)

# Peform ADF test, p-value needs to be less than 0.05 for stationarity
adf.test(ts(train_omitted))

# Make elements in the vector numeric
# gdp <- unlist(gdp)

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
  print(nrow(train_omitted))
  # create a new time series object with the updated data
  temp_model <- arima(train_omitted, order = c(2, 0, 0))
  fitted_values <- fitted(temp_model)
  square_diff <- (as.numeric(fitted_values) - train_omitted)^2
  msfe <- sum(square_diff) / nrow(train_omitted)
  list_of_msfes <- append(list_of_msfes, msfe)
}

total_msfe <- sum(as.data.frame(list_of_msfes))

dates_for_plot <- seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by="quarter")
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
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") + 
  theme(axis.text.x = element_text(angle = 45))
