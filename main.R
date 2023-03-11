####### Load Packages ##########
install.packages("caret")
library(dplyr) #data manipulation
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

###### Construct Data Set (UK-MD) ########

balanced_uk_md <- read_csv("balanced_uk_md.csv") # Balanced UK-MD data from 1998

tr_uk_md <- read_csv("tr_uk_md.csv") # Complete transformed UK-MD data from 1948

head(balanced_uk_md) 

###### Construct Data Set (Uncertain Kingdom) #######

uk_nowcast <- read_excel("uncertain-kingdom-nowcasting-gdp-and-its-revisions-dataset.xlsx"
                         ,sheet = "staticVintage")

uk_nowcast <- uk_nowcast[0:349, ]

gdp <- uk_nowcast$GDP4
ts_gdp <- ts(gdp, start=c(1990,1), end=c(2018,12), frequency=12)
interp <- approx(x = time(ts_gdp), y = ts_gdp, xout = time(ts_gdp))
wout_nas <- na.omit(interp$y)
numeric_gdp <- unlist(wout_nas)
adf.test(numeric_gdp)
acf(numeric_gdp, lag.max = 20, main='ACF')

ar_model <- ar(numeric_gdp, order.max = 1)
ar_model <- ar(numeric_gdp, order.max = 2)

info_critera <- matrix(NA, nrow=20, ncol=2)

for (p in 1:20) {
  ar_model = arima(numeric_gdp, order = c(p, 0, 0))
  info_critera[p, ] <- c(ar_model$aic, ar_model$bic)
}
colnames(info_critera) <- c("AIC", "BIC")
rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))
print(info_critera)


gdp <- as.data.frame(uk_nowcast$GDP4)
gdp_q <- as.data.frame(gdp[-1, ])
print(gdp_q)
class(gdp_q)
wout_nas <- na.omit(gdp_q)
print(wout_nas)
ts_gdp <- ts(wout_nas, start=c(1990,4), end=c(2018,12), frequency=4)
print(ts_gdp)

adf.test(ts_gdp)
unlisted_gdp <- unlist(ts_gdp)

acf(unlisted_gdp, lag.max = 20, main='ACF')

info_critera <- matrix(NA, nrow=10, ncol=2)

for (p in 1:10) {
  ar_model = arima(unlisted_gdp, order = c(p, 0, 0), method='ML')
  info_critera[p, ] <- c(ar_model$aic, ar_model$bic)
}
colnames(info_critera) <- c("AIC", "BIC")
rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))
print(info_critera)

ts_gdp$fitted_vals <- fitted(arima(unlisted_gdp, order = c(2, 0, 0)))
fitted_df <- as.data.frame(fitted_vals)
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line()
fitted_vals

###### Construct Data Set (OECD) ########

source("load_data.R")

data <- load_data();
monthly_dataset = data[['monthly']]; quarterly_dataset = data[['quarterly']];

VAR_variables <- monthly_dataset %>% 
  select(c("Time", "GDP_ANNPCT", "Relative consumer price indices","Short-term interest rates, Per cent per annum", "LRHUTTTT", "Share Prices, Index"))

VAR_table <- as_tibble(VAR_variables) %>% 
  rename(
    time = Time,
    GDP_Growth = GDP_ANNPCT,
    Inflation = 'Relative consumer price indices',
    Interest_rate = 'Short-term interest rates, Per cent per annum',
    Unemployment_Rate = LRHUTTTT,
    Stock_market_index = 'Share Prices, Index'
  )

AR_table <- VAR_table %>%
  select(c(time, GDP_Growth)) 

numeric_AR_ts <- unlist(ts(AR_table$GDP_Growth, start = c(1979, 1), frequency = 4))

adf.test(numeric_AR_ts)


plot(numeric_AR_ts, main="GDP")


