####### Load Packages ##########

library(dplyr) #data manipulation
library(tidyverse) #data manipulation 
library(tidyr) #data manipulation
library(ggplot2) #data visualisation
library(caret) #ML training 
library(forecast) #time series forecasting, stationarity testing
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

uk_nowcast <- uk_nowcast 
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

