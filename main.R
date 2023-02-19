#Test
source('process_data.R')
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)

######### Data import ############
# Data used: OECD Economic Outlook for the UK, 1979Q1-2020Q1
# https://stats.oecd.org/index.aspx?queryid=51396 (unfiltered)

# OECD does not let download all data in a single file, so we download two files and merge them
detailed_economic_outlook <- merge(
  read.csv(file = './oecd_data/economic_outlook1.csv', head = T),
  read.csv(file = './oecd_data/economic_outlook2.csv', head = T),
  all = TRUE
)

detailed_monetary_data <- read.csv('./oecd_data/monetary_stats.csv', head = T)

detailed_unemployment <- read.csv('./oecd_data/unemployment_data.csv', head = T)

detailed_monthly_inflation <- read.csv('./oecd_data/monthly_inflation.csv', head = T)

detailed_production_net_export <- read.csv('./oecd_data/production_net_export.csv', head = T)

ready_economic_outlook <- process_data(detailed_economic_outlook, 166, 'Variable')

ready_monetary_data <- process_data(detailed_monetary_data, 481, 'Subject')

ready_unemployment_data <- process_data(detailed_unemployment, 433, 'Subject')

ready_monthly_inflation <- process_data(detailed_monthly_inflation, 481, 'Subject')

ready_production_net_export <- process_data(detailed_production_net_export, 481, 'Subject')

ready_economic_outlook$Time <-
  str_replace(ready_economic_outlook$Time, 'Q1', '03')
ready_economic_outlook$Time <-
  str_replace(ready_economic_outlook$Time, 'Q2', '06')
ready_economic_outlook$Time <-
  str_replace(ready_economic_outlook$Time, 'Q3', '09')
ready_economic_outlook$Time <-
  str_replace(ready_economic_outlook$Time, 'Q4', '12')

ready_datasets <- list(
  ready_economic_outlook,
  ready_monetary_data,
  ready_unemployment_data,
  ready_monthly_inflation,
  ready_production_net_export
)

monthly_dataset <- 
  ready_datasets %>% reduce(full_join, by = 'Time')
quarterly_dataset <-
  ready_datasets %>% reduce(inner_join, by = 'Time')
