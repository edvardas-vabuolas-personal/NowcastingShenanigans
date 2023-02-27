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