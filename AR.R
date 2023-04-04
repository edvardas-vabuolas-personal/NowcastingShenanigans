###### Load Data ########

setwd("/Users/edvardasvabuolas/NowcastingShenanigans")

source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

###### Load Data ########

# Set TRUE to enable Latex export (very slow)
TEX <- TRUE

STR_BREAKS <- FALSE

STR_B <- if (STR_BREAKS == TRUE) "w_str_b" else "wout_str_b"

INTERVALS <- get_intervals()
predictions <- data.frame(
  seq(as.Date("2006-01-01"), as.Date("2022-09-01"),
    by = "month"
  )
)
names(predictions)[1] <- "Date"

for (year in c(2010, 2019, 2022)) {
  dataset_end_date <- as.character(INTERVALS[[year]]["dataset_end_date"])
  train_end_date <- as.character(INTERVALS[[year]]["train_end_date"])
  test_start_date <- as.character(INTERVALS[[year]]["test_start_date"])
  structural_breakpoints <- as.list(INTERVALS[[(paste0(year, ".break_points"))]])
  
  structural_breakpoints <- if (STR_BREAKS == TRUE) structural_breakpoints else c()
  
  data <- load_data(
    dataset_end_date = dataset_end_date
  )

  columns <- c(1, 2)
  if (STR_BREAKS == TRUE) {
    for (i in seq_along(structural_breakpoints)) {
      data[, paste0("Break_", i)] <- ifelse(seq_len(nrow(data)) < structural_breakpoints[i], 0, 1)
      columns <- append(columns, 50 + i)
    }
  }
  # Split data into train and test partitions
  train <-
    subset(data[, columns], subset = data$Date <= train_end_date)
  test <-
    subset(data[, columns], subset = data$Date >= test_start_date)

  # Drop NA values
  train <- na.omit(train)
  test <- na.omit(test)

  train[, "L1GDP"] <- lag(train[, "GDP"], n = 1)
  test[, "L1GDP"] <- lag(test[, "GDP"], n = 1)
  train[, "L2GDP"] <- lag(train[, "GDP"], n = 2)
  test[, "L2GDP"] <- lag(test[, "GDP"], n = 2)

  train <- na.omit(train)
  test <- na.omit(test)

  # START: One step ahead forecast of test sub sample

  ###### STRUCTURAL BREAK MODEL ######
  for (i in 1:nrow(test)) {
    if (length(structural_breakpoints) == 2) {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP + Break_1 + Break_2, data = train)
    } else if (length(structural_breakpoints) == 1) {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP + Break_1, data = train)
    } else {
      temp_model_sb <- lm(GDP ~ L1GDP + L2GDP, data = train)
    }
    prediction <- temp_model_sb$fitted.values[nrow(train) - 3]
    data[data$Date == max(train$Date), "Predictions"] <- prediction
    train[nrow(train) + 1, ] <- test[i, ]
  }
  # # Create new dataframe called msfe_df and import dataset
  msfe_df <- load_data(dataset_end_date = dataset_end_date)

  # # Appends the predictions column from data to msfe_df
  msfe_df$Predictions <- data$Predictions

  # # Removes all columns from datafraame except Date, GDP Growth and GDP Growth predictions
  msfe_df <- subset(msfe_df,
    select = c("Date", "GDP", "Predictions"),
    subset = data$Date >= test_start_date
  )
  plot_df <- na.omit(msfe_df)
  
  # Replaces NA values in GDP column with the next non-missing value
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # Calculate MSFE. SUM(residuals^2) / N
  msfe <- calculate_msfe(
    predictions = msfe_df$Predictions,
    oos = msfe_df$GDP
  )
  message(glue("AR(2) MSFE ({year}): {msfe}"))

  # END: One step ahead forecast of test sub sample

  plot_df$Date <- as.Date(plot_df$Date)
  # Initiate an array of quarterly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date(min(plot_df$Date)), as.Date(max(plot_df$Date)), by = "month")

  # Plot
  ar_plot <- make_plot(
    dates = plot_df$Date,
    predictions = plot_df$Predictions,
    observations = plot_df$GDP,
    msfe = msfe,
    label = glue("AR(2) {year}; {length(structural_breakpoints)} Structural Break(s)"),
    scale_y = c(-40, 20)
  )
  # ar_plot
  export_latex("plot", glue("ar_{STR_B}"), year, ar_plot, height = 3, TEX = TEX)

  names(msfe_df)[3] <- c(glue("AR(2) {year}"))

  predictions <- merge(predictions, msfe_df[, c(1, 3)], by = "Date", all = TRUE)
}

predictions$Date <- format(predictions$Date, "%d-%m-%Y")
export_latex("table", glue("ar_{STR_B}"), year, predictions, TEX = TEX)
