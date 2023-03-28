####### Load Packages ##########
source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

# Set TRUE to enable Latex export (very slow)
TEX <- TRUE

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

  ###### Load Data ########
  data <- load_data(
    dataset_end_date = dataset_end_date,
    interpolate = TRUE
  )

  columns <- c(2, 4, 11, 19, 36)
  for (i in seq_along(structural_breakpoints)) {
    data[, paste0("Break_", i)] <- ifelse(seq_len(nrow(data)) < structural_breakpoints[i], 0, 1)
    columns <- append(columns, 50 + i)
  }
  ###### Process data ######
  # Split dataset to train and test sub samples
  train <-
    subset(
      data[, columns],
      subset = data$Date <= train_end_date
    )
  test <-
    subset(
      data[, columns],
      subset = data$Date >= test_start_date
    )

  # The output of VARselect tells us what lag length we should use
  # var_select <- VARselect(train, lag.max = 10, type = "const")

  lag <- 2
  # For each row in the test sub sample
  for (i in 1:nrow(test)) {
    # Obtain coefficients for VAR(1) lag length
    temp_model <- VAR(
      y = train[, c(1, 2, 3, 4, 5)],
      p = lag,
      type = "const",
      exogen = as.matrix(train[, -c(1, 2, 3, 4, 5)])
    )

    # Forecast one step ahead; feed one observation from test sub sample
    forecast_object <-
      predict(
        object = temp_model,
        ci = 0.95,
        n.ahead = 1,
        dumvar = as.matrix(train[i, -c(1, 2, 3, 4, 5)])
      )
    prediction <- forecast_object$fcst$GDP[, 1]
    # Append train sub sample with one observation from the test sub sample
    data[nrow(train) + 1, "Predictions"] <- prediction
    train[nrow(train) + 1, ] <- as.list(test[i, ])
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

  # Replaces NA values in GDP column with the next non-missing value
  msfe_df <- na.locf(msfe_df, fromLast = TRUE)

  # # Uses the new complete panel to calculated MSFE for VAR model
  msfe <- calculate_msfe(
    predictions = msfe_df$Predictions,
    oos = msfe_df$GDP
  )
  print(msfe)

  # ##### Plot predictions and observations #####

  msfe_df$Date <- as.Date(msfe_df$Date)
  # Initiate an array of quarterly dates from 2011 to 2018
  dates_for_plot <-
    seq(as.Date(min(msfe_df$Date)), as.Date(max(msfe_df$Date)), by = "month")

  # Plot
  var_plot <- make_plot(
    dates = dates_for_plot,
    predictions = msfe_df$Predictions,
    observations = msfe_df$GDP,
    msfe = msfe,
    label = glue("VAR({lag}) {year}; {length(structural_breakpoints)} Structural Break(s); NOCB Interpolation"),
    scale_y = c(-40, 20)
  )
  # ar_plot
  export_latex("plot", "var", year, var_plot, height = 3, TEX = TEX)

  names(msfe_df)[3] <- c(glue("VAR({lag}}) {year}"))

  predictions <- merge(predictions, msfe_df[, c(1, 3)], by = "Date", all = TRUE)
}
predictions$Date <- format(predictions$Date, "%d-%m-%Y")
export_latex("table", "var", year, predictions, TEX = TEX)
