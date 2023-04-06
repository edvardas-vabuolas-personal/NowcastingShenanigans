source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")

# Set this to TRUE for LaTeX exports.
# Note: VERY SLOW and overrides files in output
TEX <- TRUE

# A simple GDP plot
GDP_PLOT <- FALSE
if (GDP_PLOT == TRUE) {
  data <- load_data(interpolate = FALSE)
  data <- na.omit(data)
  # Plot time-series of GDP growth
  gdp_plot <- ggplot() +
    geom_line(
      data = data,
      aes(x = Date, y = GDP)
    )
  export_latex("plot", "gdp", "", gdp_plot)
}

# This produces a box plot for each of the 49 variables
BOX_PLOTS <- FALSE
if (BOX_PLOTS == TRUE) {
  box_plots(data)
}

# This produces a table with statistics about each of the 49 variables
SUMMARY_TABLE <- FALSE
if (SUMMARY_TABLE) {
  # Summary table
  data <- load_data(interpolate = FALSE)
  stat_desc <- stat.desc(data[, -c(1)])
  stat_desc <- stat_desc[c(1:5, 9:10, 12:13), ]

  tranposed_stat_desc <- transpose(stat_desc)

  rownames(tranposed_stat_desc) <- colnames(stat_desc)
  colnames(tranposed_stat_desc) <- c("N", "No. of 0", "No. of NA", "Min", "Max", "Median", "Mean", "Var", "Std. dev.")

  tranposed_stat_desc <- cbind(Name = gsub("_", "£", colnames(stat_desc)), tranposed_stat_desc)
  export_latex("table", "stat", "", tranposed_stat_desc, TEX = TEX)
}

# This produces ACF plot for GDP
ACF_PLOT <- FALSE
if (ACF_PLOT == TRUE) {
  # Plot ACF function, needs to be decreasing
  acf_plot <- acf(data[, c(2)], lag.max = 20, main = "ACF")
  export_latex("plot", "acf", "", acf_plot, width = 5.3, height = 4)
}

# This performs ADF test on each of the 49 variables
ADF_TESTS <- FALSE
if (ADF_TESTS == TRUE) {
  # Perform ADF test, p-value needs to be less than 0.05 for stationarity
  data <- load_data()
  for (col in 1:ncol(data)) {
    if (col == 1) {
      message("Date variable, skipping")
      next
    }
    vector <- data[, c(col)]
    vector <- na.omit(vector)
    adf_object <- adf.test(ts(vector))
    if (adf_object$p.value > 0.05) {
      message(glue("Column with index {col} is non-stationary with p-value {adf_object$p.value}"))
    }
  }
}

# This performs PP test on each of the 49 variables
PP_TESTS <- FALSE
if (PP_TESTS == TRUE) {
  # Perform PP test, p-value needs to be less than 0.05 for stationarity
  data <- load_data()
  for (col in 1:ncol(data)) {
    if (col == 1) {
      message("Date variable, skipping")
      next
    }
    vector <- data[, c(col)]
    vector <- na.omit(vector)
    gdp_pp <- ur.pp(
      ts(vector),
      type = "Z-tau",
      model = "constant",
      lags = "short",
      use.lag = NULL
    )
    if (gdp_pp@teststat > min(gdp_pp@cval)) {
      message(glue("Column with index {col} is non-stationary"))
    }
  }
}

# This performs ZA test on VAR variables
GDP_ZA_TEST <- FALSE
if (GDP_ZA_TEST == TRUE) {
  # Perform Zandrews test to identify and accomodate for a structural break
  gdp_za <- ur.za(data$GDP, model = c("intercept"), lag = 1)
  summary(gdp_za)
  gdp_za <- ur.za(data$UNEMP_RATE, model = c("intercept"), lag = 1)
  summary(gdp_za)
  gdp_za <- ur.za(data$CPI_ALL, model = c("intercept"), lag = 1)
  summary(gdp_za)
}

# This produces a table of IC for AR
IC <- TRUE
if (IC == TRUE) {
  data <- load_data(interpolate = FALSE)
  data <- na.omit(data)
  # Initiate a matrix that will store AIC and BIC for each AR lag
  info_critera <- matrix(NA, nrow = 10, ncol = 2)

  for (p in 1:10) {
    # AIC(ar_model, k = log(length(data$GDP))) is BIC

    ar_model <- arima(data$GDP, order = c(p, 0, 0))
    info_critera[p, ] <- c(
      AIC(ar_model),
      AIC(ar_model, k = log(length(data$GDP)))
    )
  }

  colnames(info_critera) <- c("AIC", "BIC")
  info_critera <- cbind(Lag = 1:nrow(info_critera), info_critera)

  export_latex("table", glue("ar_ic"), "2022", info_critera, TEX = TEX)
}

# This produces a table of IC for VAR
VAR_LAG_SELECTION <- FALSE
if (VAR_LAG_SELECTION == TRUE) {
  data <- load_data(
    dataset_end_date = FALSE,
    interpolate = FALSE
  )
  data <- na.omit(data)
  columns <- c(2, 4, 11, 19, 36)
  var_select <- VARselect(data[, columns], lag.max = 10, type = "const")

  transposed <- t(var_select$criteria)
  transposed <- cbind(Lag = 1:nrow(transposed), transposed)
  export_latex("table", glue("var_select_interpolated"), "2022", transposed, TEX = TEX)
}
