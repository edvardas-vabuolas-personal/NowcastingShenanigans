source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")
source("data_visualisation.R")
TEX = TRUE
# START: Diagnostic checks, lag selection and structural break identification
data <- load_data(interpolate=FALSE)
data <- na.omit(data)
# Plot time-series of GDP growth
gdp_plot <- ggplot() +
    geom_line(
        data = data,
        aes(x = Date, y = GDP)
    )
export_latex("plot", "gdp", "", gdp_plot)

box_plots(data)

# Summary table
data <- load_data(interpolate=FALSE)
stat_desc <- stat.desc(data[, -c(1)])
stat_desc <- stat_desc[c(1:5, 9:10, 12:13), ]

tranposed_stat_desc <- transpose(stat_desc)

rownames(tranposed_stat_desc) <- colnames(stat_desc)
colnames(tranposed_stat_desc) <- c("N", "No. of 0", "No. of NA", "Min", "Max", "Median", "Mean", "Var", "Std. dev.")

tranposed_stat_desc <- cbind(Name = gsub("_", "£", colnames(stat_desc)), tranposed_stat_desc)
export_latex("table", "stat", "", tranposed_stat_desc, TEX = TEX)

# Plot ACF function, needs to be decreasing
acf_plot <- acf(data[, c(2)], lag.max = 20, main = "ACF")
export_latex("plot", "acf", "", acf_plot)

# Perform ADF test, p-value needs to be less than 0.05 for stationarity
data = load_data()
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

data = load_data()
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
# Perform PP test, p-value needs to be less than 0.05 for stationarity

summary(gdp_pp)

# Perform Zandrews test to identify and accomodate for a structural break
gdp_za <- ur.za(data$CPI_ALL, model = c("intercept"), lag = 1)
summary(gdp_za)

# Identify structural breaks
attach(data)
x <- Fstats(GDP ~ 1, from = 0.01) # uses the chow test to generate critical values
sctest(x) # tests for the existence of structural change with H0 = there is no structural change
strucchange::breakpoints(GDP ~ 1) # identifies the number of breakpoints with corresponding observation number

# Initiate a matrix that will store AIC and BIC for each AR lag
info_critera <- matrix(NA, nrow = 10, ncol = 2)

for (p in 1:10) {
    ar_model <- arima(data$GDP, order = c(p, 0, 0))
    info_critera[p, ] <- c(ar_model$aic, ar_model$bic)
}

colnames(info_critera) <- c("AIC", "BIC")
rownames(info_critera) <- paste0("AR", 1:nrow(info_critera))

export_latex('table', 'IC', "", info_critera) # Displays info criterion table for optimal lag length

# END: Diagnostic checks and lag selection
