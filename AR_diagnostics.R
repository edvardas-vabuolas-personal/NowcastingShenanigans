source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")

# START: Diagnostic checks, lag selection and structural break identification
data <- load_data()
data <- na.omit(data)
# Plot time-series of GDP growth
gdp_plot <- ggplot() +
    geom_line(
        data = data,
        aes(x = Date, y = GDP)
    )
export_latex("plot", "gdp", "", gdp_plot)

# Plot ACF function, needs to be decreasing
acf_plot <- acf(data[, c(2)], lag.max = 20, main = "ACF")
export_latex("plot", "acf", "", acf_plot)

# Perform ADF test, p-value needs to be less than 0.05 for stationarity
adf.test(ts(data$GDP))

# Perform PP test, p-value needs to be less than 0.05 for stationarity
gdp_pp <- ur.pp(
    data$GDP,
    type = "Z-tau",
    model = "constant",
    lags = "short",
    use.lag = NULL
)
summary(gdp_pp)

# Perform Zandrews test to identify and accomodate for a structural break
gdp_za <- ur.za(data$GDP, model = c("intercept"), lag = 1)
summary(gdp_za)

# Identify structural breaks
attach(data)
x <- Fstats(GDP ~ 1, from = 0.01) # uses the chow test to generate critical values
x
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
