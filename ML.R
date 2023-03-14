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
library(vars)
library(doParallel) # Multi-threading
library("ggpubr") # Allows combining multiple graphs

###### Load data ######
nowcasting_dataset <- read_excel(
  "230312 Nowcasting Dataset.xls",
  col_types = c(
    "numeric",
    "date",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)
rownames(nowcasting_dataset) <- nowcasting_dataset$Date

# Linearly interpolate GDP values
nowcasting_dataset$UK_GDP_4 <-
  na.approx(nowcasting_dataset$UK_GDP_4)

# Split dataset into train and test partitions
train_set <-
  subset(nowcasting_dataset[,-c(1, 2)], subset = nowcasting_dataset$Date <= '2010-12-01')
test_set <-
  subset(nowcasting_dataset[,-c(1, 2)], subset = nowcasting_dataset$Date >= '2011-01-01')

#### Creating sampling seeds for reproducibility ####
set.seed(123)
seeds <- vector(mode = "list", length = 88)
for (i in 1:527)
  seeds[[i]] <- sample.int(1000, 15)

# Enable multi-threading with three cores
registerDoParallel(cores = 3)

# Train controller. 250 train sample, growing window, 1 step ahead forecast
myTimeControl <- trainControl(
  method = "timeslice",
  initialWindow = 250,
  horizon = 1,
  fixedWindow = FALSE,
  allowParallel = TRUE,
  savePredictions = 'final',
  verbose = TRUE,
  seeds = seeds
)

##### Elastic Net #####
elastic_net <- train(
  UK_GDP_4 ~ .,
  data = nowcasting_dataset[,-c(1, 2)],
  method = "glmnet",
  family = "gaussian",
  trControl = myTimeControl,
  tuneLength = 15,
  metric = 'RMSE'
)

#### Ridge #####
# https://daviddalpiaz.github.io/r4sl/elastic-net.html
ridge <- train(
  UK_GDP_4 ~ .,
  data = nowcasting_dataset[,-c(1, 2)],
  method = "glmnet",
  family = "gaussian",
  trControl = myTimeControl,
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 1, 0.005)),
  metric = 'RMSE'
)

#### Lasso ####
lasso <- train(
  UK_GDP_4 ~ .,
  data = nowcasting_dataset[,-c(1, 2)],
  method = "glmnet",
  family = "gaussian",
  trControl = myTimeControl,
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, 0.005)),
  metric = 'RMSE'
)

#### Lists of predictions for each model ####
elastic_net_list_of_predictions <- elastic_net$pred$pred
ridge_list_of_predictions <- ridge$pred$pred
lasso_list_of_predictions <- lasso$pred$pred

#### Calculate MSFEs for each model ####
elastic_net_msfe <-
  sum((
    as.numeric(elastic_net_list_of_predictions) - test_set$UK_GDP_4
  ) ^ 2) / nrow(test_set)
ridge_msfe <-
  sum((as.numeric(ridge_list_of_predictions) - test_set$UK_GDP_4) ^ 2) / nrow(test_set)
lasso_msfe <-
  sum((as.numeric(lasso_list_of_predictions) - test_set$UK_GDP_4) ^ 2) / nrow(test_set)

#### Plot predictions and observations ####

# Initiate an array of monthly dates from 2011 to 2018
dates_for_plot <-
  seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by = "month")

# Color selection
colors <- c("Predictions" = "steelblue",
            "Observations" = "grey")

# Set graphs legend to the top
theme_set(theme_bw() +
            theme(legend.position = "top"))

### Elastic net graph ###

# Put predictions and an array of dates into a dataframe
elastic_net_predictions_df <-
  data.frame(elastic_net_list_of_predictions, dates_for_plot)

# Plot
elastic_net_plot <- ggplot() +
  # Draw predictions line
  geom_line(
    data = elastic_net_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(elastic_net_list_of_predictions),
      color = "Predictions"
    ),
    size = 1
  ) +
  
  # Draw observations line
  geom_line(
    data = test_set[, 1],
    aes(
      x = as.Date(dates_for_plot),
      y = UK_GDP_4,
      color = "Observations"
    ),
    size = 1
  ) +
  
  # Change x and y titles
  labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
  
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  
  # Apply colours
  scale_color_manual(values = colors) +
  
  # Rotate x axis label by 45 degrees
  theme(axis.text.x = element_text(angle = 45)) +
  
  # Add MSFE to the graph
  annotate(
    geom = "text",
    x = as.Date("2016-12-01"),
    y = -0.2,
    label = paste0("MSFE: ", round(elastic_net_msfe, digits = 5))
  )

### Ridge graph ###
ridge_predictions_df <-
  data.frame(ridge_list_of_predictions, dates_for_plot)

# Plot
ridge_plot <- ggplot() +
  
  # Draw predictions line
  geom_line(
    data = ridge_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(ridge_list_of_predictions),
      color = "Predictions"
    ),
    size = 1
  ) +
  # Draw observations line
  geom_line(
    data = test_set[, 1],
    aes(
      x = as.Date(dates_for_plot),
      y = UK_GDP_4,
      color = "Observations"
    ),
    size = 1
  ) +
  
  # Change x and y titles
  labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
  
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  
  # Apply colours
  scale_color_manual(values = colors) +
  
  # Rotate x axis label by 45 degrees
  theme(axis.text.x = element_text(angle = 45)) +
  
  # Add MSGE to the graph
  annotate(
    geom = "text",
    x = as.Date("2016-12-01"),
    y = -0.2,
    label = paste0("MSFE: ", round(ridge_msfe, digits = 5))
  )

### Lasso graph ###
lasso_predictions_df <-
  data.frame(lasso_list_of_predictions, dates_for_plot)

# Plot
lasso_plot <- ggplot() +
  
  # Draw predictions line
  geom_line(
    data = lasso_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(lasso_list_of_predictions),
      color = "Predictions"
    ),
    size = 1
  ) +
  
  # Draw observations line
  geom_line(
    data = test_set[, 1],
    aes(
      x = as.Date(dates_for_plot),
      y = UK_GDP_4,
      color = "Observations"
    ),
    size = 1
  ) +
  
  # Change x and y titles
  labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
  
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  
  # Apply colours
  scale_color_manual(values = colors) +
  
  # Rotate x axis labels by 45 degrees
  theme(axis.text.x = element_text(angle = 45)) +
  
  # Add MSFE to the graph
  annotate(
    geom = "text",
    x = as.Date("2016-12-01"),
    y = -0.2,
    label = paste0("MSFE: ", round(lasso_msfe, digits = 5))
  )

# Put all graphs together into a single figure
figure <- ggarrange(
  elastic_net_plot,
  ridge_plot,
  lasso_plot,
  labels = c("Elastic Net", "Ridge", "Lasso"),
  ncol = 2,
  nrow = 2
)

# Display figure
figure

show_variables_summary <- FALSE
if (show_variables_summary == TRUE) {
  # Obtain coefficients
  coef(lasso$finalModel, lasso$bestTune$lambda)
  coef(ridge$finalModel, ridge$bestTune$lambda)
  
  # Plot importance of variables
  var_importance <- varImp(ridge)
  plot(var_importance)
  
  var_importance <- varImp(lasso)
  plot(var_importance)
  
  var_importance <- varImp(elastic_net)
  plot(var_importance)
  
  # List Tuning parameters
  elastic_net$bestTune
  lasso$bestTune
  ridge$bestTune
}
