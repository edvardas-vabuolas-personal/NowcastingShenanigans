####### Load Packages ##########
source("packages_manager.R")
source("load_data.R")
source("helper_functions.R")

###### Load Data ########

nowcasting_dataset <- load_data(
  dataset_end_date = "2010-12-01",
  interpolate = TRUE
)

#   subset(nowcasting_dataset, subset = nowcasting_dataset$Date <= '2019-12-01')
train_set <-
  subset(nowcasting_dataset[, -c(1)], subset = nowcasting_dataset$Date <= "2005-12-01")
test_set <-
  subset(nowcasting_dataset[, -c(1)], subset = nowcasting_dataset$Date >= "2006-01-01")

#### Creating sampling seeds for reproducibility ####
set.seed(123)
seeds <- get_seeds()

# Enable multi-threading with three cores
registerDoParallel(cores = 3)

# Train controller. 250 train sample, growing window, 1 step ahead forecast
myTimeControl <- trainControl(
  method = "timeslice",
  initialWindow = 200,
  horizon = 1,
  fixedWindow = FALSE,
  allowParallel = TRUE,
  savePredictions = "final",
  verbose = TRUE,
  seeds = seeds
)

##### Elastic Net #####

# Intial run to obtain hyperparameters
elastic_net <- train(
  GDP_QNA_RG ~ .,
  data = nowcasting_dataset[, -c(1)],
  method = "glmnet",
  family = "gaussian",
  trControl = myTimeControl,
  tuneLength = 15,
  metric = "RMSE"
)

en_pred <- list()

# For each row in the test sub sample (expanding window)
for (i in 1:nrow(test_set)) {
  elastic_net_temp <- train(
    GDP_QNA_RG ~ .,
    data = train_set,
    method = "glmnet",
    family = "gaussian",
    tuneLength = 1,
    tuneGrid = expand.grid(alpha = elastic_net$bestTune$alpha, lambda = elastic_net$bestTune$lambda),
    metric = "RMSE"
  )
  test_pred_en <- predict(elastic_net_temp, newdata = test_set[i, ])
  # Update train sub sample with one row from test sub sample
  train_set[nrow(train_set) + 1, ] <- test_set[i, ]

  # Store prediction in the predictions list
  en_pred <-
    append(en_pred, test_pred_en)
}

#### Ridge #####
# https://daviddalpiaz.github.io/r4sl/elastic-net.html

# Initial run to obtain hyperparameters
ridge <- train(
  GDP_QNA_RG ~ .,
  data = nowcasting_dataset[, -c(1)],
  method = "glmnet",
  family = "gaussian",
  trControl = myTimeControl,
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 1, 0.005)),
  metric = "RMSE"
)

r_pred <- list()

# For each row in the test sub sample (expanding window)
for (i in 1:nrow(test_set)) {
  ridge_temp <- train(
    GDP_QNA_RG ~ .,
    data = train_set,
    method = "glmnet",
    family = "gaussian",
    tuneLength = 1,
    tuneGrid = expand.grid(alpha = ridge$bestTune$alpha, lambda = ridge$bestTune$lambda),
    metric = "RMSE"
  )
  test_pred_r <- predict(ridge_temp, newdata = test_set[i, ])
  # Update train sub sample with one row from test sub sample
  train_set[nrow(train_set) + 1, ] <- test_set[i, ]

  # Store prediction in the predictions list
  r_pred <-
    append(r_pred, test_pred_r)
}

#### Lasso ####
lasso <- train(
  GDP_QNA_RG ~ .,
  data = nowcasting_dataset[, -c(1)],
  method = "glmnet",
  family = "gaussian",
  trControl = myTimeControl,
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, 0.005)),
  metric = "RMSE"
)

l_pred <- list()

# For each row in the test sub sample (expanding window)
for (i in 1:nrow(test_set)) {
  lasso_temp <- train(
    GDP_QNA_RG ~ .,
    data = train_set,
    method = "glmnet",
    family = "gaussian",
    tuneLength = 1,
    tuneGrid = expand.grid(alpha = lasso$bestTune$alpha, lambda = lasso$bestTune$lambda),
    metric = "RMSE"
  )
  test_pred_l <- predict(lasso_temp, newdata = test_set[i, ])
  # Update train sub sample with one row from test sub sample
  train_set[nrow(train_set) + 1, ] <- test_set[i, ]

  # Store prediction in the predictions list
  l_pred <-
    append(l_pred, test_pred_l)
}

#### Random Forest ####


#### Calculate MSFEs for each model ####

# Create new dataframe called msfe_df and import dataset
msfe_df <- load_data(dataset_end_date = "2010-12-01")

# Appends the predictions column from nowcasting_dataset to msfe_df
msfe_df <- subset(msfe_df,
  select = c("Date", "GDP_QNA_RG"),
  subset = nowcasting_dataset$Date >= "2006-01-01"
)

# Replaces NA values in GDP column with the next non-missing value
msfe_df <- na.locf(msfe_df, fromLast = TRUE)

# Appends the predictions columns from each ML model to msfe_df
msfe_df$en_pred <- en_pred
msfe_df$l_pred <- l_pred
msfe_df$r_pred <- r_pred

# Uses the new complete panel to calculated MSFE for VAR model
en_msfe <-
  sum((as.numeric(msfe_df$en_pred) - msfe_df$GDP_QNA_RG)^2) / nrow(msfe_df)
r_msfe <-
  sum((as.numeric(msfe_df$r_pred) - msfe_df$GDP_QNA_RG)^2) / nrow(msfe_df)
l_msfe <-
  sum((as.numeric(msfe_df$l_pred) - msfe_df$GDP_QNA_RG)^2) / nrow(msfe_df)

#### Plot predictions and observations ####

# Initiate an array of monthly dates from 2011 to 2018
dates_for_plot <-
  seq(as.Date("2006-01-01"), as.Date("2010-12-01"), by = "month")

# Color selection
colors <- c(
  "Predictions" = "steelblue",
  "Observations" = "grey"
)

# Set graphs legend to the top
theme_set(theme_bw() +
  theme(legend.position = "top"))

### Elastic net graph ###
# Put predictions and an array of dates into a dataframe
elastic_net_predictions_df <-
  data.frame(en_pred, dates_for_plot)

# Plot
elastic_net_plot <- ggplot() +
  # Draw predictions line
  geom_line(
    data = elastic_net_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(en_pred),
      color = "Predictions"
    ),
    size = 1
  ) +

  # Draw observations line
  geom_line(
    data = elastic_net_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(test_set$GDP_QNA_RG),
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
    x = as.Date("2006-12-01"),
    y = -0.2,
    label = paste0("MSFE: ", round(en_msfe, digits = 5))
  )
### Ridge graph ###
ridge_predictions_df <-
  data.frame(r_pred, dates_for_plot)

# Plot
ridge_plot <- ggplot() +

  # Draw predictions line
  geom_line(
    data = ridge_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(r_pred),
      color = "Predictions"
    ),
    size = 1
  ) +
  # Draw observations line
  geom_line(
    data = ridge_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(test_set$GDP_QNA_RG),
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
    x = as.Date("2006-12-01"),
    y = -0.2,
    label = paste0("MSFE: ", round(r_msfe, digits = 5))
  )

### Lasso graph ###
lasso_predictions_df <-
  data.frame(l_pred, dates_for_plot)

# Plot
lasso_plot <- ggplot() +

  # Draw predictions line
  geom_line(
    data = lasso_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(l_pred),
      color = "Predictions"
    ),
    size = 1
  ) +

  # Draw observations line
  geom_line(
    data = lasso_predictions_df,
    aes(
      x = as.Date(dates_for_plot),
      y = as.numeric(test_set$GDP_QNA_RG),
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
    x = as.Date("2006-12-01"),
    y = -0.2,
    label = paste0("MSFE: ", round(l_msfe, digits = 5))
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
