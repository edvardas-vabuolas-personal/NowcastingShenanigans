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
library(doParallel)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

#### Load data ####
nowcasting_dataset <- read_excel("230312 Nowcasting Dataset.xls", 
                                 col_types = c("numeric", "date", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric"))
rownames(nowcasting_dataset) <- nowcasting_dataset$Date
nowcasting_dataset$UK_GDP_4 <- na.approx(nowcasting_dataset$UK_GDP_4)

train_set <- subset(nowcasting_dataset[, -c(1, 2)], subset = nowcasting_dataset$Date <= '2010-12-01')
test_set <- subset(nowcasting_dataset[, -c(1, 2)], subset = nowcasting_dataset$Date >= '2011-01-01')


#### creating sampling seeds ####
set.seed(123)
seeds <- vector(mode = "list", length = 88)
for(i in 1:527) seeds[[i]] <- sample.int(1000, 15)

registerDoParallel(cores=3)

# Find optimal lambda and alpha
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 250, # 
                              horizon = 1,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              savePredictions = 'final',
                              verboseIter = TRUE,
                              seeds = seeds)

tuneLength.num <- 15

elastic_net <- train(UK_GDP_4 ~ .,
                     data = nowcasting_dataset[, -c(1, 2)],
                     method = "glmnet",
                     family = "gaussian",
                     trControl = myTimeControl,
                     tuneLength = tuneLength.num,
                     metric='RMSE')
elastic_net
# https://daviddalpiaz.github.io/r4sl/elastic-net.html
ridge <- train(UK_GDP_4 ~ .,
               data = nowcasting_dataset[, -c(1, 2)],
               method = "glmnet",
               family = "gaussian",
               trControl = myTimeControl,
               tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 1, 0.005)),
               metric='RMSE')
ridge

lasso <- train(UK_GDP_4 ~ .,
               data = nowcasting_dataset[, -c(1, 2)],
               method = "glmnet",
               family = "gaussian",
               trControl = myTimeControl,
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, 0.005)),
               metric='RMSE')
lasso

elastic_net_list_of_predictions <- elastic_net$pred$pred
ridge_list_of_predictions <- ridge$pred$pred
lasso_list_of_predictions <- lasso$pred$pred

elastic_net_msfe <- sum((as.numeric(elastic_net_list_of_predictions) - test_set$UK_GDP_4)^2) / nrow(test_set)
ridge_msfe <- sum((as.numeric(ridge_list_of_predictions) - test_set$UK_GDP_4)^2) / nrow(test_set)
lasso_msfe <- sum((as.numeric(lasso_list_of_predictions) - test_set$UK_GDP_4)^2) / nrow(test_set)

### Plot predictions and observations ###
dates_for_plot <- seq(as.Date("2011-01-01"), as.Date("2018-03-01"), by="month")
colors <- c("Predictions" = "steelblue", "Observations" = "grey")


elastic_net_predictions_df <- data.frame(elastic_net_list_of_predictions, dates_for_plot)
elastic_net_plot <- ggplot() +
  # Draw line
  geom_line(data = elastic_net_predictions_df, 
            aes(
              x = as.Date(dates_for_plot),
              y = as.numeric(elastic_net_list_of_predictions), 
              color = "Predictions"),
            size = 1) +
  
  geom_line(data = test_set[,1], aes(x = as.Date(dates_for_plot),
                                     y = UK_GDP_4, color = "Observations"), 
            size = 1) +
  # Change x axis title
  labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  scale_color_manual(values = colors) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  annotate(geom="text", x=as.Date("2016-12-01"), y=-0.2, label=paste0("MSFE: ", round(elastic_net_msfe, digits=5)))

ridge_predictions_df <- data.frame(ridge_list_of_predictions, dates_for_plot)
ridge_plot <- ggplot() +
  # Draw line
  geom_line(data = ridge_predictions_df, 
            aes(
              x = as.Date(dates_for_plot),
              y = as.numeric(ridge_list_of_predictions), 
              color = "Predictions"),
            size = 1) +
  
  geom_line(data = test_set[,1], aes(x = as.Date(dates_for_plot),
                                     y = UK_GDP_4, color = "Observations"), 
            size = 1) +
  # Change x axis title
  labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  scale_color_manual(values = colors) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  annotate(geom="text", x=as.Date("2016-12-01"), y=-0.2, label=paste0("MSFE: ", round(ridge_msfe, digits=5)))

lasso_predictions_df <- data.frame(lasso_list_of_predictions, dates_for_plot)
lasso_plot <- ggplot() +
  # Draw line
  geom_line(data = lasso_predictions_df, 
            aes(
              x = as.Date(dates_for_plot),
              y = as.numeric(lasso_list_of_predictions), 
              color = "Predictions"),
            size = 1) +
  
  geom_line(data = test_set[,1], aes(x = as.Date(dates_for_plot),
                                     y = UK_GDP_4, color = "Observations"), 
            size = 1) +
  # Change x axis title
  labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  scale_color_manual(values = colors) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  annotate(geom="text", x=as.Date("2016-12-01"), y=-0.2, label=paste0("MSFE: ", round(lasso_msfe, digits=5)))

figure <- ggarrange(elastic_net_plot, ridge_plot, lasso_plot,
                    labels = c("Elastic Net", "Ridge", "Lasso"),
                    ncol = 2, nrow = 2)
figure

coef(lasso$finalModel, lasso$bestTune$lambda)
coef(ridge$finalModel, ridge$bestTune$lambda)

var_importance <- varImp(ridge)
plot(var_importance)

var_importance <- varImp(lasso)
plot(var_importance)

var_importance <- varImp(elastic_net)
plot(var_importance)

elastic_net$bestTune
lasso$bestTune
ridge$bestTune





