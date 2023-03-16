###### Packages #######

library(reticulate)
library(ggplot2)
library(nowcastLSTM) 
library(tidyverse)
library(dplyr)
library(readxl)

###### RUN THIS FIRST - Loads Python Env and Packages #######

initialize_session(python_path = "./PythonEnvironment/bin/python3")
Sys.setenv(RETICULATE_PYTHON = "./PythonEnvironment/bin/python3")
Sys.setenv(LD_LIBRARY_PATH = "./PythonEnvironment/lib/python3.11/site-packages/")
install_miniconda(path = miniconda_path(), update = TRUE, force = FALSE)
py_install(conda=miniconda_path(), c("dill", "numpy", "pandas", "pmdarima", "torch", "nowcast-lstm"), pip=TRUE)
py_config()
import("numpy")
np<-import("numpy")
###### Data import ########

nowcasting_dataset <- read_excel(
  "230315 Nowcasting Dataset.xlsx", sheet = "Nowcasting Dataset",
  col_types = c(
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
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)

nowcasting_dataset <- nowcasting_dataset[, -c(2,3,5)] #remove surplus GDP cols

###### LSTM Training ########

# C-19 Training Subset 

training <- subset(nowcasting_dataset
                   , subset = nowcasting_dataset$Date <= '2015-12-01')

# instantiating a model with 10 networks based on the previous 12 months with 50 train episodes.
# name of R object and python model name must match 

model_C19 <- LSTM(
  data=training, 
  target_variable="GDP_QNA_RG", 
  n_timesteps=12, 
  n_models=10, 
  train_episodes=50, 
  python_model_name="model_C19"
)

train_preds <- predict(model_C19, training, only_actuals_obs=TRUE)

train_preds %>% 
  gather(label, value, -date) %>% 
  ggplot() + 
  aes(x=date, y=value, color=label) %>% 
  geom_line()

