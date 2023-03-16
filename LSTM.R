###### Packages #######

library(reticulate)
library(ggplot2)
library(nowcastLSTM) 
library(tidyverse)
library(dplyr)

###### Set-Up #######

Sys.setenv(RETICULATE_PYTHON = "/Library/Frameworks/Python.framework/Versions/3.11/bin/python3")
initialize_session(python_path = "/Library/Frameworks/Python.framework/Versions/3.11/bin/python3")

