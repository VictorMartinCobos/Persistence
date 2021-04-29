# PERSISTENCE MODEL MAIN ----------------------------------------------
library(tidyverse)
library(lubridate)
source("utils.R")
source("persistence.R")

#Select a Directory
directory_path <- rstudioapi::selectDirectory(
  caption = "Select Directory",
  label = "Select",
  path = "~/Documents/"
)

#Obtain a list of all csv contained in directory chosen
list_csv_names <- list.files(
  path = directory_path,
  pattern = "*.csv",
  all.files = TRUE,
  full.names = TRUE
)

#Import each csv
csv_list <- map(list_csv_names, import_csv)

#Join csv in an unique tibble
data <- reduce(csv_list, bind_rows)

#Tidy csv
data_tidy <-tidy_csv(data)