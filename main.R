# PERSISTENCE MODEL MAIN ----------------------------------------------
library(tidyverse)
library(lubridate)
source("utils.R")

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

#Persistence model
persistence_model <- function(hour, data, day) {
  asked_hour <- dmy_h(str_c(day, hour, "H", sep = " "))
  repeat {
    length <- data %>% 
      filter(ts == asked_hour) %>% 
      pull(ts) %>% 
      length()
    if (length == 0) asked_hour <- asked_hour - weeks()
    else {
      return(
        data %>%
          filter(ts == asked_hour) %>%
          transmute(ts = dmy_h(str_c(day, hour, "H", sep = " ")), consumption)
      )
    }
  }
}
  
persistence_predict <- function(data, day) {
  map_df(0:23, persistence_model, data, day)
}
