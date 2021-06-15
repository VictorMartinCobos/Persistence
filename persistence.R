# PERSISTENCE MODELS -------------------------------------------------------
library(tidyverse)
library(lubridate)

#Persistence model 1
persistence_model_1 <- function(hour, data, day) {
  asked_hour <- dmy_h(str_c(day, hour, "H", sep = " "))
  min_ts <- data %>% 
    pull(ts) %>% 
    min()
  
  repeat {
    c <- data %>% 
      filter(ts == asked_hour) %>% 
      pull(consumption)
    if(length(c) != 0 & is.numeric(c)) break
    asked_hour <- asked_hour - weeks(1)
    if(asked_hour < min_ts) {
      return(
        tibble(ts = dmy_h(str_c(day, hour, "H", sep = " ")), consumption = NA))
    }
  }
  data %>%
    filter(ts == asked_hour) %>%
    transmute(ts = dmy_h(str_c(day, hour, "H", sep = " ")), consumption)
}

persistence_predict_1 <- function(data, day) {
  map_df(0:23, persistence_model_1, data, day)
}

#Persistence model 2
persistence_model_2 <- function(data, dt) {
  data_filtered <- data %>% 
    filter(
      ts <= dt &
      wday(ts) == wday(dt) &
      hour(ts) == hour(dt) &
      is.numeric(consumption)
    )
  
  if(nrow(data_filtered) != 0) {
    data_filtered %>%  
      slice_max(order_by = ts, n = 1) %>% 
      transmute(ts = dt, consumption)
  } else {
    tibble(ts = dt, consumption = NA)
  }
}

persistence_predict_2 <- function(data, day) {
  dt <- seq(from = as_datetime(day), to = day + hours(23), by = "hours")
  map_df(dt, ~persistence_model_2(data, .x))
}

#Persistence Model 3 ARREGLAR
persistence_model_3 <- function(data, dt) {
  data %>% 
    filter(ts <= dt, hour(ts) == hour(dt), wday(ts) == wday(dt)) %>% 
    union(tibble(ts = dt, consumption = NA)) %>%
    arrange(ts) %>% 
    fill(consumption) %>% 
    filter(ts == dt)
}

persistence_predict_3 <- function(data, day) {
  dt <- seq(from = as_datetime(day), to = day + hours(23), by = "hours")
  map_df(dt, ~persistence_model_3(data, .x))
}

#Persistence Model 3.b
persistence_model_3_b <- function(data, dt) {
  data %>% 
    filter(ts <= dt, hour(ts) == hour(dt), wday(ts) == wday(dt)) %>% 
    complete(ts = dt) %>%
    arrange(ts) %>% 
    fill(consumption) %>% 
    filter(ts == dt)
}

persistence_predict_3_b <- function(data, day) {
  dt <- seq(from = as_datetime(day), to = day + hours(23), by = "hours")
  map_df(dt, persistence_model_3_b, data = data)
}

#Persistence Model 4
persistence_predict_4 <- function(data, dt) {
  data %>% 
    arrange(ts) %>% 
    complete(ts = seq(from = dt, to = dt + hours(23), by = "hours")) %>% 
    group_by(hour(ts), wday(ts)) %>% 
    fill(consumption) %>% 
    ungroup() %>% 
    filter(date(ts) == date(dt)) %>% 
    transmute(ts, consumption)
}

#Persistence Model 5. Applied to all the dataset
persistence_data <- function(data) {
  data %>% 
    complete(ts = seq(from = min(ts), to = max(ts), by = "hours")) %>% 
    arrange(ts) %>% 
    mutate(consumption_pred = lag(consumption, 24*7)) %>% 
    group_by(hour(ts), wday(ts)) %>% 
    fill(consumption_pred) %>% 
    ungroup() %>% 
    select(ts, consumption, consumption_pred)
}

#Persistence Model 6. New variable: original date from prediction
##Ifelse version
persistence_data_date_ifelse <- function(data) {
  data %>% 
    complete(ts = seq(from = min(ts), to = max(ts), by = "hours")) %>% 
    arrange(ts) %>% 
    mutate(consumption_pred = lag(consumption, 24*7),
           ts_pred = lag(ts, 24*7)) %>% 
    mutate(ts_pred = ifelse(is.na(consumption_pred), NA, ts_pred)) %>% 
    group_by(hour(ts), wday(ts)) %>% 
    fill(consumption_pred, ts_pred) %>% 
    ungroup() %>% 
    transmute(ts, consumption, consumption_pred, ts_pred = as_datetime(ts_pred))
}

##If_else version
persistence_data_date_if_else <- function(data) {
  data %>% 
    complete(ts = seq(from = min(ts), to = max(ts), by = "hours")) %>% 
    arrange(ts) %>% 
    mutate(consumption_pred = lag(consumption, 24*7),
           ts_pred = lag(ts, 24*7)) %>% 
    mutate(ts_pred = if_else(is.na(consumption_pred), NA_POSIXct_, ts_pred)) %>% 
    group_by(hour(ts), wday(ts)) %>% 
    fill(consumption_pred, ts_pred) %>% 
    ungroup() %>% 
    transmute(ts, consumption, consumption_pred, ts_pred = as_datetime(ts_pred))
}

##Case_when version
persistence_data_date_case_when <- function(data) {
  data %>% 
    complete(ts = seq(from = min(ts), to = max(ts), by = "hours")) %>% 
    arrange(ts) %>% 
    mutate(consumption_pred = lag(consumption, 24*7),
           ts_pred = lag(ts, 24*7)) %>% 
    mutate(ts_pred = case_when(
      is.na(consumption_pred) ~ NA_POSIXct_,
      TRUE ~ ts_pred
      )
    ) %>% 
    group_by(hour(ts), wday(ts)) %>% 
    fill(consumption_pred, ts_pred) %>% 
    ungroup() %>% 
    transmute(ts, consumption, consumption_pred, ts_pred = as_datetime(ts_pred))
}

##Mutate at start version
persistence_data_date_faster <- function(data) {
  data %>% 
    arrange(ts) %>% 
    mutate(ts_pred = ts) %>% 
    complete(ts = seq(from = min(ts), to = max(ts), by = "hours")) %>% 
    mutate(consumption_pred = lag(consumption, 24*7),
           ts_pred = lag(ts_pred, 24*7)) %>% 
    group_by(hour(ts), wday(ts)) %>% 
    fill(consumption_pred, ts_pred) %>% 
    ungroup() %>% 
    select(ts, consumption, consumption_pred, ts_pred)
}

test <- data_tidy[-338, ] #24/01/2017 00H