# PERSISTENCE MODEL UTILS---------------------------------------------
library(tidyverse)
library(lubridate)

#Import each csv
import_csv <- function(file) {
  read_csv(file,
           col_types = cols(
             Fecha = col_character(),
             Hora = col_character(),
             `Consumo (Wh)` = col_double(),
             `Precio (€/kWh)` = col_skip(), 
             `Coste por hora (€)` = col_skip() 
           ),
           skip = 6)
}

#Tidy each csv
tidy_csv <- function(file) {
  file %>% 
    drop_na() %>%
    transmute(
      date = Fecha,
      hour = hm(str_sub(Hora, 1, 5)),
      consumption = `Consumo (Wh)`
    ) %>%
    add_count(date) %>% 
    transmute(
      ts = with_tz(parse(n, hour, date), tz = "UTC"),
      consumption
    ) %>% 
    arrange(ts)
}

#Parse function
parse <- function(n, hour, date) {
  case_when(
    n == 25 & hour == "2H 0M" ~ as_datetime(
      str_c(date, hour - hm("1H 0M"), sep = " "),
      tz = "Europe/Madrid",
      format = "%Y-%m-%d %HH %MM %SS")
    + dhours(),
    n == 25 & hour > "2H 0M" ~ as_datetime(
      str_c(date, hour - hm("1H 0M"), sep = " "),
      tz = "Europe/Madrid",
      format = "%Y-%m-%d %HH %MM %SS"),
    n == 23 & hour >= "2H 0M" ~ as_datetime(
      str_c(date, hour + hm("1H 0M"), sep = " "),
      tz = "Europe/Madrid",
      format = "%Y-%m-%d %HH %MM %SS"),
    hour == "0s" ~ as_datetime(
      str_c(date, "0H", sep = " "),
      tz = "Europe/Madrid",
      format = "%Y-%m-%d %HH"),
    TRUE ~ as_datetime(
      str_c(date, hour, sep = " "),
      tz = "Europe/Madrid",
      format = "%Y-%m-%d %HH %MM %SS")
  )
}


