# Create dummy datasets for 2023 CARS (wave 5) -----------------

library(dplyr)
library(purrr)
library(usethis)

source("dummy_cars_options.R")
source("create_dummy_data.R")

cars_dummy_data <- create_dummy_data(type = "test")
write.csv(cars_dummy_data, "cars_dummy_data.csv")


cars_dummy_data_clean <- create_dummy_data(type = "clean")
write.csv(cars_dummy_data_clean, "cars_dummy_data_clean.csv")

