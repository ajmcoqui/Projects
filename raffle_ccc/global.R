library(readr)
library(dplyr)
library(tidyr)

# returns dataframe with firstname/lastname columns, one row per ticket purchased
normalize_data <- function(csv_filepath) {
  raw_data <- read_csv(csv_filepath)
  expanded_data <- uncount(raw_data, num_tickets)
  expanded_data
}

# returns a random row from the dataframe using `sample()`
pick_random_row_number <- function(dataframe) {
  num_rows <- nrow(dataframe)
  random_row_number <- sample(1:num_rows, 1)
  random_row_number
}

# returns a firstname/lastname combination given a list in csv form
pick_random_name <- function(csv_filename) {
  names <- normalize_data(csv_filename)
  row <- pick_random_row_number(names)
  random_name <- names[row,]
  random_name
}
