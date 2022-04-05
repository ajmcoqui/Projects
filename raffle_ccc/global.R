library(readr)
library(dplyr)
library(tidyr)

# returns dataframe with firstname/lastname columns, one row per ticket purchased
normalize_data <- function(csv_filepath) {
  raw_data <- read_csv(csv_filepath) %>% 
    select(firstname = `First Name`, lastname = `Last Name`, email = Email, num_tickets = Quantity) %>%
    filter(!is.na(num_tickets))
  expanded_data <- uncount(raw_data, num_tickets)
  expanded_data
}

# returns a random row from the dataframe using `sample()`
pick_random_row_number <- function(dataframe, num_winners) {
  num_rows <- nrow(dataframe)
  if(num_winners <= num_rows) {
    random_row_number <- sample(1:num_rows, num_winners)
  } else {
    random_row_number <- sample(1:num_rows, num_rows)
  }
  random_row_number
}

# returns a firstname/lastname combination given a list in csv form
pick_random_name <- function(csv_filename, num_winners) {
  names <- normalize_data(csv_filename)
  row <- pick_random_row_number(names, num_winners)
  random_name <- names[row,]
  random_name
}
