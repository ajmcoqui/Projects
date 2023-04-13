library(readr)
library(dplyr)
library(tidyr)
library(stringr)

member_csv_filepath <- "data/Current_Members_2023-03-29.csv"
proxy_csv_filepath <- "data/CCC 2022 Proxy for Board Member Vote on April 30.csv"
candidate_csv_filepath <- "data/board_candidate_names_2023.csv"

# Read the member list and dedupe
member_list <- read_delim(member_csv_filepath, 
           delim = ";", escape_double = FALSE, trim_ws = TRUE) |> 
  select(first_name = `First Name`, last_name = `Last Name`) |> 
  unique()
# Add a column for for the full name, to be used in the UI dropdown
full_name <- member_list |> unite(col = "full_name", c(first_name, last_name), sep = " ")
member_list <- cbind(member_list, full_name)

# Read the proxy list, split out the names, and dedupe
proxy_list <- read_csv(proxy_csv_filepath) |> 
  select(voter = `Your name`, proxy = `Name of member who will act as proxy for you:`) |> 
  mutate(voter_first_name = str_split_i(voter, " ", 1)) |> 
  mutate(voter_last_name = str_split_i(voter, " ", 2)) |> 
  mutate(proxy_first_name = str_split_i(proxy, " ", 1)) |> 
  mutate(proxy_last_name = str_split_i(proxy, " ", 2)) |> 
  unique()

# Given a name, determine whether they are a valid active member
# In the comparison, guard against mismatched cases
# Returns a boolean
is_active_member <- function(member_first_name, member_last_name) {
  member <- member_list |> 
    filter(tolower(first_name) == tolower(member_first_name) & tolower(last_name) == tolower(member_last_name))
  if(count(member) == 1) return (TRUE) else return (FALSE)
}

# Given a name, return any members for whom they are acting as proxy
# In the comparison, guard against mismatched cases
# Returns a tibble
look_up_proxies <- function(member_first_name, member_last_name) {
  proxies <- proxy_list |> 
    filter(tolower(proxy_first_name) == tolower(member_first_name) & tolower(proxy_last_name) == tolower(member_last_name)) |> 
    select(voter_first_name, voter_last_name)
  proxies
}

candidate_list <- read_csv(candidate_csv_filepath)
# Add a column for for the full name, to be used in the UI
candidate_full_name <- candidate_list |> unite(col = "full_name", c(first_name, last_name), sep = " ")
candidate_list <- cbind(candidate_list, candidate_full_name)

