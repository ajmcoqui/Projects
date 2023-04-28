library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(googledrive)
library(googlesheets4)

member_csv_filepath <- "data/Current_Members_2023-03-29.csv"
proxy_csv_filepath <- "data/CCC 2023 Proxy for Board Member Vote on April 29.csv"

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
  select(member = `Your Name`, proxy = `Name of person who will act as your proxy.`) |> 
  mutate(member_first_name = str_split_i(member, "\t", 1)) |> 
  mutate(member_last_name = str_split_i(member, "\t", 2)) |> 
  mutate(proxy_first_name = str_split_i(proxy, "\t", 1)) |> 
  mutate(proxy_last_name = str_split_i(proxy, "\t", 2)) |> 
  unique()
# Replace tabs in names
proxy_list$member <- gsub("\t", " ", proxy_list$member)
proxy_list$proxy <- gsub("\t", " ", proxy_list$proxy)

# Establish  authorization to Google Drive and Google Sheets to store the voting data
drive_auth(cache = ".secrets", email = Sys.getenv("GOOGLE_EMAIL"))
gs4_auth(token = drive_token())

