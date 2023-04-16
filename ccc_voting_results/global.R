library(googledrive)
library(googlesheets4)

# Establish  authorization to Google Drive and Google Sheets to store the voting data
drive_auth(cache = ".secrets", email = Sys.getenv("GOOGLE_EMAIL"))
gs4_auth(token = drive_token())
vote_sheet <- drive_get("ccc_votes_2023")
member_votes <- read_sheet(vote_sheet)
