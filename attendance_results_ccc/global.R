library(googledrive)
library(googlesheets4)

# Establish  authorization to Google Drive and Google Sheets to store the voting data
drive_auth(cache = ".secrets", email = Sys.getenv("GOOGLE_EMAIL"))
gs4_auth(token = drive_token())

# Google Drive and Google Sheets handling based on the guidance in this post:
# https://stackoverflow.com/questions/44980757/remote-server-authentication-to-read-googlesheets-from-r-script-not-using-servic