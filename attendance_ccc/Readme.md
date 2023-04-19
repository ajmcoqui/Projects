# CCC Attendance app
Author: Amanda Gadrow
Date: April, 2023

This application was created to allow members of the Columbus Curling Club to check in when they arrive at the Annual Meeting.

### Required inputs
This app assumes the presence of two files in the data folder:
1. A list of current members
2. A list of proxy-member pairs

It uses these files to generate a drop-down list of members, list any proxies they might have, and allow them to submit their attendance.

### Outputs
Attendance data are written out to a Google sheet in Amanda's account. Once the board has access to its own Google Workspace, the app can be updated to write to a shared location, instead.

This app is paired with another that reads from the Google sheet and reports attendance results.
