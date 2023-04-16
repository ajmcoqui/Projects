# CCC Voting app
Author: Amanda Gadrow
Date: April, 2023

This application was created to allow members of the Columbus Curling Club to vote for new board members in the Annual Meeting.

### Required inputs
This app assumes the presence of three files in the data folder:
1. A list of current members
2. A list of proxy-member pairs
3. A list of board candidates

It is currently hard-coded to ask a particular ballot question about aligning the member and fiscal calendars. This bit could be changed easily as other ballot questions come up.

### Outputs
Votes are written out to a Google sheet in Amanda's account. Once the board has access to its own Google Workspace, the app can be updated to write to a shared location, instead.

This app is paired with another that reads from the Google sheet and reports voting results.
