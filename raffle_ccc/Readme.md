This application allows you to upload a CSV file that contains structured data from which the user wants to select one piece randomly, e.g., pick a raffle winner from the list of people who purchased tickets. Once the file is uploaded, the code in `global.R` processes it in the following way:
  
  1. Read the file and "flatten" the data to create a table containing one row per raffle ticket.
  2. Based on the length (number of rows) of the table, pick a random row number.
  3. Get the first name and last name of the person in that row.

The app then displays the winner's name next to the upload widget.

The full code is displayed here for reference and transparency: `app.R` contains the code for the user interface, while `global.R` contains the logic. This application could easily be adapted for other data formats or different random-pick needs.

Author: Amanda Gadrow
