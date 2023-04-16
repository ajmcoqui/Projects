library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(purrr)
source("global.R")

ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "cerulean"),
    img(src='ccc_logo.png', align = "top"),
    titlePanel("CCC Voting Results"),

    mainPanel(
        actionButton("refresh", "Refresh data", style="color: #2596be; background-color: #2596be; border-color: #2e6da4"),
        h5("Number of members who have voted (including proxies):"),
        textOutput("vote_counts"),
        h5("Calendar votes:"),
        tableOutput("calendar_votes"),
        h5("Board candidate votes:"),
        tableOutput("board_votes")
    )
)

server <- function(input, output, session) {

    # Get the vote data when the Refresh button is clicked    
    raw_vote_data <- reactive({
        req(input$refresh)
        vote_sheet <- drive_get("ccc_votes_2023")
        member_votes <- read_sheet(vote_sheet)
        member_votes
    })

    # These outputs are sadly duplicative, doing the same calculations for each; will need to refactor at some point.

    # When the Refresh button is clicked, calculate the number of unique voters in the data set.
    output$vote_counts <- renderText({
        req(input$refresh)
        unique_votes <- raw_vote_data() |> unique()
        unique_members <- unique_votes |> select(member) |> unique()
        num_voting_members <- unique_members$member |> unique() |> length()
        print(num_voting_members)
        num_voting_members
    })
    
    # When the Refresh button is clicked, get the calendar votes. 
    output$calendar_votes <- renderTable({
        req(input$refresh)
        unique_votes <- raw_vote_data() |> unique()
        unique_members <- unique_votes |> select(member) |> unique()
        # Get the most recent set of votes, in case of multiple submissions
        final_votes <- data.frame(member = NA, calendar_vote = NA, board_vote = NA)
        for (m in unique_members$member) {
            member_votes <- unique_votes |> filter(member == m)
            latest_timestamp <- max(member_votes$timestamp, rm.na = TRUE)
            latest_votes <- member_votes |> 
                filter(timestamp == latest_timestamp) |> 
                select(member, calendar_vote, board_vote)
            final_votes <- rbind(final_votes, latest_votes)
        }
        print(str(final_votes))
        
        # Tally the votes for calendar ballot measure
        calendar_votes <- final_votes |> 
            select(member, calendar_vote) |> 
            unique() |> 
            na.omit(calendar_vote) |> 
            count(calendar_vote) |> 
            arrange(desc(n))
        print(calendar_votes)
        calendar_votes
    })
    
    # When the Refresh button is clicked, get the board candidate votes.
    output$board_votes <- renderTable({
        req(input$refresh)
        unique_votes <- raw_vote_data() |> unique()
        unique_members <- unique_votes |> select(member) |> unique()
        # Get the most recent set of votes, in case of multiple submissions
        final_votes <- data.frame(member = NA, calendar_vote = NA, board_vote = NA)
        for (m in unique_members$member) {
            member_votes <- unique_votes |> filter(member == m)
            latest_timestamp <- max(member_votes$timestamp, rm.na = TRUE)
            latest_votes <- member_votes |> 
                filter(timestamp == latest_timestamp) |> 
                select(member, calendar_vote, board_vote)
            final_votes <- rbind(final_votes, latest_votes)
        }
        print(str(final_votes))
        
        # Tally the votes for board candidates
        board_votes <- final_votes |> 
            select(board_vote) |> 
            na.omit(board_vote) |> 
            count(board_vote) |> 
            arrange(desc(n))
        print(board_votes)
        board_votes
    })
}

shinyApp(ui = ui, server = server)


# vote_sheet <- drive_get("ccc_votes_2023")
# Poll every 5 seconds for modified data, and update the app output
# check_last_modified <- function() {
#     vote_sheet_last_modified <- googledrive::drive_find("ccc_votes_2023", n_max = 1) |>
#         mutate(modified = map_chr(drive_resource, "modifiedTime")) |> select(modified)
#     return(vote_sheet_last_modified$modified)
# }
# member_votes <- reactivePoll(5000, session, 
#                              checkFunc = function() {
#                                  vote_sheet_last_modified <- googledrive::drive_find("ccc_votes_2023", n_max = 1) |>
#                                      mutate(modified = map_chr(drive_resource, "modifiedTime")) |> select(modified)}, 
#                              valueFunc = function() {
#                                  read_sheet(vote_sheet)}
#                              )

