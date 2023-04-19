library(shiny)
library(bslib)
library(DT)
library(data.table)
library(purrr)
source("global.R")

ui <- fluidPage(
    # This JS file restricts the number of Board members that can be selected from the list to three.
    includeScript(path = "js4checkbox.js"),
    theme = bs_theme(version = 5, bootswatch = "cerulean"),
    img(src='ccc_logo.png', align = "top"),
    titlePanel("CCC Annual Meeting Ballot"),
    h4("Select your name, then cast your vote."),

    mainPanel(
        selectInput("voter",
                    "Your name:",
                    choices = member_list$full_name),
        h6("Do you agree that we should shift the member calendar to match the fiscal calendar, to simplify operations?"),
        radioButtons("calendar",
                     "",
                     choices = c("yes", "no"),
                     selected = "yes",
                     inline = TRUE),
        h6("Please pick up to three candidates for the CCC Board."),
        checkboxGroupInput(
            "candidates",
            "",
            choices = c(candidate_list$full_name, "Write-in")
        ),
        textInput("writein", "Write-in candidate"),
        hr(),
        textOutput("proxy_message"),
        br(),
        dataTableOutput("proxy_names"),
        h5("Verify your name again, please:"),
        selectInput("voter_verify",
                    "Your name:",
                    choices = member_list$full_name),
        actionButton("submit_button",
                     "Submit vote",
                     icon("broom"),
                     style="color: #2596be; background-color: #2596be; border-color: #2e6da4"),
        textOutput("submit_vote"),
        h6("If you need to change your vote after you submit, please reload this page and revote - only your latest vote will be counted.")
    )
)

server <- function(input, output) {
    
    proxies <- reactive(
        {
            proxy_list |> 
                filter(proxy == input$voter) |> 
                select(member = voter)
        }
    )
    
    # Empty data frame to hold proxy vote data
    proxy_input_map <- reactiveValues()
    
    output$proxy_message <- renderText(
        {
            if (count(proxies()) > 0) {
                return("It looks like you are designated as a proxy for at least one other CCC member. Please vote on their behalf below. \n")
            }
        }
    )
    
    output$proxy_names <- renderDataTable(
        {
            req(input$voter)
            if (count(proxies()) > 0) {
                proxy_tbl <- data.table(proxies(), vote = NA)
                proxy_tbl[, row_select_id := paste0("row_select_", .I)][, calendar_vote := as.character(radioButtons(inputId=row_select_id, label=NULL, choices=c("yes", "no"), inline = TRUE, selected = "yes")), by = row_select_id]
                proxy_tbl[, row_check_id := paste0("row_checks_", .I)][, board_vote := as.character(checkboxGroupInput(inputId=row_check_id, label=NULL, choices=c(candidate_list$full_name, "Write-in"))), by = row_check_id]
                proxy_tbl[, row_writein_id := paste0("row_writein_", .I)][, writein_vote := as.character(textInput(inputId=row_writein_id, label=NULL)), by = row_writein_id]
                # Grab the input IDs associated with each member so we can get the values later
                pim <- proxy_tbl |> select(member, row_select_id, row_check_id, row_writein_id) |> data.frame()
                proxy_input_map$pim <- pim
                # Remove unneeded columns and convert to a datatable for rendering
                proxy_tbl <- select(proxy_tbl, -vote, -row_select_id, -row_check_id, -row_writein_id)
                proxy_dt <- datatable(proxy_tbl, rownames = FALSE, escape = FALSE, selection = "none",
                                      options = list(dom = 't',
                                                     scrollX = TRUE,
                                                     order = list(list(1, 'asc')),
                                                     preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                     drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }')
                                                     )
                                      )
                return(proxy_dt)
            }
        }
    )

    output$submit_vote <- renderText(
        {
            req(input$submit_button)
            # Capture the vote of the present member
            member <- input$voter
            calendar_vote <- input$calendar
            board_vote <- input$candidates
            writein_vote <- input$writein
            member_verify <- input$voter_verify
            timestamp <- Sys.time()
            votes <- data.frame(member, calendar_vote, board_vote, writein_vote, member_verify, timestamp, proxy = NA)
            
            # If there are proxies, capture those votes, too
            if(count(proxies()) > 0) {
              # Helper functions to pull the data out of the inputs
              get_proxy_vote_by_id <- function(input_id){
                input[[input_id]]
              }
              
              create_voter_record <- function(proxy_row){
                member <- proxy_row$member
                calendar_vote <- proxy_row$row_select_id |>
                    get_proxy_vote_by_id()
                board_vote <- proxy_row$row_check_id |>
                    get_proxy_vote_by_id()
                writein_vote <- proxy_row$row_writein_id |> 
                    get_proxy_vote_by_id()
                member_verify <- "proxy"
                voter_record <- data.frame(member, calendar_vote, board_vote, writein_vote, member_verify, timestamp, proxy = input$voter)
                return(voter_record)
              }
              
              # Iterate through proxies, get votes, and add to the data.frame
              for (row in 1:nrow(proxy_input_map$pim)) {
                  proxy_vote <- create_voter_record(proxy_input_map$pim[row,])
                  votes <- rbind(votes, proxy_vote)
              }
            }
            # For local testing
            # write_csv(votes, "2023AnnualMeetingVotes.csv", append = TRUE)
            # Write results to remote spreadsheet
            withProgress(message = "Logging your vote...", {
                wb <- drive_get("ccc_votes_2023")
                # dt <- read_sheet(wb)
                new_entry <- votes
                sheet_append(wb$id, new_entry)
            })
            return("Your vote has been submitted. Thanks!")
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
