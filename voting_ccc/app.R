library(shiny)
library(bslib)
library(DT)
library(data.table)
source("global.R")

# TODO optimize for mobile devices

ui <- fluidPage(
    # This JS file restricts the number of Board members that can be selected from the list to three.
    includeScript(path = "js4checkbox.js"),
    theme = bs_theme(version = 4, bootswatch = "cerulean"),
    img(src='ccc_logo.png', align = "right"),
    titlePanel("CCC Annual Meeting"),
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
            choices = candidate_list$full_name
        ),
        hr(),
        textOutput("proxy_message"),
        br(),
        dataTableOutput("proxy_names"),
        actionButton("submit_button",
                     "Submit vote",
                     icon("broom"),
                     style="color: #2596be; background-color: #2596be; border-color: #2e6da4"),
        textOutput("submit_vote")
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
                proxy_tbl[, row_select_id := paste0("row_select_", .I)][, calendar_vote := as.character(radioButtons(inputId=row_select_id, label=NULL, choices=c("yes", "no"), selected = "yes")), by = row_select_id]
                proxy_tbl[, row_check_id := paste0("row_checks_", .I)][, board_vote := as.character(checkboxGroupInput(inputId=row_check_id, label=NULL, choices=candidate_list$full_name)), by = row_check_id]
                # Grab the input IDs associated with each member so we can get the values later
                pim <- proxy_tbl |> select(member, row_select_id, row_check_id) |> data.frame()
                proxy_input_map$pim <- pim
                # Remove unneeded columns and convert to a datatable for rendering
                proxy_tbl <- select(proxy_tbl, -vote, -row_select_id, -row_check_id)
                proxy_dt <- datatable(proxy_tbl, rownames = FALSE, escape = FALSE,
                                      options = list(dom = 't',
                                                     order = list(list(1, 'asc')),
                                                     preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                     drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }'))
                                      )
                return(proxy_dt)
            }
        }
    )
    
    output$submit_vote <- renderText(
        {
            req(input$submit_button)
            member <- input$voter
            calendar_vote <- input$calendar
            board_vote <- input$candidates
            votes <- data.frame(member, calendar_vote, board_vote)
            if(count(proxies()) > 0) {
                # TODO grab the proxy votes and add to the votes df
                print(proxy_input_map$pim)
            }
        # TODO write to remote Google drive
            write_csv(votes, "2023AnnualMeetingVotes.csv", append = TRUE)
            return("Your vote has been submitted. Thanks!")
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# TODO create separate app to read results from remote file
