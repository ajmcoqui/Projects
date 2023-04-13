library(shiny)
library(bslib)
source("global.R")

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
        h5("Do you agree that we should shift the member calendar to match the fiscal calendar?"),
        radioButtons("calendar",
                     "",
                     choices = c("yes", "no"),
                     selected = "yes",
                     inline = TRUE),
        h5("Please pick up to three candidates for the CCC Board."),
        checkboxGroupInput(
            "candidates",
            "",
            choices = candidate_list$full_name
        ),
        hr(),
        h4("Proxies"),
        tableOutput("proxy_names"),
        actionButton("submit_button",
                     "Submit vote",
                     icon("broom"),
                     style="color: #2596be; background-color: #2596be; border-color: #2e6da4"),
        textOutput("submit_vote")
    )
)

server <- function(input, output) {

    output$proxy_names <- renderTable(
        {
            req(input$voter)
            proxies <- proxy_list |> 
                filter(proxy == input$voter) |> 
                select(member = voter)
            if (count(proxies) > 0) {
                return(proxies)
            } else {
                message <- "You are not designated as another member's proxy."
                display <- data.frame(message)
                return(display)
            }
        },
        colnames = FALSE
    )
    
    output$submit_vote <- renderText(
        {
            req(input$submit_button)
            # TODO write data to remote spreadsheet
            member <- input$voter
            calendar_vote <- input$calendar
            board_vote <- input$candidates
            votes <- data.frame(member, calendar_vote, board_vote)
            write_csv(votes, "2023AnnualMeetingVotes.csv", append = TRUE)
            return("Your vote has been submitted. Thanks!")
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# TODO create separate app to read results from remote file
