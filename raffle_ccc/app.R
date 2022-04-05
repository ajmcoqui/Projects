library(shiny)
library(bslib)
source("global.R")

ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "cerulean"),
    titlePanel("Random Name Picker for CCC Raffles"),

    sidebarLayout(
        sidebarPanel(width = 5,
            numericInput("num_winners", "Number of winners to pick", value = 1),
            h6("You can upload the givesmart output file as-is, as long as it's a CSV file."),
            fileInput("uploaded_file", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
        ),
        mainPanel(
            h3(textOutput("caption")),
            tableOutput("random_name")
        )
    )
)

server <- function(input, output) {
    output$caption <- renderText(
        {
            req(input$num_winners)
            if(input$num_winners > 1) {
                cap <- "And the winners are..."
            } else {
                cap <- "And the winner is..."
            }
            return(cap)
        }
    )
    output$random_name <- renderTable(
        {
            req(input$uploaded_file, input$num_winners)
            rm <- pick_random_name(input$uploaded_file$datapath, input$num_winners)
            return(rm)
        },
        colnames = FALSE
    )
}

shinyApp(ui = ui, server = server)
