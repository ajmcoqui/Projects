library(shiny)
library(shinyjs)
library(bslib)
source("global.R")

ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "cerulean"),
    useShinyjs(), # This is just here so the code gets displayed below instead of next to the app
    titlePanel("Random Name Picker for CCC 50/50 Raffle"),

    sidebarLayout(
        sidebarPanel(
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
    shinyjs::runjs('toggleCodePosition();') # This is just here so the code gets displayed below instead of next to the app

    output$caption <- renderText("And the winner is...")
    output$random_name <- renderTable(
        {
            req(input$uploaded_file)
            rm <- pick_random_name(input$uploaded_file$datapath)
            return(rm)
        },
        colnames = FALSE
    )
}

shinyApp(ui = ui, server = server)
