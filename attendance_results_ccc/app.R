library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(purrr)
source("global.R")

ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "cerulean"),
    img(src='ccc_logo.png', align = "top"),
    titlePanel("CCC Attendance Results"),

    mainPanel(
        actionButton("refresh", "Refresh data", style="color: #2596be; background-color: #2596be; border-color: #2e6da4"),
        h5("Last Updated:"),
        textOutput("time_of_refresh"),
        h5("Number of members who have signed in (including proxies):"),
        tableOutput("attendance_counts")
    )
)

server <- function(input, output, session) {
    
    output$quorum <- renderText({
        req(input$refresh)
        as.character(56)
    })

    # Get the attendance data when the Refresh button is clicked    
    raw_attendance_data <- reactive({
        req(input$refresh)
        attendance_sheet <- drive_get("ccc_attendance_2023")
        member_votes <- read_sheet(attendance_sheet)
        member_votes
    })

    # When the Refresh button is clicked, calculate the number of unique members in the data set.
    output$attendance_counts <- renderTable({
        req(input$refresh)
        quorum = 56L
        unique_rows <- raw_attendance_data() |> unique()
        unique_members <- unique_rows |> select(member, status) |> unique()
        total_members <- unique_members$member |> unique() |> length()
        present_members <- unique_members |> filter(status == "present") |> unique() |> count()
        members_by_proxy <- unique_members |> filter(status == "proxy") |> unique() |> count()
        data.frame(quorum, total_members, present_members = present_members$n, members_by_proxy = members_by_proxy$n)
    })
    
    output$time_of_refresh <- renderText({
        req(input$refresh)
        Sys.time() |> ymd_hms(tz = "America/New_York") |> as.character()
    })
}

shinyApp(ui = ui, server = server)
