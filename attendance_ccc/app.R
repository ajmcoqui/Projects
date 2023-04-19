library(shiny)
library(bslib)
library(DT)
library(data.table)
library(purrr)
source("global.R")

ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "cerulean"),
    img(src='ccc_logo.png', align = "top"),
    titlePanel("CCC Annual Meeting Attendance"),
    h4("Select your name and press Submit to check in."),
    h6("If you are someone's designated proxy, don't select them here - we'll load them for you below."),

    mainPanel(
        selectInput("member",
                    "Your name:",
                    choices = member_list$full_name),
        textOutput("proxy_message"),
        br(),
        tableOutput("proxy_names"),
        actionButton("submit_button",
                     "Submit - I'm here!",
                     icon("broom"),
                     style="color: #2596be; background-color: #2596be; border-color: #2e6da4"),
        textOutput("submit_vote")
    )
)

server <- function(input, output) {
    
    proxies <- reactive(
        {
            proxy_list |> 
                filter(proxy == input$member) |> 
                select(member)
        }
    )
    
    output$proxy_message <- renderText(
        {
            if (count(proxies()) > 0) {
                return("It looks like you are designated as a proxy for at least one other CCC member. We'll count them as present, too. \n")
            }
        }
    )
    
    output$proxy_names <- renderTable(
        {
            req(input$member)
            if (count(proxies()) > 0) {
                proxy_tbl <- data.frame(proxies()) |> select(proxy_member = member)
                return(proxy_tbl)
            }
        }
    )

    output$submit_vote <- renderText(
        {
            req(input$submit_button)
            # Capture the name of the present member
            member <- input$member
            timestamp <- Sys.time()
            attendance <- data.frame(member, status = "present", timestamp)
            
            # If there are proxies, capture those names, too
            if(count(proxies()) > 0) {
                proxy_attendance <- data.frame(member = proxies()$member, status = "proxy", timestamp)
                attendance <- rbind(attendance, proxy_attendance)
            }
            
            # Write results to remote spreadsheet
            withProgress(message = "Logging your attendance...", {
                wb <- drive_get("ccc_attendance_2023")
                new_entry <- attendance
                sheet_append(wb$id, new_entry)
            })
            return("Your attendance has been submitted. Thanks!")
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
