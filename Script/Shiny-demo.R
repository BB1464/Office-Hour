library(shiny)
library(googlesheets4)

ui <- fluidPage(
  textInput("name", "Name"),
  textInput("email", "Email"),
  actionButton("submit", "Submit")
)

server <- function(input, output, session) {

  # Connect to the Google Sheet
  sheet_url <- "https://docs.google.com/spreadsheets/d/1AzKSsjML1QQPAC4OHLT_I3C0bR_AF7-tcl8j1ECFCXI/edit#gid=0"
  gs4_auth(token = "YOUR-TOKEN")

  #sheet <- gs_url(sheet_url)

  observeEvent(input$submit, {

  name <- input$name
  email <- input$email

  #create unique title
  title <- paste(name, Sys.time(), sep = "_")

  # Create data frame with input values and unique title
  user_input <- data.frame(Title=title, Name=name, Email=email)

  # Write the input to the Google Sheet
  sheet_append(sheet_url, user_input)

  # # Get the user's input
  # name <- input$name
  # email <- input$email
  #
  # # Write the input to the Google Sheet
  # sheet_append(sheet_url, data.frame(c(name, email)))

    # Show a message to the user
    showModal(modalDialog(
      title = "Success",
      "Your information has been saved!"
    ))
  })
}

shinyApp(ui, server)




# Golem Example -----------------------------------------------------------


library(golem)
library(googlesheets)

# Create a new golem project
create_golem("my_app")

# Add a new module to the project
add_module("user_input", "my_app")

# In the module's ui.R file:
moduleUI("user_input")
textInput("name", "Name")
textInput("email", "Email")
actionButton("submit", "Submit")

# In the module's server.R file:
moduleServer("user_input", function(input, output, session) {

  # Connect to the Google Sheet
  sheet_url <- "https://docs.google.com/spreadsheets/d/YOUR-SHEET-ID"
  gs_auth(token = "YOUR-TOKEN")
  sheet <- gs_url(sheet_url)

  observeEvent(input$submit, {
    # Get the user's input
    name <- input$name
    email <- input$email

    # Write the input to the Google Sheet
    gs_add_row(sheet, c(name, email))

    # Show a message to the user
    showModal(modalDialog(
      title = "Success",
      "Your information has been saved!"
    ))
  })
})

# In the main app.R file:
run_app("my_app")
