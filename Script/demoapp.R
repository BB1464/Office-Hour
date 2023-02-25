library(shiny)
library(googlesheets4)
library(gargle)
library(shinyjs)
library(bslib)


# Apply boastrap to Improve the appearance of the app ---------------------


#theme = bs_theme(version = 4, bootswatch = "cosmo")



# custom_theme <- bs_theme(
#   version = 5,
#   bg = "#FFFFFF",
#   fg = "#000000",
#   primary = "#0199F8",
#   secondary = "#FF374B",
#   base_font = "Maven Pro"
# )
#
# #bs_theme_preview(theme = custom_theme, with_themer = FALSE)



ui <- bslib::page_fluid(
  theme = bs_theme(version = 5,
                   #bg = "#FFFFFF",
                   #fg = "#000000",
                   #primary = "#0199F8",
                   #secondary = "#FF374B",
                   bootswatch = "darkly"),
  useShinyjs(),

  textInput("name", "Name in Full (Surname first)"),
  selectInput("gender", "Gender", c("Male", "Female")),
  textInput("nationality","Nationality"),
  textInput("iita_unit","IITA Unit"),
  bslib::page_fluid(selectInput("hub","Hub",c("West Africa","Central Africa","East Africa","South Africa")),
                    textInput("iita_supervisor","IITA Supervisor"),
                    textInput("iita_ID_card_number","IITA ID Card Number"),
                    textInput("contract_duration","Contract Expiring Date (YYYY-Month-Day)"),
                    textInput("university","University"),
                    textInput("course_of_study","Course of Study"),
                    selectInput("program","Program",c("Masters","PhD")),
                    selectInput("position_aspiring_for","Position Aspiring For",c("President","Vice President","General Secretry","PRO","Treasurer","Welfare","Assistant General Secretary","Editor in Chief")),
                    textInput("phone_number","Phone Number"),
                    textInput("email", "Email"),
                    actionButton("submit", "Submit"))

)



server <- function(input, output, session) {

  # Connect to the Google Sheet
  sheet_url <- "https://docs.google.com/spreadsheets/d/1avg-OF5LatHwa5u5ZzVMN4WI7P1EtGCWYAg_-sXYUCA/edit#gid=0"

  gs4_auth(cache = "AIzaSyAv8wlcIoz3AVxHh3Z8Byz5W0YE-jBvDJM", email = TRUE)




  observeEvent(input$submit, {

    # Use the validate() function to check that all input fields are filled out
    validate(
      need(input$name != "", "Please enter your name."),
      need(input$email != "", "Please enter your email."),
      need(input$gender != "", "Please select your gender."),
      need(input$nationality != "", "Please enter your nationality."),
      need(input$iita_unit != "", "Please enter your unit"),
      need(input$hub != "", "Please enter your hub"),
      need(input$iita_supervisor != "", "Please enter your IITA Supervisor"),
      need(input$iita_ID_card_number != "", "Please enter your ID Card Number"),
      need(input$university != "", "Please enter your University"),
      need(input$course_of_study != "", "Please enter your Course of Study"),
      need(input$program != "", "Please enter your Program"),
      need(input$position_aspiring_for != "", "Please enter your Position"),
      need(input$phone_number != "", "Please enter your Phone Number"),
      need(input$contract_duration != "", "Please enter your Contract Duration")
    )




    shinyjs::disable("submit")
    # })




    # observeEvent(input$submit, {

    # Get the user's input

    name <- input$name
    email <- input$email
    gender <- input$gender
    nationality <- input$nationality
    iita_unit <- input$iita_unit
    hub <- input$hub
    iita_supervisor <- input$iita_supervisor
    iita_ID_card_number <- input$iita_ID_card_number
    university <- input$university
    course_of_study <- input$course_of_study
    program <- input$program
    position_aspiring_for <- input$position_aspiring_for
    phone_number <- input$phone_number
    contract_duration <- input$contract_duration








    # Do something with the input
    #create unique title
    #title <- paste(name, Sys.time(), sep = "_")

    title <- paste(Sys.time())

    # Create data frame with input values and unique title
    user_input <- data.frame(Title=title, Name=name, Email=email,Gender=gender,Nationality=nationality,
                             iita_unit=iita_unit,hub=hub,iita_supervisor=iita_supervisor,iita_ID_card_number=iita_ID_card_number,contract_duration=contract_duration,
                             university=university,course_of_study=course_of_study,program=program,position_aspiring_for=position_aspiring_for,
                             phone_number=phone_number)

    # Write the input to the Google Sheet
    sheet_append(sheet_url, user_input)





    # disable the input controls
    shinyjs::disable("name")
    shinyjs::disable("email")
    shinyjs::disable("gender")
    shinyjs::disable("nationality")
    shinyjs::disable("iita_unit")
    shinyjs::disable("hub")
    shinyjs::disable("iita_supervisor")
    shinyjs::disable("iita_ID_card_number")
    shinyjs::disable("university")
    shinyjs::disable("course_of_study")
    shinyjs::disable("program")
    shinyjs::disable("position_aspiring_for")
    shinyjs::disable("phone_number")
    shinyjs::disable("contract-duration")
    shinyjs::disable("submit")


    showModal(modalDialog(
      title = "Submission",
      "You have already submitted the form.!"

    ))
  })
}

shinyApp(ui, server)

