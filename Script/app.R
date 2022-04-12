# Shiny Intro Demo
library(shiny)
library(ggplot2)

ui<- fluidPage(

shiny::tags$h1('My First Shiny App'),
shiny::tags$p('This is an introduction to shiny app development'),
shiny::selectInput(inputId = 'colour_name',label = 'Please Select a Colour',choices = c('green','yellow','blue'),selected = 'red'),

shiny::tableOutput(outputId = 'tbl1'),


shiny::plotOutput(outputId = 'plt1')


)


server <- function(input,output,session){

output$plt1 <- shiny::renderPlot({ggplot(data = iris,mapping = aes(x = Species,y = Sepal.Length,fill=input$colour_name))+
  geom_col()

})

output$tbl1 <- shiny::renderTable({
  head(mtcars)
})

}


shinyApp(ui = ui,server = server)
