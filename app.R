# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  "My first webapp using Rshiny",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), 
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             
                           ) 
                  ), 
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) 
) 

# Defining server funct 
server <- function(input, output) {
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
}

# Creating Shiny object
shinyApp(ui = ui, server = server)