library(shiny)

ui <- fluidPage(
  
  fluidRow(
    
    tags$head(
      tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
    ),
    
    textInput(inputId = "txtInp", label = "Label:"),
    numericInput(inputId = "numInp", label = "Label:", value = 0)
  )
)

server <- function(input, output){}


shinyApp(ui, server)