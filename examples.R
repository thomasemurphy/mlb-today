library(shiny)
library(shinyWidgets)

# Define the UI
ui <- bootstrapPage(
  column(3, uiOutput("class_level")),
  column(3,uiOutput("product"))
)


# Define the server code
server <- function(input, output) {
  
  output$class_level <- renderUI({
    selectInput(
      "selected_class",
      label = h4("Classification Level"),
      choices = list(
        "Brand" = "Brand",
        "Brand1" = "Brand1",
        "Brand2" = "Brand2"
      ),
      selected = "Brand"
    )
  })
  
  
  getFlavor <- reactive({
    
    if (input$selected_class =="Brand") {
      return(c( "a " = "a",
                "b" = "b",
                "c" = "c"
      ))
    }
    else if  (input$selected_class =="Brand1")
    {
      return(c(
        "1" = "1",
        "2" = "2",
        "3" = "3"
      ))
    }
    else   (input$selected_class =="Brand2")
    {
      return(
        c(
          "x" = "x",
          "y" = "y",
          "z" = "z"
        ))
    }
    
  })
  
  output$product <- renderUI({
    pickerInput(
      "selected_product",
      label = h4("Product Family"),
      choices = as.list(getFlavor()),
      selected = as.list(getFlavor()),
      options = list(
        `deselect-all-text` = "None",
        `select-all-text` = "Total",
        `actions-box` = TRUE
      ),
      multiple = F,
      width = "100%"
    )
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)