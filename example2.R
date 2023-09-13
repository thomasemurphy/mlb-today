library(shiny)
library(shinydashboard)
library(magrittr)
library(DT)
library(shinyWidgets)
library(dplyr)

ui <- dashboardPage(
  
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "Table", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    fluidRow(
      tabItems(
        
        tabItem(tabName = "Table",
                sidebarPanel(
                  
                  uiOutput("picker"),
                  
                  checkboxInput("play", strong("I want to play with my data"), value = FALSE),
                  
                  conditionalPanel(
                    condition = "input.play == 1",
                    checkboxInput("change_log2", "Log2 transformation", value = FALSE),
                    checkboxInput("run_sqrt", "sqrt option", value = FALSE)),
                  
                  actionButton("view", "View Selection")
                  
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  dataTableOutput("table")
                  
                )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    mtcars
  })
  
  data1 <- reactive({
    
    dat <- data()
    
    if(input$change_log2){
      dat <- log2(dat)
    }
    
    if(input$run_sqrt){
      dat <- sqrt(dat)
    }
    
    dat
  })
  
  # This is going to select the columns of the table
  output$picker <- renderUI({
    pickerInput(inputId = 'pick',
                label = 'Select columns to display',
                choices = colnames(data()),
                options = list(`actions-box` = TRUE),multiple = T,
                selected = colnames(data()))
  })
  
  #This function will save the "new" table with the selected columns.
  selected_columns <- eventReactive(input$view,{
    selected_columns <- data1() %>%
      select(input$pick)
    return(selected_columns)
    
  })
  
  output$table <- renderDataTable({
    
    datatable(
      selected_columns(),
      filter = list(position = 'top', clear = FALSE),
      selection = "none",
      rownames = FALSE,
      extensions = 'Buttons',
      
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = 'Blrtip',
        buttons =
          list('copy', 'print', list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Counts", title = NULL),
              list(extend = 'excel', filename = "Counts", title = NULL)),
            text = 'Download'
          )),
        lengthMenu = list(c(10, 30, 50, -1),
                          c('10', '30', '50', 'All'))
      ),
      class = "display"
    )
    
    
  },rownames=FALSE)
  
}

shinyApp(ui, server)