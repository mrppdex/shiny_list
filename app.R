library(shiny)

item_UI <- function(id) {
  ns <- NS(id)
  div(
    id = id,
    div(
      id = ns("item_container"),
      style = "border: 1px solid; border-radius: 10px; background-color: #800080; padding: 10px; margin: 10px; width: 20%; min-width: 500px;", # purple fill, rounded corners
      tagList(
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;", # flex container for arranging inputs in a row
          tags$input(id = ns("item_input"), type = "text", class = "form-control", 
                     placeholder = "Enter item here", 
                     style = "background-color: #FFFF00; color: #006400; width: 100%;") # yellow fill, dark green text
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;", # flex container for arranging inputs in a row
          tags$input(id = ns("estimate_input"), type = "text", class = "form-control", 
                     placeholder = "Enter estimate here", 
                     style = "background-color: #FFFF00; color: #006400; width: 33.33%;"), # yellow fill, dark green text
          tags$input(id = ns("lower_bound_input"), type = "text", class = "form-control", 
                     placeholder = "Enter lower bound here", 
                     style = "background-color: #FFFF00; color: #006400; width: 33.33%;"), # yellow fill, dark green text
          tags$input(id = ns("upper_bound_input"), type = "text", class = "form-control", 
                     placeholder = "Enter upper bound here", 
                     style = "background-color: #FFFF00; color: #006400; width: 33.33%;") # yellow fill, dark green text
        ),
        div(
          style = "display: flex; justify-content: flex-start; align-items: center; margin-bottom: 10px;", # flex container for arranging buttons in a row
          actionButton(ns("insert_above_button"), "Insert item above"),
          actionButton(ns("insert_below_button"), "Insert item below")
        )
      )
    )
  )
}

item_Server <- function(id, data, session) {
  print(data$ids)
  moduleServer(
    id = id,
    module = function(input, output, session) {
      observeEvent(input$insert_above_button, {
        data$insertAbove(id)
      })
      observeEvent(input$insert_below_button, {
        data$insertBelow(id)
      })
      
      observe({
        data$values[[id]] <<- list(
          Text = input$item_input,
          Estimate = input$estimate_input,
          Lower = input$lower_bound_input,
          Upper = input$upper_bound_input
        )
        print(data$values[[id]])
      })
      
    },
  session = session
  )
}

ui <- fluidPage(
  actionButton("add_button", "Add item"),
  actionButton("print_ids", "Print IDs"),  # Add a new button for printing IDs
  downloadButton("downloadData", "Save to CSV"),
  tags$div(id = "placeholder")
)

server <- function(input, output, session) {
  data <- reactiveValues(ids = character(0), values = list())
  counter <- reactiveVal(0)
  
  data$add <- function() {
    counter(counter() + 1)
    id <- paste0("item", counter())
    data$ids <- c(data$ids, id)
    insertUI(
      selector = "#placeholder",
      ui = item_UI(id),
      where = "beforeBegin"
    )
    item_Server(id, data, session)
  }
  
  data$insertAbove <- function(id) {
    counter(counter() + 1)
    idx <- match(id, data$ids)
    if (is.na(idx)) {
      stop("Invalid id: ", id)
    }
    newId <- paste0("item", counter())
    data$ids <- append(data$ids, newId, after = idx - 1)
    insertUI(
      selector = paste0("#", id),
      ui = item_UI(newId),
      where = "beforeBegin"
    )
    item_Server(newId, data, session)
  }
  
  data$insertBelow <- function(id) {
    counter(counter() + 1)
    idx <- match(id, data$ids)
    if (is.na(idx)) {
      stop("Invalid id: ", id)
    }
    newId <- paste0("item", counter())
    data$ids <- append(data$ids, newId, after = idx)
    insertUI(
      selector = paste0("#", id),
      ui = item_UI(newId),
      where = "afterEnd"
    )
    item_Server(newId, data, session)
  }
  
  observeEvent(input$add_button, {
    data$add()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      df <- do.call(rbind, lapply(data$values, function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$print_ids, {
    ids <- paste(data$ids, collapse = "\n")
    showModal(modalDialog(
      title = "IDs",
      paste("Current IDs:\n\n", ids)
    ))
  })
}

shinyApp(ui = ui, server = server)
