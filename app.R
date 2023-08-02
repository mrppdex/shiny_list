library(shiny)

item_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$script(HTML("
    $(document).on('keypress', 'input[id$=item_input]', function (e) {
        console.log(e.which);
        if (e.which == 13) {
            console.log('Enter pressed...');
            e.preventDefault();
            var $button = $('html').find('button[id$=add_button]');
            if ($button.length) {
                console.log('Button found, about to programmatically click the button...');
                $button.click();
            } else {
                console.log('Button not found');
            }
        }
    })
    "))),
    textInput(ns("item_input"), "Item:"),
    actionButton(ns("add_button"), "Add item"),
    actionButton(ns("insert_above_button"), "Insert item above"),
    actionButton(ns("insert_below_button"), "Insert item below"),
    actionButton(ns("remove_button"), "Remove item"),
    downloadButton(ns("downloadData"), "Export to CSV"),
    tags$hr(),
    uiOutput(ns("item_list"))
  )
}


item_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      items <- reactiveVal(list())
      
      observeEvent(input$add_button, {
        if (input$item_input!="") {
          items(c(items(), input$item_input))
          updateTextInput(session, "item_input", value = "")
          updateRadioButtons(session, "radio_button", selected = length(items())) 
        }
      })
      
      observeEvent(input$insert_above_button, {
        if (input$item_input=="" | length(items())==0) return()
        sel <- as.numeric(input$radio_button)
        if (is.null(sel)) {
          items(c(input$item_input, items()))
        } else if (sel == 1) {
          items(c(input$item_input, items()))
        } else {
          items(c(items()[1:(sel-1)], input$item_input, items()[sel:length(items())]))
        }
        updateTextInput(session, "item_input", value = "")
        updateRadioButtons(session, "radio_button", selected = sel)
      })
      
      
      observeEvent(input$insert_below_button, {
        sel <- as.numeric(input$radio_button)
        if (input$item_input=="" | length(items())==0) return()
        if (is.null(sel)) {
          items(c(items(), input$item_input))
        } else {
          if (sel == length(items())) {
            items(c(items(), input$item_input))
          } else {
            items(c(items()[1:sel], input$item_input, items()[(sel+1):length(items())]))
          }
        }
        updateTextInput(session, "item_input", value = "")
        updateRadioButtons(session, "radio_button", selected = sel + 1)
      })
      
      observeEvent(input$remove_button, {
        sel <- as.numeric(input$radio_button)
        if (!is.null(sel)) {
          items(items()[-sel])
        }
        updateTextInput(session, "item_input", value = "")
      })
      
      output$item_list <- renderUI({
        itemList <- items()
        if (length(itemList) > 0) {
          choices = setNames(as.list(seq_along(itemList)), itemList)
          div(
            radioButtons(session$ns("radio_button"), NULL, choices = choices, selected = NULL)
          )
        }
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(id, ".csv", sep = "")
        },
        content = function(file) {
          df <- data.frame(Item = unlist(items()))
          write.csv(df, file, row.names = FALSE)
        }
      )
    }
  )
}




ui <- fluidPage(
  item_UI("item1")
)

server <- function(input, output, session) {
  item_Server("item1")
}

shinyApp(ui = ui, server = server)
