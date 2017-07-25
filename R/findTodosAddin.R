

#' Addin to Find todos
#'
#' @return function to run todoAddin
#' @export
#'
#' @examples
#'   todoAddin:::findTodosAddin()
findTodosAddin <- function(){
  library(shiny)
  library(miniUI)
  source("R/findTodos.R")
  library(dplyr)
  library(tidyr)
  library(DT)
  ui <- miniPage(
    gadgetTitleBar("TODOS"),
    miniContentPanel(
      ## Your UI items go here.
      dataTableOutput('res')
    )
  )

  server <- function(input, output, session) {

    ## Your reactive logic goes here.
    res <- findTodos() %>% unnest(filename) %>% unnest(code)
    print(res)
    output$res <- DT::renderDataTable(res %>% as.data.frame(), selection='single')
    observe({
      input$res_rows_selected
      if(!is.null(input$res_rows_selected)){
        rstudioapi::navigateToFile(res$filename[input$res_rows_selected], line=res$rowname[input$res_rows_selected])
      }
    })
    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    observeEvent(input$done, {

      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      stopApp()
    })
  }

  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)

}

