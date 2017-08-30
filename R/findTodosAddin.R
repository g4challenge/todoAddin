

#' Addin to Find todos
#'
#' @return function to run todoAddin
#' @export
#'
#' @examples
#'   todoAddin:::findTodosAddin()
findTodosAddin <- function(){
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("TODOS"),
    miniUI::miniContentPanel(
      DT::dataTableOutput('res')
    )
  )

  server <- function(input, output, session) {
    res <- findTodos()
    #print(res)
    output$res <- DT::renderDataTable(res, selection='single')
    print("rendered")
    observe({
      input$res_rows_selected
      if(!is.null(input$res_rows_selected)){
        rstudioapi::navigateToFile(res$Filename[input$res_rows_selected], line=res$Line[input$res_rows_selected])
      }
    })
    observeEvent(input$done, {
      stopApp()
    })
  }
  viewer <- shiny::paneViewer(400)
  shiny::runGadget(ui, server, viewer = viewer)

}

