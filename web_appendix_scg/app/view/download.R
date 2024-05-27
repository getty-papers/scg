box::use(
  shiny[moduleServer, NS, downloadHandler, div],
  shinyWidgets[downloadBttn]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
  downloadBttn(ns("download_button"), 
               label = "Download Excel Files",
               style = "material-flat",
               color = "primary")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$download_button <- downloadHandler(
      filename = function() {
        "Excel_files.zip"
      },
      content = function(file) {
        path_to_zip <- "app/data/sources_of_capital_growth.zip"
        file.copy(path_to_zip, file)
      }
    )
  })
}
