box::use(
    shiny[moduleServer, NS, div],
    readr[read_csv],
    reactable[reactable, renderReactable, reactableOutput],
)


#' @export
ui <- function(id) {
    ns <- NS(id)
    div(
        reactableOutput(ns("gsc_table"))
    )
}

#' @export
server <- function(id, plot_data) {
    moduleServer(id, function(input, output, session) {
        output$gsc_table <- renderReactable({
            reactable(
                read_csv("app/data/gsc_table.csv"),
                searchable = TRUE
            )
        })
    })
}
