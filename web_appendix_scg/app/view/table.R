box::use(
    shiny[moduleServer, NS, plotOutput, renderPlot, div, renderText, textOutput],
    reactable[reactable, renderReactable, reactableOutput],
    shinyWidgets[radioGroupButtons],
    dplyr[filter, mutate, select]
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    div(
        radioGroupButtons(
            inputId = ns("indicator"),
            label = "Select indicator:",
            choices = list("θ", "𝝋", "both"),
            justified = TRUE,
            selected = "both"
        ),
        reactableOutput(ns("table"))
    )
}

#' @export
server <- function(id, plot_data) {
    moduleServer(id, function(input, output, session) {
        output$table <- renderReactable({
            reactable(
                filter(plot_data$plot_data(), country != "all") |>
                    mutate(value = round(value, 4)) |> select(-c(class, gdp)) |>
                    filter((name %in% input$indicator) | (input$indicator == "both")),
                searchable = TRUE
            )
        })
    })
}
