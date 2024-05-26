box::use(
    shiny[
        moduleServer, NS, div, plotOutput, renderPlot,
        checkboxInput, observeEvent, isolate, req,
        uiOutput, renderUI, reactive, observe, tagList
    ],
    shinyWidgets[radioGroupButtons, actionBttn],
    ggplot2[...],
    dplyr[left_join, group_by, group_split],
    purrr[map],
    magrittr[`%>%`]
)


box::use(
    app / styles / themes[theme_Publication, scale_fill_Publication, scale_colour_Publication]
)

#' @export
ui_filters <- function(id) {
    ns <- NS(id)
    div(
        radioGroupButtons(
            inputId = ns("regression"),
            #  label = "",
            choices = list("lm", "loess"),
            justified = TRUE,
            selected = "loess"
        ),
        checkboxInput(ns("weight"), label = "Weight The Regression by GDP", value = F),
        checkboxInput(ns("point"), label = "Include Points", value = T),
        actionBttn(
            inputId = ns("plot_button"),
            label = "Plot",
            style = "material-flat",
            color = "primary"
        )
    )
}
#' @export
ui_plot <- function(id) {
    ns <- NS(id)
    uiOutput(ns("plots"))
}

#' @export
server <- function(id, plot_data, session) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$plot_button, {
            isolated_input <- isolate({
                list(
                    plot_data = plot_data$plot_data(),
                    height = plot_data$height(),
                    weight = input$weight,
                    regression = input$regression,
                    point = input$point
                )
            })
            list_of_plots <- reactive({
                req(input$plot_button) # ensure plot_button has been clicked
                countries <- unique(isolated_input$plot_data["country"])
                countries["group"] <- rep(1:4, each = 35, length.out = nrow(countries))

                plot_data <- isolated_input$plot_data %>%
                    dplyr::left_join(countries, by = "country") %>%
                    group_by(group) %>%
                    group_split()

                heights <- {
                    table(countries$group) * 225
                }

                print(heights)

                plots <- plot_data %>%
                    map(
                        ~ ggplot(data = ., aes(x = year, y = value, color = name, weight = `if`(isolated_input$weight, gdp, NULL))) +
                            geom_smooth(size = 2, se = F, span = 0.7, method = isolated_input$regression) +
                            {
                                if (isolated_input$point) geom_point(size = 3, alpha = 1, shape = 5)
                            } +
                            geom_hline(yintercept = 1, size = 1, linetype = "dashed") +
                            geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
                            facet_wrap(vars(country), ncol = 1, strip.position = "top", scales = "free_y") +
                            theme_Publication() +
                            scale_colour_Publication() +
                            scale_x_continuous(position = "top")
                    )
                print(plots)
                print(heights)

                list(plots = plots, heights = heights)
            })

            output$plots <- renderUI({
                plot_output_list <- lapply(seq_len(length(list_of_plots()$plots)), function(i) {
                    plotname <- paste0("plot", i)
                    plotOutput(session$ns(plotname), height = list_of_plots()$heights[i])
                })
                do.call(tagList, plot_output_list)
            })

            observe({
                for (i in seq_len(length(list_of_plots()$plots))) {
                    local({
                        my_i <- i
                        plotname <- paste0("plot", my_i)
                        output[[plotname]] <- renderPlot(
                            {
                                list_of_plots()$plots[[my_i]]
                            },
                            height = list_of_plots()$heights[i]
                        )
                    })
                }
            })
        })
    })
}
