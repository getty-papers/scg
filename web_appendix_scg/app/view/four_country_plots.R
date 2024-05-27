box::use(
    shiny[moduleServer, NS, plotOutput, renderPlot, div, renderText, textOutput],
    shinyWidgets[radioGroupButtons],
    dplyr[filter, mutate, select],
    ggplot2[...],
    dplyr[...],
    tidyr[...]
)
box::use(
    app / styles / themes[theme_Publication, scale_fill_Publication, scale_colour_Publication]
)
#' @export
ui <- function(id) {
    ns <- NS(id)
    plotOutput(ns("four_country_plots"), height = "800px")
}

#' @export
server <- function(id, plot_data) {
    moduleServer(id, function(input, output, session) {
        output$four_country_plots <- renderPlot({
            thresholds <- c(0.01, 0.025, 0.05, 0.075, .1, .125, 0.15)
            plot_data |>
                filter(country %in% c(
                    "United States of America", "United Kingdom",
                    "Germany", "France"
                )) |>
                rowwise() |>
                mutate(
                    group = list(thresholds[thresholds < abs(`𝚫g(K)`)]),
                    country = case_match(country,
                        "United States of America" ~ "USA",
                        "United Kingdom" ~ "UK",
                        .default = country
                    )
                ) |>
                unnest(group) |>
                filter(dataset == "pwt") |>
                ggplot(aes(x = year, y = value, color = name)) +
                geom_hline(yintercept = 1, size = 1, linetype = "dashed") +
                geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
                geom_smooth(size = 2, se = F, span = 0.7) +
                # geom_point(size = 3, alpha = 1, shape = 5) +
                facet_grid(
                    cols = vars(country), rows = vars(group),
                    scales = "free_y"
                ) +
                theme_Publication() +
                scale_colour_Publication() +
                scale_x_continuous(position = "top") +
                coord_cartesian(ylim = c(-2, 2), ) +
                scale_y_continuous(breaks = seq(-2, 2, 1)) +
                theme(
                    axis.text.x = element_text(
                        angle = 60,
                        hjust = 0.2
                    ),
                    axis.title.y = element_blank()
                )
        })
    })
}
