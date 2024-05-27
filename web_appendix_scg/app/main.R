box::use(
    shiny[
        moduleServer, NS, fluidRow, icon, h1, tableOutput, renderTable,
        textOutput, renderText, span,
        fluidPage, headerPanel, sidebarLayout, mainPanel, sidebarPanel,
        tabsetPanel, tabPanel
    ],
    bslib[bs_theme],
    readr[read_csv],
    dplyr[mutate]
    # semantic.dashboard[
    #   dashboardPage,
    #   dashboardHeader, dashboardBody, dashboardSidebar,
    #   sidebarMenu, menuItem
    # ]
)

box::use(
    app / view / plot,
    app / view / table,
    app / view / filters,
    app / view / download,
    app / view / gsc_table
    # app / view / four_country_plots
)

tab <- function(...) {
    shiny::tabPanel(..., class = "p-3 border border-top-0 rounded-bottom")
}


#' @export
ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        theme = bs_theme(
            version = 5,
            bg = "#FFFFFF",
            fg = "#000000",
            primary = "#0199F8",
            secondary = "#FF374B",
            base_font = "Arial",
            with_themer = T
        ),
        headerPanel(title = "Online Appendix Sources of Capital Growth", windowTitle = "SCG Appendix"),
        sidebarLayout(
            sidebarPanel(
                fluidRow(
                    # textOutput(ns("test")),
                    filters$ui(ns("filters")),
                    plot$ui_filters(ns("plot")),
                    download$ui(ns("download"))
                )
            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tab(
                        "Plots",
                        plot$ui_plot(ns("plot"))
                    ),
                    tab(
                        "Tables",
                        table$ui(ns("table"))
                    ),
                    tab(
                        "g,s,c Table",
                        gsc_table$ui(ns("gsc_table"))
                    ),
                    # tab(
                    #     "Four Country Plot",
                    #     four_country_plots$ui(ns("four_country_plots"))
                    # ),
                    tab(
                        ""
                    )
                )
            )
        )
    )
}

#' @export
server <- function(id) {
    moduleServer(id, function(input, output, session) {
        scg_data <- read_csv("app/data/scg_data_web.csv")

        plot_data <- filters$server("filters", scg_data = scg_data)

        output$test <- renderText(plot_data$data())

        plot$server("plot", plot_data = plot_data)

        table$server("table", plot_data = plot_data)

        gsc_table$server("gsc_table")

        download$server("download")

        # four_country_plots$server("four_country_plots", plot_data = scg_data)
    })
}
