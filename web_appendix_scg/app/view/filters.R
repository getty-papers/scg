box::use(
    dplyr[filter, select, distinct, arrange, pull, mutate, count, bind_rows, between],
    tidyr[complete],
    magrittr[`%>%`],
    shiny[
        moduleServer, observeEvent, NS, sliderInput, uiOutput,
        reactive, observe, renderUI, div, span, icon, HTML,
        validate, need
    ],
    shinyWidgets[pickerInput, updatePickerInput, radioGroupButtons, awesomeCheckbox]
)


#' @export
ui <- function(id, scg_data) {
    ns <- NS(id)
    div(
        sliderInput(ns("filter"),
            "Screens",
            min = 0,
            max = 0.2,
            value = 0.01
        ),
        sliderInput(ns("years"),
            "Period",
            min = 1870,
            max = 2022,
            step = 1,
            value = c(1980, 2022),
            sep = ""
        ),
        uiOutput(ns("country_selector")),
        radioGroupButtons(
            inputId = ns("class"),
            label = "Select indicator",
            choices = list(
                "Consumption",
                "Savings/Investment"
            ),
            justified = TRUE,
            selected = "Savings/Investment"
        ),
        awesomeCheckbox(
            inputId = ns("aggregate"),
            label = "Add Aggregate Row",
            value = FALSE,
        ),
        # radioGroupButtons(
        #     inputId = ns("dataset"),
        #     label = "Select Data Source",
        #     choiceNames = list(HTML(paste0(
        #         "<span>PWT </span>",
        #         '<span data-toggle="tooltip" data-placement="right" title="Penn World Table 10.0">',
        #         icon("info-circle"),
        #         "</span>"
        #     )), HTML(paste0(
        #         "<span>WID </span>",
        #         '<span data-toggle="tooltip" data-placement="right" title="World Inequality Database">',
        #         icon("info-circle"),
        #         "</span>"
        #     ))),
        #     choiceValues = c("pwt", "wid"),
        #     justified = TRUE,
        #     selected = "wid"
        # )
    )
}

#' @export
server <- function(id, scg_data, countries) {
    moduleServer(id, function(input, output, session) {
        data <- reactive({
            scg_data %>%
                # filter(dataset %in% input$dataset) %>%
                filter(abs(`𝚫g(K)`) > input$filter) %>%
                filter(class %in% input$class) %>%
                filter(between(x = year, input$years[1], input$years[2])) %>%
                select(-c(`𝚫g(K)`))
                # select(-c(`𝚫g(K)`, dataset))
        })

        country_choices <- reactive({
            scg_data %>%
                # filter(dataset %in% input$dataset) %>%
                filter(between(x = year, input$years[1], input$years[2])) %>%
                select(country) %>%
                distinct() %>%
                arrange(country) %>%
                pull(country)
        })

        height <- reactive({
            validate(
                need(!is.null(input$countries), "Please select a country")
            )
            length(input$countries) * 225 + 225 * input$aggregate
        })

        plot_data <- reactive({
            data() %>%
                filter(country %in% input$countries) %>%
                {
                    `if`(input$aggregate, bind_rows(dplyr::mutate(., country = "all"), .), .)
                }
        })

        n_observations <- reactive({
            data() %>%
                count(country) %>%
                complete(country = country_choices(), fill = list(n = 0), explicit = TRUE) %>%
                arrange(country_choices()) %>%
                pull(n) %>%
                magrittr::divide_by(., 2)
        })

        disabled_choices <- reactive({
            !country_choices() %in% (data() %>% pull(country))
        })

        output$country_selector <- renderUI({
            pickerInput(
                inputId = session$ns("countries"),
                label = "Countries",
                choices = country_choices(),
                selected = input$countries, # Use the input value to remember previous choices
                multiple = TRUE,
                options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE
                ),
                choicesOpt = list(
                    disabled = disabled_choices(),
                    style = ifelse(disabled_choices(),
                        yes = "color: rgba(119, 119, 119, 0.5);",
                        no = ""
                    ),
                    `subtext` = n_observations()
                )
            )
        })

        observe({
            updatePickerInput(
                session = session,
                inputId = session$ns("countries"),
                selected = input$countries # Update the selected choices with the current input value
            )
        })

        list(
            height = height,
            plot_data = plot_data
            # ,data = reactive(input$dataset)
        )
    })
}
