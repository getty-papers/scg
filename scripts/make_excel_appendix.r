source("./scripts/load_libraries.r")

scg_data <- read_csv("./data/scg_data.csv")

screens <- c(
    "θs", "𝝋s", "θc", "𝝋c", "θ*s", "θ*c",
    "θs Screen .01", "θs Screen .025", "θs Screen .05", "θs Screen .075",
    "θs Screen .1", "θs Screen .125", "θs Screen .15",
    "θc Screen .01", "θc Screen .025", "θc Screen .05", "θc Screen .075",
    "θc Screen .1", "θc Screen .125", "θc Screen .15",
    "𝝋c Screen .01", "𝝋c Screen .025", "𝝋c Screen .05", "𝝋c Screen .075",
    "𝝋c Screen .1", "𝝋c Screen .125", "𝝋c Screen .15",
    "𝝋s Screen .01", "𝝋s Screen .025", "𝝋s Screen .05", "𝝋s Screen .075",
    "𝝋s Screen .1", "𝝋s Screen .125", "𝝋s Screen .15",
    "θ*c Screen .01", "θ*c Screen .025", "θ*c Screen .05", "θ*c Screen .075",
    "θ*c Screen .1", "θ*c Screen .125", "θ*c Screen .15",
    "θ*s Screen .01", "θ*s Screen .025", "θ*s Screen .05", "θ*s Screen .075",
    "θ*s Screen .1", "θ*s Screen .125", "θ*s Screen .15"
)

scg_panel <- scg_data %>%
    ungroup() %>%
    select(-any_of(c("savings", "market", "final_consumption", "labor_share"))) %>%
    group_by(country) %>%
    mutate(n_years = n()) %>%
    filter(n_years > 20) %>%
    select(-n_years) %>%
    ungroup() %>%
    arrange(country) %>%
    pivot_longer(-c(year, country)) %>%
    mutate(country = str_replace(country, "Saint Vincent and the Grenadines", "Saint Vincent & Grenadines"))

scg_all <- scg_data %>%
    select(-any_of(c("savings", "market", "final_consumption", "labor_share"))) %>%
    select(year, country, all_of(screens))

countries_sheets <- scg_panel %>%
    group_by(country) %>%
    nest() %>%
    mutate(data = map(data, ~ pivot_wider(., names_from = year, values_from = value))) %>%
    rename("name" = "country")

country_names_sheet <- tibble(name = "Countries", data = scg_panel %>% select(country) %>% distinct() %>% arrange(country) %>% list())

all_sheet <- tibble(name = "All", data = scg_all |> ungroup() %>% list())

sheets_to_write <- bind_rows(all_sheet, country_names_sheet, countries_sheets)


template_path <- "data/saving_excel_template.xlsx"
wb <- loadWorkbook(file = template_path)
sheets_to_remove <- getSheetNames(template_path)[-1]
sheets_to_remove %>% walk(~ removeWorksheet(wb, .))

for (i in 1:nrow(sheets_to_write)) {
    sheet_name <- sheets_to_write[i, 1]
    sheet_data <- sheets_to_write[i, 2] %>% unnest(data)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_data, startRow = 1, startCol = 1)
}
workbook_name <- paste0("./data/", "sources_of_capital_growth_", Sys.Date(), ".xlsx")

saveWorkbook(wb, workbook_name, overwrite = TRUE)

zip(
    paste0(
        "./web_appendix_scg/app/data/",
        "sources_of_capital_growth",
        ".zip"
    ),
    workbook_name
)
