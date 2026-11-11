source("./scripts/load_libraries.r")

# library("tidyverse")
# library("fixest")

# Same sample as scg_data.csv; toggle SCG_ANALYSIS_YEARS in get_wid_data.r. Full panel: scg_data_long.csv
scg_data <- read_csv("./data/scg_data.csv")
clean_wid_data <- read_csv("./data/clean_wid_data.csv") %>%
    mutate(final_consumption = final_consumption + household_expenditure)

setFixest_dict(c(
    # "I(`𝚫s*`)" = "$\\theta_s$",
    # "I(`𝚫qs`)" = "$\\varphi_s$",
    "`𝚫g(K)`" = "Regression of $\\Delta s^*$ on $\\Delta g(K)$",
    "`g(K)`" = "Regression of $s^*$ on \\(g(K)\\)",
    "-I(`𝚫c*`)" = "$\\theta_c$",
    "I(`𝚫qc`)" = "$\\varphi_c$"
))

wid_si_table <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    fixest::feols(c(I(`𝚫s*`)) ~ `𝚫g(K)` | year + country,
        weights = ~weight,
        vcov = "HC1"
    ) %>%
etable(
    tex = TRUE, depvar = FALSE, style.tex = style.tex("aer"),
    headers = list("$\\Delta s^*$" = 1)
)

# wid_c_table <- scg_data %>%
#     filter(year %in% 1980:2022) %>%
#     filter(abs(`𝚫g(K)`) > 0.01) %>%
#     fixest::feols(c(-I(`𝚫c*`), I(`𝚫qc`)) ~ `𝚫g(K)` | year + country,
#     weight = weight,
#         vcov = "HC1") %>%
#     etable(
#         tex = TRUE, depvar = FALSE, style.tex = style.tex("aer"),
#         headers = list("$-\\Delta c^*$" = 1, "$\\Delta q_c$" = 1)
#     )

`wid_s*_c*_table` <- scg_data %>%
    filter(abs(`g(K)`) > 0.01) %>%
    fixest::feols(I(`s*`) ~ `g(K)` | year + country,
        weights = ~weight,
        vcov = "HC1"
    ) %>%
     etable(
         tex = TRUE,
         depvar = FALSE,
         style.tex = style.tex("aer"),
         headers = c("$s^*$")
     )
    

focus_countries <- c("USA", "United Kingdom", "France", "Germany")
coverage_series <- c(
    "Consumption" = "final_consumption",
    "Savings" = "savings",
    "Capital" = "market",
    "GDP" = "gdp"
)

country_series_coverage <- bind_rows(lapply(names(coverage_series), function(series_label) {
    series_col <- coverage_series[[series_label]]
    clean_wid_data %>%
        filter(country %in% focus_countries) %>%
        group_by(country) %>%
        summarize(
            series = series_label,
            first_year = ifelse(
                any(!is.na(.data[[series_col]])),
                min(year[!is.na(.data[[series_col]])]),
                NA_real_
            ),
            last_year = ifelse(
                any(!is.na(.data[[series_col]])),
                max(year[!is.na(.data[[series_col]])]),
                NA_real_
            ),
            observations = sum(!is.na(.data[[series_col]])),
            .groups = "drop"
        )
})) %>%
    mutate(period = paste0(first_year, " - ", last_year, " (", observations, ")")) %>%
    select(country, series, period) %>%
    pivot_wider(names_from = series, values_from = period) %>%
    mutate(country = factor(country, levels = focus_countries)) %>%
    arrange(country) %>%
    mutate(country = as.character(country))


ratio_coverage <- scg_data %>%
    group_by(country) %>%
    summarize(
        first_year = ifelse(
            any(is.finite(`θ*s`)),
            min(year[is.finite(`θ*s`)]),
            NA_real_
        ),
        last_year = ifelse(
            any(is.finite(`θ*s`)),
            max(year[is.finite(`θ*s`)]),
            NA_real_
        ),
        .groups = "drop"
    )

indicator_coverage <- scg_data %>%
    group_by(country) %>%
    summarize(
        first_year = ifelse(
            any(is.finite(θs)),
            min(year[is.finite(θs)]),
            NA_real_
        ),
        last_year = ifelse(
            any(is.finite(θs)),
            max(year[is.finite(θs)]),
            NA_real_
        ),
        .groups = "drop"
    )


thetas_table <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    group_by(country) %>%
    drop_na(θs) %>%
    reframe(
        retained_years = length(year),
        θs = round(mean(θs, na.rm = TRUE), 2),
    ) %>%
    left_join(indicator_coverage, by = "country") %>%
    mutate(years = paste0(first_year, " - ", last_year, " (", retained_years, ")")) %>%
    select(country, years, θs) %>%
    mutate(split = row_number() <= max(row_number() / 2)) %>%
    split(.$split) %>%
    imap(~ kable(.x %>% select(-split), "latex",
        booktabs = TRUE,
        col.names = c("Country", "Period", "$\\theta_s^*$"), escape = FALSE
    ))


# Last section

countries_to_keep <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    drop_na(`𝝋c`, `𝝋s`) %>%
    pull(country) |>
    unique()

theta_table <- scg_data %>%
    filter(abs(`g(K)`) > 0.01) %>%
    filter(country %in% countries_to_keep) %>%
    group_by(country) %>%
    drop_na(`g(K)`, `θ*s`, `θ*c`) %>%
    reframe(
        retained_years = length(year),
        `θ*s` = round(mean(`θ*s`, na.rm = TRUE), 2),
#        `θ*c` = round(mean(`θ*c`, na.rm = TRUE), 2),
    ) %>%
    left_join(ratio_coverage, by = "country") %>%
    mutate(years = paste0(first_year, " - ", last_year, " (", retained_years, ")")) %>%
    select(country, years, `θ*s`) %>%
    mutate(split = row_number() >= max(row_number() / 2) + 1) %>%
    split(.$split) %>%
    imap(~ kable(.x %>% select(-split), "latex",
        booktabs = TRUE,
        col.names = c("Country", "Period",
                      "$\\frac{s^*}{g(K)}$"
                     # "$\\theta^*_{c,i}$"
                      ),
        escape = FALSE
    ))

get_content <- function(table) {
    start <- str_which(table, "\\\\midrule") + 1
    end <- str_which(table, "\\\\bottomrule") - 1
    return(table[start:end])
}

thetas_table$`TRUE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content()
thetas_table$`FALSE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content()

theta_table$`TRUE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content()
theta_table$`FALSE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content()


# Define the template for the table
reg_table <- function(content, label) {
    paste0(
         "\\centering
\\begin{tabularx}{\\columnwidth}{lcc}\n",
        content, "\n",
        "\\end{tabularx}
   \\label{", label, "}\n"
    ) |>
    str_replace("year", "Year") |>
    str_replace("country", "Country")
}

# Define the caption and label
caption <- "Regression of $- \\Delta c^*$ and $\\Delta q_c$ on $\\Delta g(K)$. Screen = 0.01. $H_0 \\text{ per thrift theory: } \\Delta g(K) \\cong -\\Delta c^* \\& \\Delta q_c \\cong 0$"

# tex_wid_c_table <- reg_table(
#     "Regression of $- \\Delta c^*$ and $\\Delta q_c$ on $\\Delta g(K)$ (Screen = 0.01). $H_0 \\text{ per thrift theory: } \\Delta g(K) \\cong -\\Delta c^* \\& \\Delta q_c \\cong 0$",
#     paste(get_content(wid_c_table)[-3], collapse = "\n"),
#     label = "tbl-wid_c_table"
# )
#
# write_file(tex_wid_c_table, "tables/tbl-wid_c_table.tex")

 tex_wid_si_table <- reg_table(
     paste(get_content(wid_si_table)[-3], collapse = "\n"),
     label = "tbl-wid_si_table"
 )
 
 write_file(tex_wid_si_table, "tables/tbl-wid_si_table.tex")
 
 `tex_wid_s*_c*_table` <- reg_table(
     paste(get_content(`wid_s*_c*_table`)[-3], collapse = "\n"),
     label = "tbl-4"
 )
 
 write_file(`tex_wid_s*_c*_table`, "tables/tbl-4.tex")

right_part_tbl5 <- theta_table$`TRUE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")

left_part_tbl5 <- theta_table$`FALSE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")

tex_tbl5 <- paste(
    "\\begin{table}[pos=h]
\\caption{Country-level summary of $\\frac{s^*}{g(K)}$ in the current draft sample. Screen = 0.01. Period reports unscreened country coverage in the analysis file; number of years clearing screen shown in ()}\\label{tbl-5}%
\\hrule
\\makebox[\\textwidth][c]{%
{\\centering

\\begin{tabular}{llrr}",
    
    left_part_tbl5,
    "\\end{tabular}
}

{\\centering
\\begin{tabular}{llrr}",
    right_part_tbl5,
    "\\end{tabular}

}
}
\\hrule
\\begin{flushleft}
\\footnotesize \\emph{Note:} This table is based on observations with $|g(K)| > 0.01$ in the current analysis file.
\\end{flushleft}
\\end{table}"
)

write_file(tex_tbl5, "tables/tbl-5.tex")

tex_country_series_coverage <- paste(
    "\\begin{table}[H]
\\caption{Coverage of the underlying series for France, Germany, the United Kingdom, and the USA in the raw cleaned WID file before the analysis-sample filter. This table is not screened. Each entry reports first year, last year, and number of non-missing observations.}\\label{tbl-country-series-coverage}
\\hrule
\\centering
\\begin{tabularx}{\\textwidth}{lXXXX}",
    paste(
        c(
            "\\toprule",
            "Country & Consumption & Savings & Capital & GDP\\\\",
            "\\midrule",
            apply(country_series_coverage, 1, function(row) paste0(paste(row, collapse = " & "), "\\\\")),
            "\\bottomrule"
        ),
        collapse = "\n"
    ),
    "\\end{tabularx}
\\hrule
\\end{table}",
    sep = "\n"
)

write_file(tex_country_series_coverage, "tables/tbl-country_series_coverage.tex")


right_part_tbl_indicator_table <- thetas_table$`TRUE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")

left_part_tbl_indicator_table <- thetas_table$`FALSE` |>
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")

tex_tbl_indicator_table <- paste("\\begin{table}[H]
\\caption{Country-level summary of $\\theta_s^*$ in the current draft sample. Screen = 0.01. Period reports unscreened country coverage in the analysis file; number of years clearing screen shown in ()}%
\\hrule
\\makebox[\\textwidth][c]{%
{\\centering

\\begin{tabular}{llrr}",
    right_part_tbl_indicator_table,
    "\\end{tabular}
}

{\\centering
\\begin{tabular}{llrr}",
    left_part_tbl_indicator_table,
    "\\end{tabular}

}

}
\\hrule
\\label{tbl-indicator_table}
\\begin{flushleft}
\\footnotesize \\emph{Note:} This table is based on observations with $|\\Delta g(K)| > 0.01$ in the current analysis file.

\\end{flushleft}
\\end{table}",
    sep = "\n"
)

write_file(tex_tbl_indicator_table, "tables/tbl-indicator_table.tex")
