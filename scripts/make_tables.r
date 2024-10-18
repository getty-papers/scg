source("./scripts/load_libraries.r")

scg_data <- read_csv("./data/scg_data.csv")

setFixest_dict(c(
    # "I(`𝚫s*`)" = "$\\theta_s$",
    # "I(`𝚫qs`)" = "$\\varphi_s$",
    "`𝚫g(K)`" = "Regression of $\\Delta s^*$ on $\\Delta g(K)$",
    "`g(K)`" = "Regression of $s^*$ on \\(g(K)\\)",
    "-I(`𝚫c*`)" = "$\\theta_c$",
    "I(`𝚫qc`)" = "$\\varphi_c$"
))

wid_si_table <- scg_data %>%
    filter(year %in% 1980:2022) %>%
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
    filter(year %in% 1980:2022) %>%
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
    



thetas_table <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    group_by(country) %>%
    drop_na(θs) %>%
    reframe(
        years = paste0(min(year), " - ", max(year), " (", length(year), ")"),
        θs = round(mean(θs, na.rm = TRUE), 2),
    ) %>%
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
    filter(year %in% 1980:2022) %>%
    filter(abs(`g(K)`) > 0.01) %>%
    filter(country %in% countries_to_keep) %>%
    group_by(country) %>%
    drop_na(`g(K)`, `θ*s`, `θ*c`) %>%
    reframe(
        years = paste0(min(year), " - ", max(year), " (", length(year), ")"),
        `θ*s` = round(mean(`θ*s`, na.rm = TRUE), 2),
#        `θ*c` = round(mean(`θ*c`, na.rm = TRUE), 2),
    ) %>%
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
caption <- "Regression of $- \\Delta c^*$ and $\\Delta q_c$ on $\\Delta g(K)$ (Screen = 0.01). $H_0 \\text{ per thrift theory: } \\Delta g(K) \\cong -\\Delta c^* \\& \\Delta q_c \\cong 0$"

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
\\caption{Average \\(\\frac{s^*}{g(K)}\\) in 92 countries (screen = 0.01). Number of years clearing screen shown in ()}\\label{tbl-5}%
\\hline
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
\\hline
\\begin{flushleft}
\\footnotesize \\emph{Note:} Thrift theory predicts \\(\\frac{s^*}{g(K)} = 1\\). Free growth theory makes no prediction for these data.
\\end{flushleft}
\\end{table}"
)

write_file(tex_tbl5, "tables/tbl-5.tex")


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
\\caption{Average \\(\\theta_s^*\\) in 92 countries (screen = 0.01). Number of years clearing screen shown in ()}%
\\hline
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
\\hline
\\label{tbl-indicator_table}
\\begin{flushleft}
\\footnotesize \\emph{Note:} Thrift theory predicts \\(\\theta_s^* = 1\\).
\\end{flushleft}
\\end{table}",
    sep = "\n"
)

write_file(tex_tbl_indicator_table, "tables/tbl-indicator_table.tex")
