source("./scripts/load_libraries.r")

# Analysis window for scg_data.csv (plots, tables, Excel, gsc_table, LaTeX year span).
# Lags are computed on full WID history first; then we subset rows here.
# Switch periods by editing this line, e.g. 1870:2022, 1950:2019, or NULL for no filter
# (then scg_data.csv matches scg_data_long.csv).
SCG_ANALYSIS_YEARS <- 1870:2022

body(download_wid)[[2]][[2]] <-  substitute(all(indicators == 'all') && areas == "all")
    
raw_wid_data <- download_wid(
    indicators = c(
        "final_consumption" = "mcongo", "collective_consumption" = "mcolgo",
        "individual_consumption" = "mindgo", "labor_share" = "wlabsh",
        "net_national_savings" = "msavin",
        "market_wealth" = "mnweal",
        "book_wealth" = "mnwboo", "household_expenditure" = "mconhn",
        "gdp" = "mgdpro", "loc_usd_ppp" = "xlcusp"
    ),
    areas = "all", years = "all", ages = "all", pop = "all", verbose = TRUE
)

clean_wid_data <- raw_wid_data %>%
    filter(str_length(country) == 2) %>%
    as_tibble() %>%
    mutate(variable = str_remove_all(variable, "999i")) %>%
    mutate(
        variable = recode(variable,
            "mcongo" = "final_consumption", "mcolgo" = "collective_consumption",
            "mindgo" = "individual_consumption", "wlabsh" = "labor_share",
            "msavin" = "savings", "mnweal" = "market", "mnwboo" = "book",
            "mconhn" = "household_expenditure", "mgdpro" = "gdp", "xlcusp" = "loc_usd_ppp",
            .default = variable
        ),
        iso2c = country
    ) %>%
    select(-percentile) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(country = countrycode::countrycode(iso2c, origin = "iso2c", destination = "country.name")) %>%
    mutate(country = case_match(country,
        "Hong Kong SAR China" ~ "Hong Kong",
        "United States" ~ "USA",
        .default = country
    )) %>%  # recode country names
    drop_na(country) |>
    arrange(country, year) %>%
    mutate(
        pp_conv = last(loc_usd_ppp),
        .by = country
    ) %>%
    mutate(
        gdp = gdp / pp_conv
    ) %>%
    distinct()

write_csv(clean_wid_data, "./data/clean_wid_data.csv")

scg_data <- clean_wid_data %>%
    mutate(final_consumption = final_consumption + household_expenditure) %>%
    select(
        year, savings, country, market, final_consumption,
        gdp
    ) %>%
    ungroup() %>%
    rowwise() %>%
    # drop na K, but keep row if either C or K are present
    filter(all(any(!is.na(final_consumption), !is.na(savings)), !is.na(market))) %>%
    ungroup() %>%
    group_by(country) %>%
    mutate(
        C = final_consumption,
        K = market,
        Snet = savings,
        #
        `𝚫Snet` = Snet - lag(Snet, n = 1L),
        # `𝚫𝚫Snet` = `𝚫Snet` - lag(`𝚫Snet`,n = 1L),
        `𝚫K` = K - lag(K, n = 1L),
        `Qs` = `𝚫K` - Snet,
        `Qc` = `𝚫K` + C,
        # `𝚫𝚫K` = `𝚫K` - lag(`𝚫K`, n = 1L),
        # `𝚫𝚫K/K` = `𝚫𝚫K`/K,
        # `s*` = Snet / K,
        #
        `s*` = Snet / lag(K, n = 1L),
        `g(K)` = `𝚫K` / lag(K, n = 1L),
        `qs` = `Qs` / lag(K, n = 1L),
        `c*` = C / lag(K, n = 1L),
        `qc` = Qc / lag(K, n = 1L),
        `𝚫s*` = `s*` - lag(`s*`, n = 1L),
        `𝚫g(K)` = `g(K)` - lag(`g(K)`, n = 1L),
        `𝚫qs` = `qs` - lag(`qs`, n = 1L),
        `𝚫c*` = `c*` - lag(`c*`, n = 1L),
        `𝚫qc` = `qc` - lag(`qc`, n = 1L),
        θs = `𝚫s*` / `𝚫g(K)`,
        𝝋s = `𝚫qs` / `𝚫g(K)`,
        θc = -`𝚫c*` / `𝚫g(K)`,
        𝝋c = `𝚫qc` / `𝚫g(K)`,
        #
        `1-c*` = 1 - `c*`,
        `θ*s` = `s*` / `g(K)`,
        `θ*c` = `1-c*` / `g(K)`,
        #
        `θs Screen .01` = ifelse(abs(`𝚫g(K)`) > 0.01, θs, NA),
        `θs Screen .025` = ifelse(abs(`𝚫g(K)`) > 0.025, θs, NA),
        `θs Screen .05` = ifelse(abs(`𝚫g(K)`) > 0.05, θs, NA),
        `θs Screen .075` = ifelse(abs(`𝚫g(K)`) > 0.075, θs, NA),
        `θs Screen .1` = ifelse(abs(`𝚫g(K)`) > 0.1, θs, NA),
        `θs Screen .125` = ifelse(abs(`𝚫g(K)`) > 0.125, θs, NA),
        `θs Screen .15` = ifelse(abs(`𝚫g(K)`) > 0.15, θs, NA),
        #
        `θc Screen .01` = ifelse(abs(`𝚫g(K)`) > 0.01, θc, NA),
        `θc Screen .025` = ifelse(abs(`𝚫g(K)`) > 0.025, θc, NA),
        `θc Screen .05` = ifelse(abs(`𝚫g(K)`) > 0.05, θc, NA),
        `θc Screen .075` = ifelse(abs(`𝚫g(K)`) > 0.075, θc, NA),
        `θc Screen .1` = ifelse(abs(`𝚫g(K)`) > 0.1, θc, NA),
        `θc Screen .125` = ifelse(abs(`𝚫g(K)`) > 0.125, θc, NA),
        `θc Screen .15` = ifelse(abs(`𝚫g(K)`) > 0.15, θc, NA),
        #
        `𝝋c Screen .01` = ifelse(abs(`𝚫g(K)`) > 0.01, 𝝋c, NA),
        `𝝋c Screen .025` = ifelse(abs(`𝚫g(K)`) > 0.025, 𝝋c, NA),
        `𝝋c Screen .05` = ifelse(abs(`𝚫g(K)`) > 0.05, 𝝋c, NA),
        `𝝋c Screen .075` = ifelse(abs(`𝚫g(K)`) > 0.075, 𝝋c, NA),
        `𝝋c Screen .1` = ifelse(abs(`𝚫g(K)`) > 0.1, 𝝋c, NA),
        `𝝋c Screen .125` = ifelse(abs(`𝚫g(K)`) > 0.125, 𝝋c, NA),
        `𝝋c Screen .15` = ifelse(abs(`𝚫g(K)`) > 0.15, 𝝋c, NA),
        #
        `𝝋s Screen .01` = ifelse(abs(`𝚫g(K)`) > 0.01, 𝝋s, NA),
        `𝝋s Screen .025` = ifelse(abs(`𝚫g(K)`) > 0.025, 𝝋s, NA),
        `𝝋s Screen .05` = ifelse(abs(`𝚫g(K)`) > 0.05, 𝝋s, NA),
        `𝝋s Screen .075` = ifelse(abs(`𝚫g(K)`) > 0.075, 𝝋s, NA),
        `𝝋s Screen .1` = ifelse(abs(`𝚫g(K)`) > 0.1, 𝝋s, NA),
        `𝝋s Screen .125` = ifelse(abs(`𝚫g(K)`) > 0.125, 𝝋s, NA),
        `𝝋s Screen .15` = ifelse(abs(`𝚫g(K)`) > 0.15, 𝝋s, NA),
        #
        `θ*c Screen .01` = ifelse(abs(`g(K)`) > 0.01, `θ*c`, NA),
        `θ*c Screen .025` = ifelse(abs(`g(K)`) > 0.025, `θ*c`, NA),
        `θ*c Screen .05` = ifelse(abs(`g(K)`) > 0.05, `θ*c`, NA),
        `θ*c Screen .075` = ifelse(abs(`g(K)`) > 0.075, `θ*c`, NA),
        `θ*c Screen .1` = ifelse(abs(`g(K)`) > 0.1, `θ*c`, NA),
        `θ*c Screen .125` = ifelse(abs(`g(K)`) > 0.125, `θ*c`, NA),
        `θ*c Screen .15` = ifelse(abs(`g(K)`) > 0.15, `θ*c`, NA),
        #
        `θ*s Screen .01` = ifelse(abs(`g(K)`) > 0.01, `θ*s`, NA),
        `θ*s Screen .025` = ifelse(abs(`g(K)`) > 0.025, `θ*s`, NA),
        `θ*s Screen .05` = ifelse(abs(`g(K)`) > 0.05, `θ*s`, NA),
        `θ*s Screen .075` = ifelse(abs(`g(K)`) > 0.075, `θ*s`, NA),
        `θ*s Screen .1` = ifelse(abs(`g(K)`) > 0.1, `θ*s`, NA),
        `θ*s Screen .125` = ifelse(abs(`g(K)`) > 0.125, `θ*s`, NA),
        `θ*s Screen .15` = ifelse(abs(`g(K)`) > 0.15, `θ*s`, NA),
    ) %>%
    ungroup() %>%
    mutate(weight = gdp / sum(gdp), .by = year)

scg_data_long <- scg_data
write_csv(scg_data_long, "./data/scg_data_long.csv")

scg_data <- if (is.null(SCG_ANALYSIS_YEARS) || length(SCG_ANALYSIS_YEARS) == 0L) {
    scg_data_long
} else {
    scg_data_long %>% filter(year %in% SCG_ANALYSIS_YEARS)
}

write_csv(scg_data, "./data/scg_data.csv")

scg_year_range <- range(scg_data$year, na.rm = TRUE)
writeLines(
    c(
        "% Auto-generated by scripts/get_wid_data.r (do not edit by hand)",
        sprintf("\\newcommand{\\scgYearRange}{%d--%d}", scg_year_range[1], scg_year_range[2])
    ),
    "./tables/scg_year_span.tex"
)

# Web appendix: full year range; consumers can narrow in the UI.
scg_data_web <- scg_data_long %>%
    select(
        country, year,
        θc, `𝝋c`, θs, `𝝋s`,
        # `θ*s`, `θ*c`,
        `𝚫g(K)`, gdp
    ) %>%
    pivot_longer(-c(country, year, `𝚫g(K)`, gdp)) %>%
    mutate(
        class = ifelse(str_sub(name, 2, 2) == "c", "Consumption", "Savings/Investment"),
        name = str_sub(name, 1, 1)
    ) %>%
    drop_na(value)

write_csv(scg_data_web, "./web_appendix_scg/app/data/scg_data_web.csv")

gsc_table <- scg_data %>%
    reframe(
        years = paste0(min(year), " - ", max(year), " (", length(year), ")"),
        `g(K)` = round(mean(`g(K)`, na.rm = TRUE), 2),
        `c*` = round(mean(`c*`, na.rm = TRUE), 2),
        `s*` = round(mean(`s*`, na.rm = TRUE), 2),
        .by = country
    )

write_csv(gsc_table, "./web_appendix_scg/app/data/gsc_table.csv")
