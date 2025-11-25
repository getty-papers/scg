source("./scripts/load_libraries.r")

body(download_wid)[[2]][[2]] <-  substitute(all(indicators == 'all') && areas == "all")
    
raw_wid_data <- download_wid(
    indicators = c(
        "final_consumption" = "mcongo", "collective_consumption" = "mcolgo",
        "individual_consumption" = "mindgo", "labor_share" = "wlabsh",
        "net_national_savings" = "msavin", "market_wealth" = "mnweal",
        "book_wealth" = "mnwboo", "household_expenditure" = "mconhn",
        "gdp" = "mgdpro", "loc_usd_ppp" = "xlcusp",
        "national_non_financial_assets" = "wnwnfa",
            "national_housing_assets" = "mnwhou",
            "national_business_and_nonfinancial_assests" = "mnwbus",
        "net_foreign_assets" = "mnwnxa"
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
            "wnwnfa" = "national_non_financial_assets" ,
                "mnwhou" = "national_housing_assets",
                "mnwbus" = "national_business_and_nonfinancial_assests",
            "mnwnxa" = "net_foreign_assets",
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


clean_wid_data %>%
    select(
        country,
        year,
        market,
        national_non_financial_assets,
        # national_housing_assets,
        # national_business_and_nonfinancial_assests,
        net_foreign_assets
    ) %>%
    filter(
        country == "USA"
    ) %>%
    select(-country) %>%
    tail()

    pivot_longer(
        -year
    ) %>%
    ggplot(aes(x = year, y = value, color = name)) +
    geom_line()
