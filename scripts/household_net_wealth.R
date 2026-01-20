library(tidyverse)
indicators = c(
  # Nation Level
  mnweal = "Market-value national wealth",
  mnwboo = "Book-value national wealth",
  
  
  # Households (mhw*)
  mhweal = "Household net wealth",
  mhwnfa = "Household non-financial assets",
  mhwhou = "Household housing assets",
  mhwdwe = "Dwellings (Household)",
  mhwlan = "Land underlying dwellings (Household)",
  mhwbus = "Household business & other non-financial assets",
  mhwagr = "Agricultural land (Household)",
  mhwodk = "Other domestic capital (Household)",
  mhwfin = "Financial assets (Household)",
  mhwfiw = "Currency, deposits, bonds & loans (Household)",
  mhwcud = "Currency & deposits (Household)",
  mhwbol = "Bonds & loans (Household)",
  mhweqi = "Equities & fund shares (Household)",
  mhwpen = "Pension funds & life insurance (Household)",
  mhwdeb = "Liabilities (Household)",
  mhwfie = "Financial assets except currency & deposits (Household)",
  
  # NPISH / Non-Profit Institutions (miw*)
  miweal = "NPISH net wealth",
  miwnfa = "NPISH non-financial assets",
  miwhou = "NPISH housing assets",
  miwdwe = "Dwellings (NPISH)",
  miwlan = "Land underlying dwellings (NPISH)",
  miwbus = "NPISH business & other non-financial assets",
  miwagr = "Agricultural land (NPISH)",
  miwodk = "Other domestic capital (NPISH)",
  miwfin = "Financial assets (NPISH)",
  miwfiw = "Currency, deposits, bonds & loans (NPISH)",
  miwcud = "Currency & deposits (NPISH)",
  miwbol = "Bonds & loans (NPISH)",
  miweqi = "Equities & fund shares (NPISH)",
  miwpen = "Pension funds & life insurance (NPISH)",
  miwdeb = "Liabilities (NPISH)",
  miwfie = "Financial assets except currency & deposits (NPISH)",
  
  # Government (mgw*)
  mgweal = "Government net wealth",
  mgwnfa = "Government non-financial assets",
  mgwhou = "Government housing assets",
  mgwdwe = "Dwellings (Gov)",
  mgwlan = "Land underlying dwellings (Gov)",
  mgwbus = "Government business & other non-financial assets",
  mgwagr = "Agricultural land (Gov)",
  mgwodk = "Other domestic capital (Gov)",
  mgwfin = "Financial assets (Gov)",
  mgwfiw = "Currency, deposits, bonds & loans (Gov)",
  mgwcud = "Currency & deposits (Gov)",
  mgwbol = "Bonds & loans (Gov)",
  mgweqi = "Equities & fund shares (Gov)",
  mgwpen = "Pension funds & life insurance (Gov)",
  mgwdeb = "Liabilities (Gov)",
  mgwdec = "Consolidated government debt",
  mgwfie = "Financial assets except currency & deposits (Gov)",
  
  
  msavin = "Net National Saving"
) 




household_wid <- download_wid(
  indicators = names(indicators),
  areas = "all", years = "all", ages = "all", pop = "all", verbose = TRUE
)

clean_data <- household_wid %>%
    filter(str_length(country) == 2) %>%
    as_tibble() %>%
    mutate(variable = str_remove_all(variable, "999i")) %>%
    distinct() %>%
    mutate(
        variable = indicators[variable],
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
    arrange(country, year) 

values <- clean_data %>%
  filter(country == "USA", year == 2024) %>% 
  pivot_longer(-c(country, year, iso2c)) %>%
  mutate(code = setNames(names(indicators), indicators)[name]) 

code_value <- setNames(values$value, values$code)

# Structure table specification: WID code, indentation, operator, description
structure_household <- tribble(
  ~code,  ~indent, ~op,  ~desc,
  "mhweal", 0, "(=)", "household net wealth",
  "mhwnfa", 1, "(+)", "household non-financial assets",
  "mhwhou", 2, "(+)", "household housing assets",
  "mhwdwe", 3, "(+)", "dwellings",
  "mhwlan", 3, "(+)", "land underlying dwellings",
  "mhwbus", 2, "(+)", "household business and other non-financial assets",
  "mhwagr", 3, "(+)", "agricultural land",
  "mhwodk", 3, "(+)", "other domestic capital",
  "mhwfin", 1, "(+)", "financial assets",
  "mhwfiw", 2, "(+)", "currency, deposits, bonds, and loans",
  "mhwcud", 3, "(+)", "currency and deposits",
  "mhwbol", 3, "(+)", "bonds and loans",
  "mhweqi", 2, "(+)", "equities and fund shares",
  "mhwpen", 2, "(+)", "pension funds and life insurance",
  "mhwdeb", 1, "(-)", "liabilities",
  "mhwfie", 0, "(=)", "financial assets except for currency and deposits",
  "mhwfin", 1, "(+)", "financial assets",
  "mhwcud", 1, "(-)", "currency and deposits"
)


structure_npish <- tribble(
  ~code,     ~indent, ~op,  ~desc,
  "miweal",   0,       "(=)", "non-profit institutions’ net wealth",
  "miwnfa",   1,       "(+)", "non-profit institutions’ non-financial assets",
  "miwhou",   2,       "(+)", "non-profit institutions’ housing assets",
  "miwdwe",   3,       "(+)", "dwellings",
  "miwlan",   3,       "(+)", "land underlying dwellings",
  "miwbus",   2,       "(+)", "non-profit institutions’ business and other non-financial assets",
  "miwagr",   3,       "(+)", "agricultural land",
  "miwodk",   3,       "(+)", "other domestic capital",
  "miwfin",   1,       "(+)", "financial assets",
  "miwfiw",   2,       "(+)", "currency, deposits, bonds, and loans",
  "miwcud",   3,       "(+)", "currency and deposits",
  "miwbol",   3,       "(+)", "bonds and loans",
  "miweqi",   2,       "(+)", "equities and fund shares",
  "miwpen",   2,       "(+)", "pension funds and life insurance",
  "miwdeb",   1,       "(-)", "liabilities",
  "miwfie",   0,       "(=)", "financial assets except for currency and deposits",
  "miwfin",   1,       "(+)", "financial assets",
  "miwcud",   1,       "(-)", "currency and deposits"
)

structure_public <- tribble(
  ~code,    ~indent, ~op,  ~desc,
  "mgweal",  0,       "(=)", "net wealth of the general government",
  "mgwnfa",  1,       "(+)", "government non-financial assets",
  "mgwhou",  2,       "(+)", "government housing assets",
  "mgwdwe",  3,       "(+)", "dwellings",
  "mgwlan",  3,       "(+)", "land underlying dwellings",
  "mgwbus",  2,       "(+)", "government business and other non-financial assets",
  "mgwagr",  3,       "(+)", "agricultural land",
  "mgwodk",  3,       "(+)", "other domestic capital",
  "mgwfin",  1,       "(+)", "financial assets",
  "mgwfiw",  2,       "(+)", "currency, deposits, bonds, and loans",
  "mgwcud",  3,       "(+)", "currency and deposits",
  "mgwbol",  3,       "(+)", "bonds and loans",
  "mgweqi",  2,       "(+)", "equities and fund shares",
  "mgwpen",  2,       "(+)", "pension funds and life insurance",
  "mgwdeb",  1,       "(-)", "liabilities",
  "mgwdec",  0,       "(=)", "consolidated government debt",
  "mgwfie",  0,       "(=)", "financial assets except for currency and deposits",
  "mgwfin",  1,       "(+)", "financial assets",
  "mgwcud",  1,       "(-)", "currency and deposits"
)



structure <- bind_rows(structure_household, structure_npish, structure_public)
# Format output table
formatted <- structure %>%
  mutate(
    amount = code_value[code] %||% NA_real_,  # match values, allow missing
    desc = indicators[code],
    indent_spaces = strrep("    ", indent),  # 4 spaces per indent level
    line = sprintf("%s%s\t%s %s\t%s",
                   indent_spaces, code, op, desc,
                   ifelse(!is.na(amount), scales::dollar(amount), "???"))
  )

# Display final formatted output
paste(formatted$line, sep = "\n", collapse = "\n") %>% clipr::write_clip()
  


###
#Could I see a comparison between book value (nwboo) and market value (mweal) for the US over time?
  
clean_data %>%
  filter(
    country == "USA"
  ) %>%
  select(
    year,
 "Market-value national wealth",
 "Book-value national wealth",
  ) %>%
  drop_na() %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, linetype = name)) + 
  geom_line(linewidth = 3) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/10^12, suffix = "T"), n.breaks = 6) +
  scale_x_continuous(n.breaks = 9) +
  # directlabels::geom_dl(aes(label = name), method = "smart.grid") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(y = NULL, x = NULL,
       title = "USA Book & Market National Value",
       subtitle = "Book = Solid, Market = Dashed")
  
  
clean_data %>%
  filter(
    country == "USA"
  ) %>%
  select(year,
 "Book-value national wealth",
 "Net National Saving"
         ) %>%
  mutate(
   `Book-value Change` = `Book-value national wealth` - lag(`Book-value national wealth`, 1),
   .keep="unused"
  ) %>%
  drop_na()%>%
  pivot_longer(-year) %>%
  ggplot(aes(x=year, y=value, color = name)) +
  geom_line(linewidth = 2) +
  theme_minimal(base_size = 18) +
  labs(
   x = NULL,
   y = NULL,
   title = "Book National Wealth\nand Net National Saving for the USA"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(scale =10^-12, prefix = "T")) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
  

# household_wid %>%
#   filter(str_length(country) == 2) %>%
#   as_tibble() %>%
#   mutate(variable = str_remove_all(variable, "999i")) %>%
#   distinct() %>%
#   mutate(
#     iso2c = country
#   ) %>%
#   filter(variable == "mgwpen") %>%
#   filter(country == "US")  %>%
#    filter(year == 2024) %>% View()

  

## Topin's Q
clean_data %>%
  filter(
    country == "USA"
  ) %>%
  select(
    year,
 "Market-value national wealth",
 "Book-value national wealth",
  ) %>%
  mutate(value = `Market-value national wealth`/`Book-value national wealth`) %>%
  drop_na() %>%
  ggplot(aes(x = year, y = value)) + 
  geom_line(linewidth = 3) +
  # scale_y_continuous(labels = scales::dollar_format(scale = 1/10^12, suffix = "T"), n.breaks = 6) +
  scale_x_continuous(n.breaks = 9) +
  # directlabels::geom_dl(aes(label = name), method = "smart.grid") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(y = NULL, x = NULL,
       title = "USA Market/Book National Value",
       subtitle = "Tobin Q")
  
  