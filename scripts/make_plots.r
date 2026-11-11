source("./scripts/load_libraries.r")
source("./scripts/plot_themes.r")

# scg_data.csv is the analysis sample; change SCG_ANALYSIS_YEARS in get_wid_data.r to switch periods.
# Full history: data/scg_data_long.csv — \scgYearRange in main.tex follows scg_data.csv after get_wid_data.r
scg_data <- read_csv("./data/scg_data.csv")
scg_yr <- range(scg_data$year, na.rm = TRUE)
scg_yr_txt <- paste(scg_yr[1], scg_yr[2], sep = "--")

### Make line plots here
make_plot <- function(mydata, x, y) {
    mydata <- mydata %>% select(year, country, 'y' = y, weight)
    means <- mydata %>%
    summarize(
        mean = mean(y, na.rm = TRUE),
        wt_mean = weighted.mean(y, weight, na.rm = TRUE)
    )
    
    mean_years <- mydata %>%
        group_by(year) %>%
        summarize(mean = mean(y, na.rm = TRUE),
                  wt_mean = weighted.mean(y, weight, na.rm = TRUE))
    
    print(mean_years)

    x_breaks <- pretty(range(mydata$year, na.rm = TRUE), n = 9)

    out <- ggplot(data = mydata,
                      aes(x=year, y=y, group = country)
                      ) +
        # geom_point(color = "lightgrey", show.legend = F, alpha = 1) +
        geom_hline(yintercept = 0, color = "black", size = 1) +
        # geom_hline(yintercept = 1, color = "black", size = 1) +
        geom_point(data = mean_years,
                   aes(x = year, y = wt_mean, group = NA),
                   color = "#ef3b2c",
                   size = 3) +
        geom_smooth(aes(weight = weight, group = NA),
                    se = FALSE,
                    color = "#386cb0",
                    size = 1.2,
                    method = "loess",
                    span = 0.7
        )  +
        coord_cartesian(ylim = c(-1, 2), ) +
        scale_y_continuous(breaks = seq(-1, 2, 1)) +
        scale_x_continuous(
            breaks = x_breaks,
            labels = scales::label_number(accuracy = 1)
        ) +
        theme_Publication() +
        scale_colour_Publication() +
        theme(
            panel.grid.major = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
        ) +
    labs(x = "Year") +
        # labs( caption = "Data from World Inequality Database") +
    annotate("text", x = Inf, y = Inf, hjust = "right", vjust = "top", size = 12/.pt, color = "grey",
        label = "Data from World Inequality Database") +
        annotate(
            "text",
            x = min(mydata$year, na.rm = TRUE),
            y = 1.5,
            label = paste("Average:", round(pull(means, "wt_mean"), 3)),
            hjust = "left",
            size = 16 / .pt
        )
        return(out)
}



# Snet theta
si_theta_plot <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(mydata=., y = 'θs') +
    labs(
        y = TeX("$\\theta_s^*$"),
        # title = TeX(sprintf("$\\theta_s^*$ (%s) across countries, weighted to GDP", scg_yr_txt)),
        subtitle = "span: 0.7, screen: = 0.01"
    )

    

# Snet phi
si_phi_plot <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(., y ='𝝋s') +
    labs(
        y = TeX("$\\varphi_s^*$"),
        title = TeX(sprintf("$\\varphi_s^*$ (%s) across countries, weighted to GDP", scg_yr_txt))
    )

# C theta
c_theta_plot <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(., y ='θc') +
    labs(
        y = TeX("$\\theta_c$"),
        title = TeX(sprintf("$\\theta_c$ (%s) across countries, weighted to GDP", scg_yr_txt))
    )


# C phi
c_phi_plot <- scg_data %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(., y ='𝝋c') +
    labs(
        y = TeX("$\\varphi_c$"),
        title = TeX(sprintf("$\\varphi_c$ (%s) across countries, weighted to GDP", scg_yr_txt))
    )

# new s and c plots
`s_theta*_plot` <- scg_data %>%
    filter(abs(`g(K)`) > 0.01) %>%
    make_plot(., y ='θ*s') +
    labs(
        y = TeX("$\\frac{s^*}{g(K)}$"),
        subtitle = "span: 0.7, screen: = 0.01"
    )

`c_theta*_plot` <- scg_data %>%
    filter(abs(`g(K)`) > 0.01) %>%
    make_plot(., y ='θ*c') +
    labs(
        y = TeX("$\\theta^*_c$"),
        title = TeX(sprintf("$\\theta^*_c$ (%s) across countries, weighted to GDP", scg_yr_txt))
    )
    # coord_cartesian(ylim = NULL) +
    # scale_y_continuous(breaks = seq(-50, 50, 10))


save_plot <- function(filename, plot) {
    ggsave(
        filename = filename,
        plot = plot,
        path = "./figure-pdf/",
        device = "pdf",
        width = 10, height = 3.5,
        units = "in",
        dpi = 300
    )
}


save_plot(filename = "fig-si_plots-1.pdf", plot = si_theta_plot)
save_plot(filename = "fig-si_plots-2.pdf", plot = si_phi_plot)

save_plot(filename = "fig-c_plots-1.pdf", plot = c_theta_plot)
save_plot(filename = "fig-c_plots-2.pdf", plot = c_phi_plot)

save_plot(filename = "fig-s_c_theta_plots-1.pdf", plot = `s_theta*_plot`)
save_plot(filename = "fig-s_c_theta_plots-2.pdf", plot = `c_theta*_plot`)


# scg_data %>% 
#     mutate(cs = `C`/`Snet`) %>%
#     filter(between(year, 1980, 2022)) %>%
# #    filter(country == "USA") %>%
#     select(cs, `C`, `Snet`, country, year) %>%
#     ggplot(aes(x = year, y = cs)) +
#     geom_point(alpha = 0.5, color = "lightgrey") +
#     geom_smooth(se = FALSE, linewidth = 2) +
#     theme_minimal(base_size = 18) + 
#     coord_cartesian(ylim = c(-30,30)) +
#     labs(y = "C / Snet", x = "Year")
# 

# scg_data %>%
#     filter(year %in% 1980:2022) %>%
#     filter(abs(`g(K)`) > 0.01) %>%
#     filter(`Snet` < 0) %>% 
#     select(year, country, `Snet`, `s*`) %>%
#     slice_sample(n = 10) %>%
#     kable() %>%
#     clipr::write_clip()

