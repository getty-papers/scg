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
        summarize(
            wt_mean = weighted.mean(y, weight, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(is.finite(wt_mean))

    x_breaks <- pretty(range(mydata$year, na.rm = TRUE), n = 9)

    out <- ggplot(data = mean_years, aes(x = year, y = wt_mean)) +
        geom_hline(yintercept = 0, color = "black", size = 1) +
        geom_point(
            color = "#ef3b2c",
            size = 3
        ) +
        geom_smooth(
            se = FALSE,
            color = "#386cb0",
            linewidth = 1.2,
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
    make_plot(mydata=., y = 'θs') +
    labs(
        y = TeX("$\\theta_s$"),
        # title = TeX(sprintf("$\\theta_s^*$ (%s) across countries, weighted to GDP", scg_yr_txt)),
        subtitle = "span: 0.7, unscreened"
    )

    

# Snet phi
si_phi_plot <- scg_data %>%
    make_plot(., y ='𝝋s') +
    labs(
        y = TeX("$\\varphi_s^*$"),
        title = TeX(sprintf("$\\varphi_s^*$ (%s) across countries, weighted to GDP", scg_yr_txt))
    )

# C theta
c_theta_plot <- scg_data %>%
    make_plot(., y ='θc') +
    labs(
        y = TeX("$\\theta_c$"),
        title = TeX(sprintf("$\\theta_c$ (%s) across countries, weighted to GDP", scg_yr_txt))
    )


# C phi
c_phi_plot <- scg_data %>%
    make_plot(., y ='𝝋c') +
    labs(
        y = TeX("$\\varphi_c$"),
        title = TeX(sprintf("$\\varphi_c$ (%s) across countries, weighted to GDP", scg_yr_txt))
    )

# new s and c plots
`s_theta*_plot` <- scg_data %>%
    make_plot(., y ='θ*s') +
    labs(
        y = TeX("$\\frac{s^*}{g(K)}$"),
        subtitle = "span: 0.7, unscreened"
    )

`c_theta*_plot` <- scg_data %>%
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

make_history_dot_panel <- function(mydata, value_col, panel_label, y_limits = NULL,
                                   y_title_angle = 90) {
    yearly <- mydata %>%
        group_by(year) %>%
        summarize(
            value = weighted.mean(.data[[value_col]], weight, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(is.finite(value))

    x_breaks <- seq(
        floor(min(yearly$year, na.rm = TRUE) / 20) * 20,
        floor(max(yearly$year, na.rm = TRUE) / 20) * 20,
        by = 20
    )

    if (is.null(y_limits)) {
        y_breaks <- pretty(yearly$value, n = 5)
        y_limits <- range(y_breaks, na.rm = TRUE)
    } else {
        y_breaks <- pretty(y_limits, n = 5)
    }

    avg_value <- weighted.mean(mydata[[value_col]], mydata$weight, na.rm = TRUE)
    y_span <- diff(y_limits)
    avg_y <- y_limits[2] - 0.08 * y_span

    ggplot(yearly, aes(x = year, y = value)) +
        geom_hline(yintercept = 0, color = "grey30", linewidth = 0.5) +
        geom_point(color = "#ef3b2c", size = 2.1) +
        geom_smooth(
            method = "loess",
            se = FALSE,
            span = 0.7,
            color = "#386cb0",
            linewidth = 1.2
        ) +
        coord_cartesian(ylim = y_limits) +
        scale_x_continuous(
            breaks = x_breaks,
            labels = scales::label_number(accuracy = 1)
        ) +
        scale_y_continuous(
            breaks = y_breaks,
            labels = scales::label_number(accuracy = 0.1)
        ) +
        labs(x = NULL, y = panel_label) +
        theme_Publication() +
        theme(
            axis.title.y = element_text(
                angle = y_title_angle,
                vjust = 0.5,
                margin = margin(r = 10)
            ),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(),
            panel.grid.major.x = element_line(color = "#e5e5e5"),
            panel.grid.major.y = element_line(color = "#ececec"),
            panel.grid.minor = element_blank()
        ) +
        annotate(
            "text",
            x = min(yearly$year, na.rm = TRUE),
            y = avg_y,
            label = paste("Average:", round(avg_value, 3)),
            hjust = 0,
            vjust = 1,
            size = 12 / .pt
        )
}

make_history_dot_figure <- function(top_plot, bottom_plot, filename, subtitle) {
    out <- cowplot::plot_grid(
        top_plot,
        bottom_plot,
        ncol = 1,
        align = "v",
        axis = "lr",
        labels = NULL,
        rel_heights = c(1, 1)
    )

    out <- cowplot::ggdraw(out) +
        cowplot::draw_label(
            subtitle,
            x = 0.5,
            y = 0.99,
            hjust = 0.5,
            vjust = 1,
            fontface = "plain",
            size = 11,
            color = "grey30"
        )

    ggsave(
        filename = filename,
        plot = out,
        path = "./figure-pdf/",
        device = "pdf",
        width = 10,
        height = 7.5,
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

history_ratio_plot <- scg_data %>%
    filter(year >= 1870) %>%
    make_history_dot_panel("θ*s", TeX("$\\frac{s^*}{g(K)}$"), y_limits = c(-2, 2),
        y_title_angle = 0)

history_theta_plot <- scg_data %>%
    filter(year >= 1870) %>%
    make_history_dot_panel("θs", TeX("$\\theta_s$"), y_limits = c(-1, 1), y_title_angle = 0)

history_c_ratio_plot <- scg_data %>%
    mutate(`-c_over_g` = -`c*` / `g(K)`) %>%
    filter(year >= 1870) %>%
    make_history_dot_panel(
        "-c_over_g",
        TeX("$\\frac{-c^*}{g(K)}$"),
        y_limits = c(-10, 6),
        y_title_angle = 0
    )

history_c_theta_plot <- scg_data %>%
    filter(year >= 1870) %>%
    make_history_dot_panel("θc", TeX("$\\theta_c$"), y_limits = c(-1, 1), y_title_angle = 0)

make_history_dot_figure(
    top_plot = history_ratio_plot,
    bottom_plot = history_theta_plot,
    filename = "fig-history-savings.pdf",
    subtitle = "span: 0.7, unscreened"
)

make_history_dot_figure(
    top_plot = history_c_ratio_plot,
    bottom_plot = history_c_theta_plot,
    filename = "fig-history-consumption.pdf",
    subtitle = "span: 0.7, unscreened"
)


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
