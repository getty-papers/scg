source("./scripts/load_libraries.r")
source("./scripts/plot_themes.r")

scg_data <- read_csv("./data/scg_data.csv")

### Make line plots here
make_plot <- function(mydata, x, y) {
    mydata <- mydata %>% select(year, country, 'y' = y, weight)
    means <- mydata %>%
    summarize(
        mean = mean(y, na.rm = TRUE),
        wt_mean = weighted.mean(y, weight, na.rm = TRUE)
    )
        #
        out <- ggplot(data = mydata,
                      aes(x=year, y=y, group = country)
                      ) +
        geom_point(color = "lightgrey", show.legend = F, alpha = 0.5) +
        geom_hline(yintercept = 0, color = "black", size = 1) +
        geom_hline(yintercept = 1, color = "black", size = 1) +
        geom_smooth(aes(weight = weight, group = NA),
            se = FALSE,
            color = "#386cb0",
            size = 1.5,
            method = "loess",
            span = 0.7
        ) +
        # geom_smooth(
        #     method = "loess", span = 0.7,
        #     #  formula = y ~ x + I(x^2),
        #     show.legend = FALSE,
        #     aes(group = NULL),
        #     color = "#ef3b2c",
        #     se = FALSE,
        #     size = 1.5
        # ) +
        coord_cartesian(ylim = c(-3, 3), ) +
        scale_y_continuous(breaks = seq(-3, 3, 1)) +
        scale_x_continuous(breaks = seq(1980, 2022, 1)) +
        theme_Publication() +
        scale_colour_Publication() +
        theme(panel.grid.major = element_blank(),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
        labs(
            subtitle = '<span style="color:#386cb0;"><strong>smooth loess</strong></span>,<br> span: 0.7, screen: > 0.01',
            caption = "Data from World Inequality Database"
        ) +
        annotate("text", x = 1980, y = -2,
                 label = paste("Average:", round(pull(means, "wt_mean"),3)), hjust = "left", size = 16/.pt) 
        # annotate("text",
        #          label = round(pull(means, "mean"),3),
        #          color = "red",
        #          hjust = "left",
        #          x = 1991, y = -2.3, size = 16/.pt) +
        # annotate("text",
        #          label = round(pull(means, "wt_mean"),3),
        #          color = "blue",
        #          hjust = "left",
        #          x = 1991, y = -1.7, size = 16/.pt)

        return(out)
}



# Snet theta
si_theta_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(mydata=., y = 'θs') +
    labs(
        y = TeX("$\\theta_s^*$"),
        x = "Year",
        # title = TeX("$\\theta_s^*$ 1980:2022 across countries, weighted to GDP")
    )


# Snet phi
si_phi_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(., y ='𝝋s') +
    labs(
        y = TeX("$\\varphi_s^*$"), x = "Year",
        title = TeX("$\\varphi_s^*$ 1980:2022 across countries, weighted to GDP")
    )

# C theta
c_theta_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(., y ='θc') +
    labs(
        y = TeX("$\\theta_c$"), x = "Year",
        title = TeX("$\\theta_c$ 1980:2022 across countries, weighted to GDP")
    )


# C phi
c_phi_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    make_plot(., y ='𝝋c') +
    labs(
        y = TeX("$\\varphi_c$"), x = "Year",
        title = TeX("$\\varphi_c$ 1980:2022 across countries, weighted to GDP")
    )

# new s and c plots
`s_theta*_plot` <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`g(K)`) > 0.01) %>%
    make_plot(., y ='θ*s') +
    labs(
        # y = TeX("$\\theta^*_s$"),
        y = TeX("$\\frac{s^*}{g(K)}$"),
        x = "Year",
        # title = TeX("$\\theta^*_s$ 1980:2022 across countries")
        # title = TeX("$\\frac{s^*}{g(K)}$ 1980:2022 across countries, weighted to GDP")
    )

`c_theta*_plot` <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`g(K)`) > 0.01) %>%
    make_plot(., y ='θ*c') +
    labs(
        y = TeX("$\\theta^*_c$"), x = "Year",
        title = TeX("$\\theta^*_c$ 1980:2022 across countries, weighted to GDP")
    ) 
    # coord_cartesian(ylim = NULL) +
    # scale_y_continuous(breaks = seq(-50, 50, 10))


save_plot <- function(filename, plot) {
    ggsave(
        filename = filename,
        plot = plot,
        path = "./figure-pdf/",
        device = "pdf",
        width = 10, height = 5,
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