source("./scripts/load_libraries.r")
source("./scripts/plot_themes.r")

scg_data <- read_csv("./data/scg_data.csv")

### Make line plots here
make_plot <- function(p) {
    p +
        geom_point(color = "lightgrey", show.legend = F, alpha = 0.3) +
        geom_hline(yintercept = 0, color = "black", size = 1) +
        geom_hline(yintercept = 1, color = "black", size = 1) +
        geom_smooth(aes(weight = weight, group = NA),
            se = FALSE,
            color = "#386cb0",
            size = 1.5,
            method = "loess",
            span = 0.7
        ) +
        geom_smooth(
            method = "loess", span = 0.7,
            #  formula = y ~ x + I(x^2),
            show.legend = FALSE,
            aes(group = NULL),
            color = "#ef3b2c",
            se = FALSE,
            size = 1.5
        ) +
        coord_cartesian(ylim = c(-3, 3), ) +
        scale_y_continuous(breaks = seq(-3, 3, 1)) +
        theme_Publication() +
        scale_colour_Publication() +
        theme(panel.grid.major = element_blank()) +
        labs(
            subtitle = '<span style="color:#ef3b2c;"><strong>smooth loess</strong></span>, <span style="color:#386cb0;"><strong>weighted to GDP</strong></span>,<br> span: 0.7, screen: > 0.01',
            caption = "Data from World Inequality Database"
        )
}

# Snet theta
si_theta_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    ggplot(aes(x = year, y = `θs`, group = country)) %>%
    make_plot() +
    labs(
        y = TeX("$\\theta_s$"), x = "Year",
        title = TeX("$\\theta_s$ 1980:2022 across countries")
    )


# Snet phi
si_phi_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    ggplot(aes(x = year, y = `𝝋s`, group = country)) %>%
    make_plot() +
    labs(
        y = TeX("$\\varphi_s$"), x = "Year",
        title = TeX("$\\varphi_s$ 1980:2022 across countries")
    )

# C theta
c_theta_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    ggplot(aes(x = year, y = `θc`, group = country)) %>%
    make_plot() +
    labs(
        y = TeX("$\\theta_c$"), x = "Year",
        title = TeX("$\\theta_c$ 1980:2022 across countries")
    )


# C phi
c_phi_plot <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`𝚫g(K)`) > 0.01) %>%
    ggplot(aes(x = year, y = `𝝋c`, group = country)) %>%
    make_plot() +
    labs(
        y = TeX("$\\varphi_c$"), x = "Year",
        title = TeX("$\\varphi_c$ 1980:2022 across countries")
    )

# new s and c plots
`s_theta*_plot` <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`g(K)`) > 0.01) %>%
    ggplot(aes(x = year, y = `θ*s`, group = country)) %>%
    make_plot() +
    labs(
        y = TeX("$\\theta^*_s$"),
        x = "Year",
        title = TeX("$\\theta^*_s$ 1980:2022 across countries")
    )

`c_theta*_plot` <- scg_data %>%
    filter(year %in% 1980:2022) %>%
    filter(abs(`g(K)`) > 0.01) %>%
    ggplot(aes(x = year, y = `θ*c`, group = country)) %>%
    make_plot() +
    labs(
        y = TeX("$\\theta^*_c$"), x = "Year",
        title = TeX("$\\theta^*_c$ 1980:2022 across countries")
    ) +
    coord_cartesian(ylim = NULL) +
    scale_y_continuous(breaks = seq(-50, 50, 10))


square_save <- function(filename, plot) {
    ggsave(
        filename = filename,
        plot = plot,
        path = "./figure-pdf/",
        device = "pdf",
        width = 5, height = 5,
        units = "in",
        dpi = 300
    )
}


square_save(filename = "fig-si_plots-1.pdf", plot = si_theta_plot)
square_save(filename = "fig-si_plots-2.pdf", plot = si_phi_plot)

square_save(filename = "fig-c_plots-1.pdf", plot = c_theta_plot)
square_save(filename = "fig-c_plots-2.pdf", plot = c_phi_plot)

square_save(filename = "fig-s_c_theta_plots-1.pdf", plot = `s_theta*_plot`)
square_save(filename = "fig-s_c_theta_plots-2.pdf", plot = `c_theta*_plot`)
