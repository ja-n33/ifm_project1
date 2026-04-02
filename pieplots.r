pacman::p_load(dplyr, tidyr, ggplot2, httr, jsonlite, ggtext)


theme_erasmus <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
  theme(
    # backgrounds
    plot.background    = element_rect(fill = "#F7F6F2", color = NA),
    panel.background   = element_rect(fill = "#F7F6F2", color = NA),

    # grid
    panel.grid.major.y = element_line(color = "#E2E0DA", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),

    # axis
    axis.line.x        = element_line(color = "#BFBDB5", linewidth = 0.5),
    axis.ticks.x       = element_line(color = "#BFBDB5", linewidth = 0.4),
    axis.ticks.y       = element_blank(),
    axis.text          = element_text(color = "#5F5E5A", size = 8.5,
                                      family = "Georgia"),
    axis.title         = element_text(color = "#2C2C2A", size = 9,
                                      family = "Georgia", face = "italic"),

    # titles
    plot.title         = element_text(color = "#0e3d2e", size = 16,
                                      family = "Georgia", face = "bold",
                                      margin = margin(b = 4)),
    plot.subtitle       = element_text(color = "dimgrey", size = 12, 
                                      family = "Georgia", hjust = 0, 
                                      margin = margin(t = 10), face = "bold"), 
    plot.caption       = element_text(color = "dimgrey", size = 12, 
                                      family = "Georgia", hjust = 0,
                                       margin = margin(t = 10), face = "bold"), 

    # legend
    legend.position    = "left",
    legend.justification = "left",
    legend.direction = "vertical",
    legend.text        = element_text(color = "#2C2C2A", size = 16,
                                      family = "Georgia", face = "bold"),
    legend.title       = element_blank(),
    legend.key.width   = unit(0.2, "cm"),
    legend.key.height  = unit(0.2, "cm"),
    legend.key.spacing.y = unit(0.6,"cm"),
    legend.background  = element_rect(fill = "transparent", color = NA),
    legend.margin      = margin(0, 0, 4, 0),

    # margins
    plot.margin        = margin(16, 20, 12, 14)
  )
}

########################################################################################################################################################################

################################################################################ Pie Charts ############################################################################ 

########################################################################################################################################################################

assets_dta <- readr::read_csv(here::here("mx_int_assets.csv"))

assets <- assets_dta %>%
    select(COUNTERPART_COUNTRY, `2024`) %>%
    filter(!(COUNTERPART_COUNTRY %in% c("International Organizations", "Not Specified (including Confidential)", "World", "World Minus 25 Significant Financial Centers"))) 

top7 <- assets %>%
        arrange(desc(`2024`)) %>%
        head(7) %>%
        pull(COUNTERPART_COUNTRY)

assets_reduced <- assets %>%
        mutate(COUNTERPART_COUNTRY = ifelse(COUNTERPART_COUNTRY %in% top7, COUNTERPART_COUNTRY, "Other"), val = `2024` / sum(`2024`, na.rm = TRUE) * 100) %>%
        group_by(COUNTERPART_COUNTRY) %>%
        summarize(val = sum(val, na.rm = TRUE)) %>%
        mutate(COUNTERPART_COUNTRY = forcats::fct_reorder(COUNTERPART_COUNTRY, val, .desc = TRUE),
         COUNTERPART_COUNTRY = forcats::fct_relevel(COUNTERPART_COUNTRY, "Other", after = Inf))

assets_pie <- ggplot(assets_reduced, aes(x = "", y = val, fill = COUNTERPART_COUNTRY)) +
  geom_col(width = 1) +
  scale_fill_manual(values = c("United States" = "#0e3d2e", "Ireland" = "#5dcaa5", "Luxembourg" = "#336633", "Other" = "dimgrey", "Brazil" = "#33CC33", "Germany" = "#009900", "United Kingdom" = "#669966", "Spain" = "#99CC99")) +
  coord_polar(theta = "y") +
  geom_text(aes(label = ifelse(val > 5, paste0(round(val), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 5,
            family = "Georgia", 
            fontface = "bold") +
  labs(title = "Mexican Foreign Asset Composition by Country",
        fill = "Country",
        caption = "Source: IMF") +
        guides(fill = guide_legend(nrow = 8, byrow = FALSE)) +
    theme_erasmus() + 
theme(axis.text = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background    = element_rect(fill = "transparent", color = NA),
        panel.background   = element_rect(fill = "transparent", color = NA)) 

ggsave(file = here::here("assetspie.png"), plot = assets_pie, width = 10, height = 6, dpi = 300, bg = "transparent")


####
## Liabilities
####

liabilities_dta <- readr::read_csv(here::here("mx_int_liabilities.csv"))

liabilities <- liabilities_dta %>%
    select(COUNTERPART_COUNTRY, `2024`) %>%
    filter(!(COUNTERPART_COUNTRY %in% c("International Organizations", "Not Specified (including Confidential)", "World", "World Minus 25 Significant Financial Centers", "SEFER + SSIO"))) 

top7 <- liabilities %>%
        arrange(desc(`2024`)) %>%
        head(7) %>%
        pull(COUNTERPART_COUNTRY) 

liabilities_reduced <- liabilities %>%
        mutate(COUNTERPART_COUNTRY = ifelse(COUNTERPART_COUNTRY %in% top7, COUNTERPART_COUNTRY, "Other"), val = `2024` / sum(`2024`, na.rm = TRUE) * 100) %>%
        group_by(COUNTERPART_COUNTRY) %>%
        summarize(val = sum(val, na.rm = TRUE)) %>%
        mutate(COUNTERPART_COUNTRY = ifelse(COUNTERPART_COUNTRY == "Netherlands", "Netherlands", COUNTERPART_COUNTRY), COUNTERPART_COUNTRY = forcats::fct_reorder(COUNTERPART_COUNTRY, val, .desc = TRUE),
         COUNTERPART_COUNTRY = forcats::fct_relevel(COUNTERPART_COUNTRY, "Other", after = Inf)) 


liabilities_pie <- ggplot(liabilities_reduced, aes(x = "", y = val, fill = COUNTERPART_COUNTRY)) +
    geom_col(width = 1) +
    scale_fill_manual(values = c("United States" = "#0e3d2e", "Luxembourg" = "#5dcaa5", "Cayman Islands" = "#336633", "Other" = "dimgrey", "United Kingdom" = "#33CC33", "Ireland" = "#009900", "Netherlands" = "#669966", "Japan" = "#99CC99")) +
    coord_polar(theta = "y") +
    geom_text(aes(label = ifelse(val > 5, paste0(round(val), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 3.5,
            family = "Georgia", 
            fontface = "bold") +
    labs(title = "Mexican Foreign Liabilities Composition by Country",
        fill = "Country", 
        caption = "Source: IMF") +
                guides(fill = guide_legend(nrow = 8, byrow = FALSE)) +
    theme_erasmus() + 
    theme(axis.text = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background    = element_rect(fill = "transparent", color = NA),
        panel.background   = element_rect(fill = "transparent", color = NA)) 

ggsave(file = here::here("liabilitiespie.png"), plot = liabilities_pie, width = 10, height = 6, dpi = 300, bg = "transparent")

