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
    plot.title         = element_text(color = "#0e3d2e", size = 14,
                                      family = "Georgia", face = "bold",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "#5F5E5A", size = 9,
                                      family = "Georgia", 
                                      margin = margin(b = 12)),
    plot.caption       = element_text(color = "#888780", size = 7.5,
                                      family = "Georgia", hjust = 0,
                                      margin = margin(t = 10)),

    # legend
    legend.position    = "right",
    legend.justification = "center",
    legend.direction = "vertical",
    legend.text        = element_text(color = "#2C2C2A", size = 8.5,
                                      family = "Georgia"),
    legend.title       = element_blank(),
    legend.key.width   = unit(1.8, "cm"),
    legend.key.height  = unit(0.35, "cm"),
    legend.background  = element_rect(fill = "#F7F6F2", color = NA),
    legend.margin      = margin(0, 0, 4, 0),

    # margins
    plot.margin        = margin(16, 20, 12, 14)
  )
}



########################################################################################################################################################################

################################################################################ Openness ############################################################################## 

########################################################################################################################################################################

data <- openxlsx::read.xlsx(here::here("ewn_2025.xlsx"), "Dataset")


assetcols <- c(colnames(data)[stringr::str_detect(colnames(data), "assets")], "FX.Reserves.minus.gold")
liabilitycols <- c(colnames(data)[stringr::str_detect(colnames(data), "liab")])

financial_df <- data %>%
    rename(gdp = 'GDP.(US$)') %>%
    filter(Year >= 1990) %>%
    group_by(Year) %>%
    mutate(total_assets = rowSums(across(all_of(assetcols))),
            total_liabilities = rowSums(across(all_of(liabilitycols))),
            financial_integration = (total_assets + total_liabilities) / gdp * 100,
            net_position = (total_assets - total_liabilities) / gdp * 100) %>%
    ungroup()

####
##Compare Mexico to RoW & LatAm
####

row <- unique(financial_df$Country)
row <- row[-which(row == "Mexico")]

latam <- row[row %in% c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
  "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
  "Uruguay", "Venezuela, Rep. Bol.",
  "Belize", "Costa Rica", "El Salvador", "Guatemala",
  "Honduras", "Nicaragua", "Panama",
  "Antigua and Barbuda", "Bahamas, The", "Barbados",
  "Dominica", "Dominican Republic", "Grenada", "Haiti",
  "Jamaica", "St. Kitts and Nevis", "St. Lucia",
  "St. Vincent & Grens.", "Trinidad and Tobago")]

latam_ex_carib <- latam[-which(latam %in% c("Antigua and Barbuda", "Bahamas, The", "Barbados",
  "Dominica", "Dominican Republic", "Grenada", "Haiti",
  "Jamaica", "St. Kitts and Nevis", "St. Lucia",
  "St. Vincent & Grens.", "Trinidad and Tobago"))]

mexico <- "Mexico"

grouplist <- list(row = row, latam = latam_ex_carib, mexico = mexico)

defacto_plot <- tibble(Year = numeric(),
                        type = character(), 
                        value = numeric())

for (i in 1:length(grouplist)){
    temp_group <- grouplist[[i]]
    temp_name <- names(grouplist[i])
    temp_df <- financial_df %>%
        filter(Country %in% temp_group) %>%
        group_by(Year) %>%
        summarise(value = mean(financial_integration, na.rm = TRUE) ) %>%
        mutate(type = temp_name) %>%
        select(Year, type, value)
    
    defacto_plot <- bind_rows(defacto_plot, temp_df)
}

defacto_intplot <- ggplot(data = defacto_plot %>% filter(Year >= 1996 & Year <= 2022), aes(x = Year, y = value, colour = type)) +
            geom_line(linewidth = 1.5) +
            scale_colour_manual(values = c("latam" = "#0e3d2e", "row" = "#006600", "mexico" = "#5dcaa5"), labels = c("mexico" = "Mexico", "row" = "Rest of World excl Mexico", "latam" = "Latin America excl Mexico")) +
            scale_x_continuous(breaks = seq(from = 1996, to = 2022, by = 4), expand = c(0, 0)) + 
            labs(title = "Financial Integration, 1996-2022", 
            caption = "Source: EWN Database\nNote: Financial Integration is the sum of total liabilities and total assets as a percentage of GDP", 
            y = "Percentage",
            x = "Year") +
            theme_erasmus()
            # theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
            #         plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
            #         plot.title.position = "plot",
            #         plot.caption.position = "plot",
            #         plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
            #         axis.title.y = element_text(margin = margin(r = 8)),
            #         panel.grid = element_blank(),
            #         legend.position = "right",
            #         legend.title = element_text(size = 9, face = "bold"),
            #         legend.spacing.y = unit(1, 'cm'),
            #         plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
            #         axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            #         axis.title.x = element_blank(),
            #         axis.line   = element_line(color = "black"),
            #         axis.ticks = element_line(color = "black")) +

defacto_intplot
ggsave(file = here::here("integration.png"), plot = defacto_intplot, width = 10, height = 6, dpi = 300)

