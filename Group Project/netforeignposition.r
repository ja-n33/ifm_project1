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

######################################################################## International Position ######################################################################## 

########################################################################################################################################################################

##Show Net Financial Position as with the data we used in the second quiz

data <- openxlsx::read.xlsx(here::here("ewn_2025.xlsx"), "Dataset")

##relevant cols: Portfolio.equity.assets, portfolio.equity.liabbilities, FDI.assets, FDI.liabilities, Debt.assets, debt.liabilities
##financial.derivatives.(assets), financial.derivatives.(liabilities)


mx_df <- data %>%
  filter(Country == "Mexico") %>%
  select(Year, Portfolio.equity.assets, Portfolio.equity.liabilities, FDI.assets, FDI.liabilities, `Debt.assets.(portfolio.debt.+.other.investment)`, 
        `Debt.liabilities.(portfolio.debt.+.other.investment)`, `financial.derivatives.(assets)`, `financial.derivatives.(liabilities)`, `FX.Reserves.minus.gold`, `GDP.(US$)`, `OFFICIAL.IIP`, `Net.IIP.excl.gold`)

assetcols <- c(colnames(mx_df)[stringr::str_detect(colnames(mx_df), "asset")], "FX.Reserves.minus.gold")
liabilitycols <- colnames(mx_df)[stringr::str_detect(colnames(mx_df), "liabilities")]


mx_long <- mx_df %>%
    mutate(balance = rowSums(across(all_of(assetcols)), na.rm = TRUE) - rowSums(across(all_of(liabilitycols)), na.rm = TRUE), ratio = balance / `GDP.(US$)` * 100, z_ratio  = (ratio - mean(ratio)) / sd(ratio) * 100) %>%
    pivot_longer(!Year, names_to = "type", values_to = "value") %>%
    mutate(value = ifelse(type %in% liabilitycols, -(value), value)) %>%
    filter(Year >= 1990)

balance_plot <- ggplot(data = mx_long, aes(x = Year, y = value, fill = type)) +
    geom_bar(data = mx_long %>% filter(!(type %in% c("balance", "ratio", "z_ratio"))), stat = "identity", position = "stack") +
    geom_line(data = mx_long %>% filter(type == "balance"), linewidth = 1, aes(color = type, linetype = type)) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5,linewidth = 0.75, linetype = "dashed") +
    scale_linetype_manual(values = c("balance" = "solid"), labels = c("balance" = "Balance")) +
    scale_color_manual(values = c("balance" = "black"), labels = c("balance" = "Balance")) +
    scale_fill_manual(values = c("Portfolio.equity.assets" ="#0e3d2e", "Portfolio.equity.liabilities" = "#0e3d2e", "FDI.assets" = "#5dcaa5", "FDI.liabilities" = "#5dcaa5", "Debt.assets.(portfolio.debt.+.other.investment)" = "darkgreen", "Debt.liabilities.(portfolio.debt.+.other.investment)" = "darkgreen", "financial.derivatives.(assets)" = "dimgrey", "financial.derivatives.(liabilities)" = "dimgrey", "FX.Reserves.minus.gold" = "azure3"), 
                      breaks = c("Portfolio.equity.assets", "FDI.assets", "Debt.assets.(portfolio.debt.+.other.investment)", "financial.derivatives.(assets)", "FX.Reserves.minus.gold"), 
                      labels = c("Portfolio.equity.assets" = "Portfolio Equity", "FDI.assets" = "Foreign Direct Investment", "Debt.assets.(portfolio.debt.+.other.investment)" = "Foreign Debt", "financial.derivatives.(assets)" = "Foreign Derivatives", "FX.Reserves.minus.gold" = "Non-gold FX Reserves")) +
    scale_y_continuous(labels = \(x) format(abs(x), big.mark = ",", scientific = FALSE)) +
    scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 4), expand = c(0, 0)) + 
    annotate("text", x = 1992, y = 500000,   label = "Assets",      
         hjust = 0, vjust = 0, size = 3, color = "#5F5E5A", family = "Georgia", fontface = "italic") +
    annotate("text", x = 1992, y = -500000,  label = "Liabilities",  
         hjust = 0, vjust = 1, size = 3, color = "#5F5E5A", family = "Georgia", fontface = "italic") +
    labs(title = "Mexican Financial Account Balance, 1992-2022", 
        caption = "Source: EWN Database", 
        y = "US$", 
        x = "Year") +
    theme_minimal(base_size = 10)+
    theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
            plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
            axis.title.y = element_text(margin = margin(r = 8), face = "bold"),
            panel.grid = element_blank(),
            legend.position = "right",
            legend.title = element_blank(),
            legend.spacing.y = unit(1, 'cm'),
            plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_text(face = "bold"),
            axis.line   = element_line(color = "black"),
            axis.ticks = element_line(color = "black")) +
        theme_erasmus()

ggsave(file = here::here("financialbalance.png"), plot = balance_plot, width = 10, height = 6, dpi = 300)

