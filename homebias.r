pacman::p_load(dplyr, tidyr, ggplot2, patchwork)

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

##Calculate Home Bias



####
##Mexican Foreign Equity Holdings
####

##All data are in millions of current US dollars. All stock variables are measured as of Dec 31 and hence converted in USD at the end-of-period exchange rate. Flow variables (including GDP) are converted in USD using the period-average exchange rate.

mx_foreign_eq <- readxl::read_xlsx(here::here("ewn_2025.xlsx"), sheet = "Dataset") %>%
        filter(Year >= 1992, stringr::str_detect(Country, "Mexico")) %>%
        select(Year, `Portfolio equity assets`, `Portfolio equity liabilities`) %>%
        rename(year = Year, mx_foreign_eq = `Portfolio equity assets`, mx_foreign_liab = `Portfolio equity liabilities`)


View(mx_foreign_eq)


####
##Total Mexican Equity Holdings
####


##from Dalquist et al 2003, use market cap plus foreign equity holdings. 
##"Market capitalization of listed domestic companies (current US$)" "Market capitalization of listed domestic companies (current US$)" "Market capitalization of listed domestic companies (current US$)" "Market capitalization of listed domestic companies (current US$)"

mx_marketcap <- WDI::WDI(indicator = "CM.MKT.LCAP.CD", country = "MX", start = 1992, end = 2023) %>%
        rename(mx_mcap = CM.MKT.LCAP.CD) %>%
        select(year, mx_mcap) 


##World Market Portfolio Weights

wld_marketcap <- WDI::WDI(indicator = "CM.MKT.LCAP.CD", country = "all", start = 1992, end = 2023) %>%
        rename(wld_mcap = CM.MKT.LCAP.CD) %>%
        group_by(year) %>%
        summarize(wld_mcap  = sum(wld_mcap, na.rm = TRUE)) ##%>%
        ##mutate(growth = (wld_mcap - wld_mcap[match(year - 1, year)]) / wld_mcap[match(year - 1, year)] * 100)

mx_homebias_df <- left_join(mx_marketcap, left_join(wld_marketcap, mx_foreign_eq, by = "year"), by = "year") %>%
                mutate(hb = 1 - ((mx_foreign_eq * 1000000) / ((mx_foreign_eq * 1000000) + mx_mcap - mx_foreign_liab)) / ((wld_mcap - mx_mcap) / wld_mcap)) %>%
                select(year, hb) %>%
                tidyr::pivot_longer(cols = hb, names_to = "type", values_to = "value")



####
## OECD Countries
####


wld_marketcap_full <- WDI::WDI(indicator = "CM.MKT.LCAP.CD", country = "all", start = 1992, end = 2023) %>%
        rename(wld_mcap = CM.MKT.LCAP.CD)

oecd_reflist <- tibble::tibble(
  ewn_country = c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
    "Costa Rica", "Czech Republic", "Denmark", "Estonia", "Finland",
    "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
    "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania",
    "Luxembourg", "Netherlands", "New Zealand", "Norway",
    "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain",
    "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ),
  iso2c = c(
    "AU", "AT", "BE", "CA", "CL", "CO", "CR", "CZ", "DK", "EE", "FI",
    "FR", "DE", "GR", "HU", "IS", "IE", "IL", "IT", "JP", "KR", "LV",
    "LT", "LU", "NL", "NZ", "NO", "PL", "PT", "SK", "SI", "ES",
    "SE", "CH", "TR", "GB", "US"
  )
)
oecd_list <- list()


for (i in 1:nrow(oecd_reflist)){
        iso2c_i <- oecd_reflist$iso2c[i]
        countryname <- oecd_reflist$ewn_country[i]

        home_foreign_eq <- readxl::read_xlsx(here::here("ewn_2025.xlsx"), sheet = "Dataset") %>%
                filter(Year >= 1992, Country == countryname) %>%
                select(Year, `Portfolio equity assets`, `Portfolio equity liabilities`) %>%
                rename(year = Year, home_foreign_eq = `Portfolio equity assets`, foreign_liab = `Portfolio equity liabilities`)

        ####
        ##Total Home Equity Holdings
        ####

        home_marketcap <- wld_marketcap_full %>%
                    filter(iso2c == iso2c_i) %>%
                    rename(home_mcap = wld_mcap)

        ####
        ##World Market Portfolio Weights
        ####

        temp_homebias_df <- left_join(home_marketcap, left_join(wld_marketcap, home_foreign_eq, by = "year"), by = "year") %>%
                        mutate(hb = 1 - ((home_foreign_eq * 1000000) / ((home_foreign_eq * 1000000) + home_mcap - foreign_liab)) / ((wld_mcap - home_mcap) / wld_mcap), countryname = countryname) 

        
        oecd_list[[countryname]] <- temp_homebias_df

}

oecd_df <- bind_rows(oecd_list, .id = "countryname") %>%
    group_by(year) %>%
    summarize(oecd_hb = mean(hb, na.rm = TRUE)) %>%
    tidyr::pivot_longer(cols = oecd_hb, names_to = "type", values_to = "value")


View(oecd_df)

####
## LatAm Countries
####

latam_reflist <- tibble::tibble(
  ewn_country = c(
    "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
    "Ecuador", "El Salvador", "Guatemala", "Honduras",
    "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela, Rep. Bol."
  ),
  iso2c = c(
    "AR", "BO", "BR", "CL", "CO", "CR", "EC", "SV", "GT", "HN", 
    "NI", "PA", "PY", "PE", "UY", "VE"
  )
)

latam_list <- list()


for (i in 1:nrow(latam_reflist)){
        iso2c_i <- latam_reflist$iso2c[i]
        countryname <- latam_reflist$ewn_country[i]

        home_foreign_eq <- readxl::read_xlsx(here::here("ewn_2025.xlsx"), sheet = "Dataset") %>%
                filter(Year >= 1992, Country == countryname) %>%
                select(Year, `Portfolio equity assets`, `Portfolio equity liabilities`) %>%
                rename(year = Year, home_foreign_eq = `Portfolio equity assets`, foreign_liab = `Portfolio equity liabilities`)

        ####
        ##Total Home Equity Holdings
        ####

        
        home_marketcap <- wld_marketcap_full %>%
                    filter(iso2c == iso2c_i) %>%
                    rename(home_mcap = wld_mcap)

        ####
        ##World Market Portfolio Weights
        ####

        temp_homebias_df <- left_join(home_marketcap, left_join(wld_marketcap, home_foreign_eq, by = "year"), by = "year") %>%
                        mutate(hb = 1 - ((home_foreign_eq * 1000000) / ((home_foreign_eq * 1000000) + home_mcap - foreign_liab)) / ((wld_mcap - home_mcap) / wld_mcap), countryname = countryname) 

        
        latam_list[[countryname]] <- temp_homebias_df

}


latam_df <- bind_rows(latam_list, .id = "countryname") %>%
    group_by(year) %>%
    summarize(latam_hb = mean(hb, na.rm = TRUE)) %>%
    tidyr::pivot_longer(cols = latam_hb, names_to = "type", values_to = "value")


####
### Bind Series
####

full_df <- tibble(year = numeric(),
                type = character(), 
                value = numeric())

full_df <- bind_rows(full_df, (bind_rows(mx_homebias_df, bind_rows(latam_df, oecd_df))))


####
## Plot
####


homebias_plot <- ggplot(data = full_df, aes(x = year, y = value, colour = type)) +
            geom_line(linewidth = 1.5) +
            scale_colour_manual(values = c("latam_hb" = "#0e3d2e", "oecd_hb" = "#006600", "hb" = "#5dcaa5"), labels = c("hb" = "Mexico", "oecd_hb" = "OECD excl Mexico", "latam_hb" = "Latin America excl Mexico")) +
            scale_x_continuous(breaks = seq(from = 1996, to = 2022, by = 4), expand = c(0, 0)) + 
            labs(title = "Equity Home Bias, 1992-2022", 
            caption = "Source: EWN Database, World Bank\nNote: Equity Home Bias equals Equity home bias is measured as one minus the ratio of a country's actual foreign equity allocation to the share foreign equities represent in the world market portfolio.)", 
            y = "Home Bias",
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

homebias_plot
ggsave(file = here::here("homebias.png"), plot = homebias_plot, width = 10, height = 6, dpi = 300)

