pacman::p_load(dplyr, tidyr, ggplot2, httr, jsonlite, ggtext)
TE_API <- Sys.getenv("TRADINGECONOMICS")

get_te <- function(endpoint, params = list()) {
  params[["c"]] <- TE_API
  url <- paste0("https://api.tradingeconomics.com/", endpoint)
  response <- GET(url, query = params)
  fromJSON(content(response, "text", encoding = "UTF-8"))
}

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
    legend_direction = "vertical",
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

#################################################################################################################################################################

######################################################################## Default Risk ######################################################################## 

#################################################################################################################################################################

sp_scale <- c(
  "AAA"  = 20, "AA+" = 19, "AA"  = 18, "AA-" = 17,
  "A+"   = 16, "A"   = 15, "A-"  = 14,
  "BBB+" = 13, "BBB" = 12, "BBB-"= 11,
  "BB+"  = 10, "BB"  =  9, "BB-" =  8,
  "B+"   =  7, "B"   =  6, "B-"  =  5,
  "CCC+" =  4, "CCC" =  3, "CCC-"=  2,
  "CC"   =  1, "D"   =  0
)

moodys_scale <- c(
  "Aaa" = 20,
  "Aa1" = 19, "Aa2" = 18, "Aa3" = 17,
  "A1"  = 16, "A2"  = 15, "A3"  = 14,
  "Baa1"= 13, "Baa2"= 12, "Baa3"= 11,
  "Ba1" = 10, "Ba2" =  9, "Ba3" =  8,
  "B1"  =  7, "B2"  =  6, "B3"  =  5,
  "Caa1"=  4, "Caa2"=  3, "Caa3"=  2,
  "Ca"  =  1, "C"   =  0
)

dual_labels <- setNames(paste(names(sp_scale), "/", names(moodys_scale)), sp_scale)

print(dual_labels)

ratings_dta <- get_te("credit-ratings/historical", params = list(d1 = "1990-01-01", d2 = "2024-01-01"))

mx_ratings <- ratings_dta %>%
            filter(Country == "Mexico", Agency != "DBRS") %>% 
            mutate(Date = as.Date(Date, "%m/%d/%Y"), 
                    Year = as.integer(format(Date, "%Y")), 
                    Rating = na_if(Rating, "NA")) %>%
            group_by(Agency, Year) %>%
            slice_max(Date, n = 1) %>% ##within each year, it looks for the maximum date and takes only that value
            ungroup() %>%
            complete(Agency, Year = min(Year):max(Year)) %>%
            group_by(Agency) %>%
            arrange(Year) %>%
            fill(Rating, .direction = "down")  %>% ##ratings are filled by the prior NA values
            select(Year, Agency, Rating) %>%
            filter(Year >= 1992) %>%
            mutate(n_Rating = ifelse(Agency == "Moody's", moodys_scale[Rating], sp_scale[Rating]))


ratingsplot <- ggplot(mx_ratings, aes(x = Year, y = n_Rating, colour = Agency)) +
        geom_line(linewidth = 1.5) +
        scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 2), expand = c(0, 0)) + 
        scale_colour_manual(values = c("Moody's" = "#0e3d2e", "S&P" = "#5dcaa5")) +
        geom_vline(xintercept = 1994, color = "black", alpha = 0.12,
             linewidth = 1, linetype = "dashed") +
        annotate("text", x = 1994.3, y = 8.2, label = "Tequila crisis",
           size = 2.4, color = "#7dbfa0", hjust = 0, angle = 90) +
        geom_vline(xintercept = 2008, color = "black", alpha = 0.12,
             linewidth = 1, linetype = "dashed") +
        annotate("text", x = 2008.3, y = 8.2, label = "Global Financial Crisis",
           size = 2.4, color = "#7dbfa0", hjust = 0, angle = 90) +
        scale_y_continuous(breaks = as.numeric(names(dual_labels)), labels = dual_labels) +
        labs(title = "Mexican Sovereign Credit Ratings", 
            caption = "Source: S&P, Moody's, Trading Economics\nNote: Ratings reflect the Mexican federal government's ability to service foreign-denomindated bonds", 
            y = "Rating (S&P / Moody's)") +
        theme_erasmus() 


ggsave(file = here::here("credit_ratings.png"), plot = ratingsplot, width = 10, height = 6, dpi = 300)


####
## Public Debt Levels
####

mx_public_debt <- wbids::ids_get(entities = "MEX",
                                series = "DT.DOD.DPPG.CD",
                                counterparts = "WLD",
                                start_year = 1990,
                                end_year   = 2023) %>%
                                mutate(debt = value) %>%
                                select(year, debt)

mx_gni <- WDI::WDI(country   = "MX",
                    indicator = c(gni = "NY.GNP.MKTP.CD"),
                    start     = 1990,
                    end       = 2023) %>%
                    select(year, gni)

mx_public_debt <- left_join(mx_public_debt, mx_gni, by = "year") %>%
                    mutate(ratio = debt / gni * 100)

mx_ds <- wbids::ids_get(entities = "MEX",
                series       = "DT.TDS.DPPG.CD", ##Debt service spending
                counterparts = "WLD",
                start_year   = 1990,
                end_year     = 2023) %>%
                mutate(ds = value) %>%
                select(year, ds)

mx_exports <- WDI::WDI(country   = "MX",
                        indicator =  "BX.GSR.GNFS.CD",  # imports of goods & services, USD
                        start = 1990,
                        end = 2023) %>%
                        mutate(exports = BX.GSR.GNFS.CD) %>%
                        select(year, exports)


mx_imports <-  WDI::WDI(country   = "MX",
                        indicator =  "BM.GSR.GNFS.CD",  # exports of goods & services, USD
                        start = 1990,
                        end = 2023) %>%
                        mutate(imports = BM.GSR.GNFS.CD) %>%
                        select(year, imports)

mx_reserves <- WDI::WDI(country   = "MX",
                        indicator = "FI.RES.TOTL.CD",  # total reserves incl. gold, USD
                        start     = 1990,
                        end       = 2023) %>%
                        mutate(reserves = FI.RES.TOTL.CD) %>%
                        select(year, reserves)

##Calculate debt serice ratio as DS / Exports + Foreign Reserves
##CD means Current USD - all values are denominated

mx_ds_ratio <- left_join(mx_exports, left_join(mx_ds, left_join(mx_reserves, mx_imports, by = "year"), by = "year"), by = "year") %>%
  mutate(ds_ratio = ds / (exports  + reserves) * 100)

##we can add exports but i think reserves might be better
debtserviceplot <- ggplot(mx_ds_ratio, aes(x = year, y = ds_ratio, colour = "Mexico")) +
        geom_line(linewidth = 1.5, show.legend = FALSE) +
        scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 2), expand = c(0, 0)) + 
        scale_colour_manual(values = c("Mexico" = "#0e3d2e")) +
        geom_vline(xintercept = 1994, color = "black", alpha = 0.12,
             linewidth = 1, linetype = "dashed") +
        annotate("text", x = 1994.3, y = 8.2, label = "Tequila Crisis",
           size = 2.4, color = "#7dbfa0", hjust = 0, angle = 90) +
        geom_vline(xintercept = 2008, color = "black", alpha = 0.12,
             linewidth = 1, linetype = "dashed") +
        annotate("text", x = 2008.3, y = 8.2, label = "Global Financial Crisis",
           size = 2.4, color = "#7dbfa0", hjust = 0, angle = 90) +
        scale_y_continuous(limits = c(0, 40)) +
        labs(title = "Mexican Debt Serivce Raio", 
            caption = "Source: International Monetary Fund, World Bank\nNote: Debt Service Ratio equals debt service costs over merchandise exports plus reserves", 
            y = "Ratio") +
        theme_erasmus()

debtserviceplot
ggsave(file = here::here("debtservice.png"), plot = debtserviceplot, width = 10, height = 6, dpi = 300)

