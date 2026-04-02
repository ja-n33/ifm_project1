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
    axis.text          = element_text(color = "#5F5E5A", size = 10,
                                      family = "Georgia", face = "bold"),
    axis.title         = element_text(color = "#2C2C2A", size = 12,
                                      family = "Georgia", face = "bold.italic"),

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
    legend.position    = "right",
    legend.justification = "center",
    legend.direction = "vertical",
    legend.text        = element_text(color = "#2C2C2A", size = 12,
                                      family = "Georgia", face = "bold"),
    legend.title       = element_blank(),
    legend.key.width   = unit(0.5, "cm"),
    legend.key.height  = unit(0.35, "cm"),
        legend.key.spacing.y = unit(0.6,"cm"),
    legend.background  = element_rect(fill = "#F7F6F2", color = NA),
    legend.margin      = margin(0, 0, 4, 0),

    # margins
    plot.margin        = margin(16, 20, 12, 14)
  )
}

########################################################################################################################################################################

############################################################################## Correlations ############################################################################ 

########################################################################################################################################################################

##Use PWT available at https://www.rug.nl/ggdc/productivity/pwt/?lang=en
pwt <- readxl::read_xlsx(here::here('pwt110.xlsx'), sheet = "Data")

wld_growth_pwt <- pwt %>%
    rename(iso3c = countrycode) %>%
    select(iso3c, year, rgdpe) %>%
    filter(iso3c != "MEX") %>%
    filter(year >= 1950) %>%
    group_by(year) %>%
    mutate(wgt = rgdpe / sum(rgdpe, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(iso3c) %>%
    mutate(lag_rgdpe = rgdpe[match(year - 1, year)], growth_pwt = (rgdpe - lag_rgdpe) / lag_rgdpe) %>%
    ungroup() %>%
    group_by(year) %>%
    summarise(wld_growth_pwt = weighted.mean(growth_pwt, wgt, na.rm = TRUE) * 100) %>%
    mutate(iso3c = "WLD")
#Expenditure-side real GDP at chained PPPs (in mil. 2021US$)

mex_growth_pwt <- pwt %>%
    rename(iso3c = countrycode) %>%
    filter(iso3c == "MEX") %>%
    select(iso3c, year, rgdpe) %>%
    filter(year >= 1950) %>%
    mutate(lag_rgdpe = rgdpe[match(year - 1, year)], growth_pwt = (rgdpe - lag_rgdpe) / lag_rgdpe) 
#Expenditure-side real GDP at chained PPPs (in mil. 2021US$)

mex_cons_pwt <- pwt %>%
    rename(iso3c = countrycode) %>%
    filter(iso3c == "MEX") %>%
    select(iso3c, year, ccon) %>%
    filter(year >= 1950) %>%
    mutate(lag_ccon = ccon[match(year - 1, year)], cons_pwt = (ccon - lag_ccon) / lag_ccon) 
#Real consumption of households and government, at current PPPs (in mil. 2021US$)


##FRED Mexico Private Consumption Growth
# mex_cons_fred <- fredr(
#   series_id = "NCPRSAXDCMXQ",
#   observation_start = as.Date("1993-01-01"), 
#   observation_end = as.Date("2019-09-30"),   
#   frequency = "a",                     
#   units = "pc1") %>%
#   mutate(year = as.integer(stringr::str_sub(date, 1, 4)), cons_fred = value, iso3c = "MEX") %>%
#   select(iso3c, year, cons_fred)

seriesnames <- c("mex_growth_pwt", "mex_cons_pwt", "wld_growth_pwt")
seriesvals <- c("growth_pwt", "cons_pwt", "wld_growth_pwt")
seriesdf <- tibble(series = seriesnames, 
                    values = seriesvals)

years_ranked <- list() ##create a list within a list
values_stored<- list()

for (i in 1:nrow(seriesdf)){
    series <- seriesdf[[i, "series"]]
    value <- seriesdf[[i, "values"]]
    years_ranked[[series]] <- list() ##create a list within a list
    values_stored[[series]] <- list()

    for (t in 1970:2022){
        temp_vec <- get(series) %>%
            mutate(year = as.numeric(year), across(all_of(value), as.numeric)) %>%
            filter(year <= t, year > (t - 15)) %>%
            arrange(.data[[value]]) 
        years_ranked[[series]][[as.character(t)]]  <- temp_vec %>% pull(as.numeric(year))

        temp_vec <- temp_vec %>%
            arrange(.data[["year"]]) 
        values_stored[[series]][[as.character(t)]] <- temp_vec %>% pull(.data[[value]])    
    }
}

cons_pwt_df <- tibble(year = numeric(), 
                    mexc_mexy_sp = numeric(),
                    mexc_wldy_sp = numeric(), 
                    mexc_mexy_pe = numeric(),
                    mexc_wldy_pe = numeric(), 
                    )

for (t in 1992:2022){
    mexc_mexy_sp_temp <- cor(years_ranked[["mex_cons_pwt"]][[as.character(t)]], years_ranked[["mex_growth_pwt"]][[as.character(t)]], method = "spearman")
    mexc_wldy_sp_temp <- cor(years_ranked[["mex_cons_pwt"]][[as.character(t)]], years_ranked[["wld_growth_pwt"]][[as.character(t)]], method = "spearman")

    mexc_mexy_pe_temp <- cor(values_stored[["mex_cons_pwt"]][[as.character(t)]], values_stored[["mex_growth_pwt"]][[as.character(t)]], method = "pearson")
    mexc_wldy_pe_temp <- cor(values_stored[["mex_cons_pwt"]][[as.character(t)]], values_stored[["wld_growth_pwt"]][[as.character(t)]], method = "pearson")
    temp_df <- tibble(year = t, 
                    mexc_mexy_sp = mexc_mexy_sp_temp,
                    mexc_wldy_sp =  mexc_wldy_sp_temp,
                    mexc_mexy_pe = mexc_mexy_pe_temp,
                    mexc_wldy_pe = mexc_wldy_pe_temp)
    cons_pwt_df <- bind_rows(cons_pwt_df, temp_df)}

cons_pwt_df <- cons_pwt_df %>%
        tidyr::pivot_longer(cols = -year, names_to = "type", values_to = "value")

pwtplot <- ggplot(data = cons_pwt_df[cons_pwt_df$type %in% c("mexc_mexy_pe", "mexc_wldy_pe" ), ], aes(x = year, y = value, colour = type)) +
    geom_line(linewidth = 2) +
    scale_colour_manual(values = c("mexc_mexy_pe" = "#0e3d2e", "mexc_wldy_pe" = "#5dcaa5"), labels = c("mexc_mexy_pe" = "Correlation with\nDomestic Output", "mexc_wldy_pe" = "Correlation with\nWorld Output")) +
    scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 4), expand = c(0, 0)) + 
    geom_hline(yintercept = 0, colour = "dimgrey", linewidth = 0.75) +
    geom_vline(xintercept = 1994, color = "black", alpha = 0.2,
             linewidth = 0.75, linetype = "dashed") +
    geom_vline(xintercept = 2008, color = "black", alpha = 0.2,
             linewidth = 0.75, linetype = "dashed") +
    labs(title = "Mexican Consumption Pearson Correlations, 1992-2002", 
      caption = "Source: Penn World Table\nNote: Consumption includes Private and Public Consumption Spending.\n            First vertical line represents Tequila Crisis, second represents GFC.", 
      y = "Correlation", 
      x = "Year") +
    theme_erasmus()


ggsave(file = here::here("pearsonplot_tidy.png"), plot = pwtplot, width = 10, height = 6, dpi = 300)
