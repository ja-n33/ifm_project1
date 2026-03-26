pacman::p_load(dplyr, pwt10, ggplot2, fredr)

country_codes <- c(
    "AUS", "AUT", "CAN", "FIN",
    "FRA", "DEU", "IRL", "ITA", "JPN",
    "MEX", "NLD", "NZL", "SWE",
    "CHE", "GBR", "USA",
    "IND", "ZAF"
)

wld_codes <- c(
    "AUS", "AUT", "CAN", "FIN",
    "FRA", "DEU", "IRL", "ITA", "JPN",
    "NLD", "NZL", "SWE", "IND", 
    "CHE", "GBR", "USA", "ZAF"
)

country_match <- c(
    "Australia" = "AUS", "Austria" = "AUT", "Canada" = "CAN", "Finland" = "FIN",
    "France" = "FRA", "Germany" = "DEU", "Ireland" = "IRL", "Italy" = "ITA", "Japan" = "JPN",
    "Mexico" = "MEX", "Netherlands" = "NLD", "New.Zealand" = "NZL", "Sweden" = "SWE",
    "Switzerland" = "CHE", "United.Kingdom" = "GBR", "United.States" = "USA",
    "India" = "IND", "South.Africa" = "ZAF"
)

gdp <- pwt10::pwt10.01 %>%
    rename(iso3c = isocode) %>%
    select(country, iso3c, year, rgdpe) %>%
    filter(year >= 1971) %>% ##we can change whether we want to weigh accoding to share in sample gdp or world gdp
    group_by(year) %>%
    mutate(wgt = rgdpe / sum(rgdpe, na.rm = TRUE)) %>%
    ungroup()

#################################################################################################################################################################

######################################################################## Synchronisation ######################################################################## 

#################################################################################################################################################################

####
## Consumption
####

##PWT Mexico Consumption Growth
mex_cons_pwt <- pwt10::pwt10.01 %>%
    rename(iso3c = isocode) %>%
    filter(iso3c == "MEX") %>%
    select(iso3c, year, ccon) %>%
    filter(year >= 1956) %>%
    mutate(lag_ccon = ccon[match(year - 1, year)], ccon_pwt_growth = (ccon - lag_ccon) / lag_ccon) 

subset <- pwt10::pwt10.01 %>%
    mutate(isocode = as.character(isocode)) %>%
    rename(iso3c = isocode) %>%
    filter(year == 1956, !is.na(ccon)) %>%
    pull(iso3c) %>%
    unique()

##PWT World Consumption Growth
wld_cons_pwt <- pwt10::pwt10.01 %>%
    rename(iso3c = isocode) %>%
    filter(iso3c  %in% country_codes) %>% ##here we use the OECD subsample again
    select(iso3c, year, ccon) %>%
    filter(year >= 1956) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(lag_ccon = ccon[match(year - 1, year)], ccon_pwt_growth = (ccon - lag_ccon) / lag_ccon) 


years_ranked <- list()

for (iso in c("MEX", country_codes)){
    years_ranked[[iso]] <- list() ##create a list within a list

    for (t in 1970:2019){
        if (iso == "MEX"){
            temp_vec <- mex_cons_pwt %>%
            filter(iso3c == iso, year <= t, year > (t - 15)) %>%
            arrange(ccon_pwt_growth) %>%
            pull(year)
        } else {
        temp_vec <- wld_cons_pwt %>%
            mutate(iso3c = as.character(iso3c)) %>%
            filter(iso3c == iso, year <= t, year > (t - 15)) %>%
            arrange(ccon_pwt_growth) %>%
            pull(year)}
        years_ranked[[iso]][[as.character(t)]] <- temp_vec
    }
}


cons_df <- tibble(year = numeric(), 
                    cons_sp_ind = numeric())

for (t in 1970:2019){
    year <- t
    year_df <- tibble(partner = character(), 
                        cons_sp_ind = numeric())

    for (iso in unique(wld_cons_pwt$iso3c)){
    cons_sp_ind <- cor(years_ranked[["MEX"]][[as.character(t)]], years_ranked[[iso]][[as.character(t)]], method = "spearman")
    temp_df <- tibble(partner = iso, 
                    cons_sp_ind = cons_sp_ind)
    year_df <- bind_rows(year_df, temp_df)}
    avg_df <- year_df %>%
                summarise(cons_sp_ind = mean(cons_sp_ind)) %>%
                mutate(year = t)
    
    cons_df <- bind_rows(cons_df, avg_df)

}


consplot <- ggplot(data = cons_df, aes(x = year, y = cons_sp_ind)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = seq(from = 1970, to = 2019, by = 5)) + 
    labs(title = "Spearman Rank Correlation of Mexican Consumption Growth vs World Consumption Growth", 
    caption = "Source: Penn World Table\nNote: Consumption includes Private and Public Consumption Spending", 
    y = "Correlation") +
    theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
            plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
            axis.title.y = element_text(margin = margin(r = 8)),
            panel.grid = element_blank(),
            legend.position = "right",
            legend.title = element_text(size = 9, face = "bold"),
            legend.spacing.y = unit(1, 'cm'),
            plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_blank(),
            axis.line   = element_line(color = "black"),
            axis.ticks = element_line(color = "black"))
consplot


ggsave(file = here::here("consplot.png"), plot = consplot, width = 10, height = 6, dpi = 300)



####
## Share Price
####

dta <- read.csv(here::here("Group Project", "sharepricedata.csv"), sep = ",", skip = 2)

share_dta <- dta %>%
    mutate(Category = as.numeric(stringr::str_sub(Category, 1, 4))) %>%
    rename(year = Category) %>%
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "country", values_to = "shareprice") %>%
    mutate(iso3c = country_match[country]) %>%
    group_by(country) %>%
    filter(!(is.na(shareprice[year == min(year)])))

share_growth <- share_dta %>%
    group_by(iso3c) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(lag_price = shareprice[match(year - 1, year)], sharegrowth = (shareprice - lag_price) / lag_price) %>%
    filter(year >= 1971) %>%
    left_join(gdp %>% select(wgt, year, iso3c), by = c("year", "iso3c"))

####    
## Not in use
# row_series <- share_growth %>%
#     filter(iso3c != "MEX") %>%
#     group_by(year) %>%
#     summarise(growth = weighted.mean(sharegrowth, wgt, na.rm = TRUE)) %>%
#     mutate(iso3c = "WLD")

# mex_series <- share_growth %>%
#     filter(iso3c == "MEX") %>%
#     select(year, growth = sharegrowth) %>%
#     mutate(iso3c = "MEX")

# share_series <- bind_rows(row_series, mex_series)
####

years_ranked <- list()

for (iso in country_codes){
    years_ranked[[iso]] <- list() ##create a list within a list

    for (t in 1985:2019){
        temp_vec <- share_growth %>%
            filter(iso3c == iso, year <= t, year > (t - 15)) %>%
            arrange(sharegrowth) %>%
            pull(year)
        years_ranked[[iso]][[as.character(t)]] <- temp_vec
    }
}


share_df <- tibble(year = numeric(), 
                    sp_ind = numeric())

for (t in 1985:2019){
    year <- t
    year_df <- tibble(partner = character(), 
                        sp_ind = numeric())

    for (iso in wld_codes){
    sp_ind <- cor(years_ranked[["MEX"]][[as.character(t)]], years_ranked[[iso]][[as.character(t)]], method = "spearman")
    temp_df <- tibble(partner = iso, 
                    sp_ind = sp_ind)
    year_df <- bind_rows(temp_df, year_df <- bind_rows(year_df, tibble(partner = iso, sp_ind = sp_ind)))}
    avg_df <- year_df %>%
                summarise(sp_ind = mean(sp_ind)) %>%
                mutate(year = t)
    
    share_df <- bind_rows(share_df, avg_df)

}


shareplot <- ggplot(data = share_df, aes(x = year, y = sp_ind)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = seq(from = 1985, to = 2019, by = 5)) + 
    labs(title = "Spearman Rank Correlation of Mexican Share Prices vs World Share Prices", 
    caption = "Source: OECD, Penn World Table", 
    y = "Correlation") +
    theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
            plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
            axis.title.y = element_text(margin = margin(r = 8)),
            panel.grid = element_blank(),
            legend.position = "right",
            legend.title = element_text(size = 9, face = "bold"),
            legend.spacing.y = unit(1, 'cm'),
            plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_blank(),
            axis.line   = element_line(color = "black"),
            axis.ticks = element_line(color = "black"))
shareplot


ggsave(file = here::here("shareplot.png"), plot = shareplot, width = 10, height = 6, dpi = 300)


#################################################################################################################################################################

########################################################################Correlation #############################################################################

#################################################################################################################################################################

##
##FRED Mexico Private Consumption Growth
mex_cons_fred <- fredr(
  series_id = "NCPRSAXDCMXQ",
  observation_start = as.Date("1993-01-01"), 
  observation_end = as.Date("2019-09-30"),   
  frequency = "a",                     
  units = "pc1") %>%
  mutate(year = as.integer(stringr::str_sub(date, 1, 4)), cons_fred = value, iso3c = "MEX") %>%
  select(iso3c, year, cons_fred)

##FRED Mexico GDP Growth            
mex_growth_fred <- fredr(
    series_id = "NGDPRSAXDCMXQ",
    observation_start = as.Date("1993-01-01"), 
    observation_end = as.Date("2019-09-30"),   
    frequency = "a",                     
    units = "pc1") %>%
    mutate(year = as.integer(stringr::str_sub(date, 1, 4)), growth_fred = value, iso3c = "MEX") %>%
    select(iso3c, year, growth_fred)

##PWT World GDP Growth            
wld_growth_pwt <- pwt10::pwt10.01 %>%
    rename(iso3c = isocode) %>%
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

str(wld_growth_pwt)
##PWT Mexico GDP Growth
mex_growth_pwt <- pwt10::pwt10.01 %>%
    rename(iso3c = isocode) %>%
    filter(iso3c == "MEX") %>%
    select(iso3c, year, rgdpe) %>%
    filter(year >= 1950) %>%
    mutate(lag_rgdpe = rgdpe[match(year - 1, year)], growth_pwt = (rgdpe - lag_rgdpe) / lag_rgdpe) 


##Plot Growth (not rank) Correlations using FRED and PWT Data


seriesnames <- c("mex_growth_fred", "mex_cons_fred", "wld_growth_pwt")
seriesvals <- c("growth_fred", "cons_fred", "wld_growth_pwt")
seriesdf <- tibble(series = seriesnames, 
                    values = seriesvals)


years_ranked <- list() ##create a list within a list
values_stored<- list()
for (i in 1:nrow(seriesdf)){
    series <- seriesdf[[i, "series"]]
    value <- seriesdf[[i, "values"]]
    years_ranked[[series]] <- list() ##create a list within a list
    values_stored[[series]] <- list()

    for (t in 2009:2019){
        temp_vec <- get(series) %>%
            mutate(year = as.numeric(year)) %>%
            filter(year <= t, year > (t - 15)) %>%
            arrange(.data[[value]]) 
        years_ranked[[series]][[as.character(t)]]  <- temp_vec %>% pull(year)

        temp_vec <- temp_vec %>%
            arrange(.data[["year"]]) 
        values_stored[[series]][[as.character(t)]] <- temp_vec %>% pull(.data[[value]])    
    }
}

cons_fred_pwt_df <- tibble(year = numeric(), 
                    mexc_mexy_sp = numeric(),
                    mexc_wldy_sp = numeric(), 
                    mexc_mexy_pe = numeric(),
                    mexc_wldy_pe = numeric(), 
                    )

for (t in 2009:2019){
    mexc_mexy_sp_temp <- cor(years_ranked[["mex_cons_fred"]][[as.character(t)]], years_ranked[["mex_growth_fred"]][[as.character(t)]], method = "spearman")
    mexc_wldy_sp_temp <- cor(years_ranked[["mex_cons_fred"]][[as.character(t)]], years_ranked[["wld_growth_pwt"]][[as.character(t)]], method = "spearman")

    mexc_mexy_pe_temp <- cor(values_stored[["mex_cons_fred"]][[as.character(t)]], values_stored[["mex_growth_fred"]][[as.character(t)]], method = "pearson")
    mexc_wldy_pe_temp <- cor(values_stored[["mex_cons_fred"]][[as.character(t)]], values_stored[["wld_growth_pwt"]][[as.character(t)]], method = "pearson")
    temp_df <- tibble(year = t, 
                    mexc_mexy_sp = mexc_mexy_sp_temp,
                    mexc_wldy_sp =  mexc_wldy_sp_temp,
                    mexc_mexy_pe = mexc_mexy_pe_temp,
                    mexc_wldy_pe = mexc_wldy_pe_temp)
    cons_fred_pwt_df <- bind_rows(cons_fred_pwt_df, temp_df)}

cons_fred_pwt_df <- cons_fred_pwt_df %>%
        tidyr::pivot_longer(cols = -year, names_to = "type", values_to = "value")

fred_pwtplot <- ggplot(data = cons_fred_pwt_df[cons_fred_pwt_df$type %in% c("mexc_mexy_pe", "mexc_wldy_pe" ), ], aes(x = year, y = value, colour = type)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c("mexc_mexy_pe" = "steelblue", "mexc_wldy_pe" = "firebrick"), labels = c("mexc_mexy_pe" = "Correlation with Domestic Output", "mexc_wldy_pe" = "Correlation with World Output")) +
    scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 2)) + 
    labs(title = "Mexican Private Consumption Pearson Correlations", 
    caption = "Source: FRED, Penn World Table\nNote: Consumption denotes final private consumption", 
    y = "Correlation") +
    theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
            plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
            axis.title.y = element_text(margin = margin(r = 8)),
            panel.grid = element_blank(),
            legend.position = "right",
            legend.title = element_text(size = 9, face = "bold"),
            legend.spacing.y = unit(1, 'cm'),
            plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_blank(),
            axis.line   = element_line(color = "black"),
            axis.ticks = element_line(color = "black"))

fred_pwtplot


ggsave(file = here::here("pearsonplot_short.png"), plot = fred_pwtplot, width = 10, height = 6, dpi = 300)

#####
##Now plot over long time horizon
####

seriesnames <- c("mex_growth_pwt", "mex_cons_pwt", "wld_growth_pwt")
seriesvals <- c("growth_pwt", "ccon_pwt", "wld_growth_pwt")
seriesdf <- tibble(series = seriesnames, 
                    values = seriesvals)


years_ranked <- list() ##create a list within a list
values_stored<- list()

for (i in 1:nrow(seriesdf)){
    series <- seriesdf[[i, "series"]]
    value <- seriesdf[[i, "values"]]
    years_ranked[[series]] <- list() ##create a list within a list
    values_stored[[series]] <- list()

    for (t in 1970:2019){
        temp_vec <- get(series) %>%
            mutate(year = as.numeric(year), across(.data[[value]], as.numeric)) %>%
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

for (t in 1970:2019){
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
    geom_line(linewidth = 2.5) +
    scale_colour_manual(values = c("mexc_mexy_pe" = "steelblue", "mexc_wldy_pe" = "firebrick"), labels = c("mexc_mexy_pe" = "Correlation with Domestic Output", "mexc_wldy_pe" = "Correlation with World Output")) +
    scale_x_continuous(breaks = seq(from = 1970, to = 2019, by = 5)) + 
    labs(title = "Mexican Private Consumption Pearson Correlations", 
    caption = "Source: Penn World Table\nNote: Consumption includes Private and Public Consumption Spending", 
    y = "Correlation") +
    theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
            plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
            axis.title.y = element_text(margin = margin(r = 8)),
            panel.grid = element_blank(),
            legend.position = "right",
            legend.title = element_text(size = 9, face = "bold"),
            legend.spacing.y = unit(1, 'cm'),
            plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_blank(),
            axis.line   = element_line(color = "black"),
            axis.ticks = element_line(color = "black"))

pwtplot


ggsave(file = here::here("pearsonplot_long.png"), plot = pwtplot, width = 10, height = 6, dpi = 300)


############################################################################# Plot Total Assets/Liabilities Compared to Other Countries ######################################################################## 

financialdata <- openxlsx::read.xlsx(here::here("quiz2", "homework.xlsx"), "Data")


assetcols <- c(colnames(financialdata)[stringr::str_detect(colnames(financialdata), "assets")], "FX.Reserves.minus.gold")
liabilitycols <- c(colnames(financialdata)[stringr::str_detect(colnames(financialdata), "liab")])

financial_df <- financialdata %>%
    rename(gdp = 'GDP.(US$)') %>%
    group_by(Year) %>%
    mutate(total_assets = rowSums(across(all_of(assetcols))),
            total_liabilities = rowSums(across(all_of(liabilitycols))),
            financial_integration = (total_assets + total_liabilities) / gdp * 100,
            net_position = (total_assets - total_liabilities) / gdp * 100) %>%
    ungroup()

####
##Compare Mexico to RoW & LatAm
####

row <- unique(financialdata$Country)
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

defacto_intplot <- ggplot(data = defacto_plot, aes(x = Year, y = value, colour = type)) +
            geom_line(linewidth = 2.5) +
            scale_colour_manual(values = c("latam" = "steelblue", "row" = "firebrick", "mexico" = "blue4"), labels = c("mexico" = "Mexico", "row" = "Rest of World", "latam" = "Latin America")) +
            scale_x_continuous(breaks = seq(from = 1981, to = 2014, by = 5)) + 
            labs(title = "Financial Integration, 1981-2014", 
            caption = "Source: IMF\nNote: Financial Integration is the sum of total liabilities and total assets as a percentage of GDP", 
            y = "Percentage") +
            theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
                    plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
                    plot.title.position = "plot",
                    plot.caption.position = "plot",
                    plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
                    axis.title.y = element_text(margin = margin(r = 8)),
                    panel.grid = element_blank(),
                    legend.position = "right",
                    legend.title = element_text(size = 9, face = "bold"),
                    legend.spacing.y = unit(1, 'cm'),
                    plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
                    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
                    axis.title.x = element_blank(),
                    axis.line   = element_line(color = "black"),
                    axis.ticks = element_line(color = "black"))
defacto_intplot
ggsave(file = here::here("integration.png"), plot = defacto_intplot, width = 10, height = 6, dpi = 300)


mex_nfp <- financial_df %>%
        filter(Country == "Mexico") %>%
        select(Year, net_position)


mex_nfp_plot <- ggplot(data = mex_nfp, aes(x = Year, y = net_position, colour = "mexico")) +
            geom_line(data = mex_nfp, linewidth = 2.5, colour = "blue4") +
            scale_x_continuous(breaks = seq(from = 1981, to = 2014, by = 5)) + 
            geom_hline(yintercept = -50, linetype = "dashed", colour = "black", linewidth = 1) +
            labs(title = "Net Foreign Asset Position, 1981-2014", 
            caption = "Source: IMF\nNote: Financial Integration is the sum of total liabilities and total assets as a percentage of GDP", 
            y = "Percentage of GDP") +
            theme(plot.title = element_text(size = rel(0.995), hjust = 0, face = "bold", margin = margin(b = 4, l = -4)),
                    plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 2)),
                    plot.title.position = "plot",
                    plot.caption.position = "plot",
                    plot.caption = element_text(hjust = 0, size = rel(0.7), margin = margin(t = 9)),
                    axis.title.y = element_text(margin = margin(r = 8)),
                    panel.grid = element_blank(),
                    legend.position = "right",
                    legend.title = element_text(size = 9, face = "bold"),
                    legend.spacing.y = unit(1, 'cm'),
                    plot.margin = unit(c(4, 0, 3, 5), units = 'pt'),
                    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
                    axis.title.x = element_blank(),
                    axis.line   = element_line(color = "black"),
                    axis.ticks = element_line(color = "black"))
mex_nfp_plot
ggsave(file = here::here("nfa.png"), plot = mex_nfp_plot, width = 10, height = 6, dpi = 300)
