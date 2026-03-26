pacman::p_load(dplyr, tidyr, ggplot2)

data <- openxlsx::read.xlsx(here::here("quiz2", "homework.xlsx"), "Data")
# This question covers q1-q4.

# Since the end of the 1980s, several countries have undertaken a series of reforms in order to liberalize their financial transactions.
# Some argue that one of the benefits of this openness is higher economic growth. Using the available data (homework.xls) assess whether countries with higher levels of financial integration in 1981 had higher levels of economic growth between 1981 and 2014.
# Note: The file homework.xls Download homework.xlshas information on assets and liabilities for 150 countries since 1981.
# Important: Handle missing values appropriately in your code. Think carefully about how to handle missing values. Preferably, inspect your data before estimation.
# To break this analysis down, we answer it in the following sub questions:
# a)
# Define financial integration as follows: Financial Integrationi,t = (total assetsi,t + total liabilitiesi,t ) / GDPi,t         

# q1: Use this definition to plot the degree of financial integration for Argentina, France and the United States to answer 2 statements. 
# We only focus on the subsample of Argentina, France and the United States in this question.

assetcols <- c(colnames(data)[stringr::str_detect(colnames(data), "assets")], "FX.Reserves.minus.gold")
liabilitycols <- c(colnames(data)[stringr::str_detect(colnames(data), "liab")])
names(data)
print(assetcols)
print(liabilitycols)

df <- data %>%
    rename(gdp = 'GDP.(US$)') %>%
    group_by(Year) %>%
    mutate(total_assets = rowSums(across(all_of(assetcols))),
            total_liabilities = rowSums(across(all_of(liabilitycols))),
            financial_integration = (total_assets + total_liabilities) / gdp) %>%
    ungroup()

View(df[df$Country == "France",])
q1 <- df %>%
    filter(Country %in% c("Argentina", "France", "United States")) 

sum(is.na(q1$financial_integration))
q1plot <- ggplot(data = q1, aes(x = as.numeric(Year), y = financial_integration, colour = Country)) +
    geom_line() +
    labs(
        title = "Financial Integration over Time",
        x = "Year",
        y = "Financial Integration",
        colour = "Country"
    ) +
    theme_minimal()
ggsave(here::here("quiz2", "q1plot.png"), plot = q1plot, width = 10, height = 6, dpi = 300)


# b)

# Calculate the total growth rate of GDP between 1981-2014 (not the annual average growth rate) in decimal notation.

# Note: suppose X grew from 100 to 110, enter the growth rate as (110-100)/100=0.1 NOT 10%!

growthrates <- tibble(country = character(), 
                        gdp_growth = numeric(), 
                        fi_1981 = numeric())

for (country in unique(data$Country)){
    temp_df <- df %>%
        filter(Country == country, Year %in% c(1981, 2014)) %>%
        mutate(growth = (gdp[Year == 2014] - gdp[Year == 1981]) / gdp[Year == 1981]) 

    gdp_growth <- temp_df$growth[1]
    
    fi <- temp_df$financial_integration[temp_df$Year == 1981]

    temp_row <- tibble(country = country, 
                    gdp_growth = gdp_growth, 
                    fi_1981 = fi)
    
    growthrates <- bind_rows(growthrates, temp_row)
}
View(growthrates)

print(psych::describe(growthrates), digits = 4)

print(growthrates %>% arrange(desc(gdp_growth)) %>% head(15))


temp_df <- df %>%
        filter(Country == "France", Year %in% c(1981, 2014)) %>%
        mutate(growth = (gdp[Year == 2014] - gdp[Year == 1981]) / gdp[Year == 1981])

View(temp_df)
# q2: Report the sample average and sample standard deviation of the growth rate and financial integration in 1981.

 

# c)

# q3: Run a linear regression to estimate the effect of the level of financial integration in 1981 and total growth rate of GDP between 1981-2014.

 
ln_ = lm(formula = gdp_growth ~ fi_1981, data = growthrates)
print(summary(ln_))
# d)

# q4: Interpret your findings based on evaluating 2 statements.