if (!require(pacman)) install.packages("pacman")
pacman::p_unload(all)
pacman::p_load(tidyverse, janitor)
rm(list = ls())
install.packages("WDI")
# ------------------------------
library(WDI)         # To fetch World Bank data
library(dplyr)       # For data wrangling
library(tidyr)       # For reshaping
library(ggplot2)     # For plotting
library(stringr)     # For string cleaning
# ------------------------------
countries <- c("AUS", "DEU", "JPN", "SGP", "KOR", "SWE", "USA")  # ISO codes
indicators <- c("NY.GDP.PCAP.KD")  # GDP per capita (constant 2015 US$)
# ------------------------------
gdp_data <- WDI(country = countries, indicator = indicators,
                start = 2000, end = 2020, extra = TRUE)
# ------------------------------
gdp_clean <- gdp_data %>%
  rename(gdp_per_capita = NY.GDP.PCAP.KD) %>%
  select(country, iso2c, year, gdp_per_capita) %>%
  filter(!is.na(gdp_per_capita)) %>%
  mutate(country = case_when(
    iso2c == "AU" ~ "Australia",
    iso2c == "DE" ~ "Germany",
    iso2c == "JP" ~ "Japan",
    iso2c == "SG" ~ "Singapore",
    iso2c == "KR" ~ "South Korea",
    iso2c == "SE" ~ "Sweden",
    iso2c == "US" ~ "U.S.",
    TRUE ~ iso2c
  ))
# ------------------------------
ggplot(gdp_clean, aes(x = year, y = gdp_per_capita, color = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "GDP per capita (constant 2015 US$)",
       x = "Year", y = "GDP per capita (USD)", color = "Country") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
head(gdp_data)  # Look at the actual numbers