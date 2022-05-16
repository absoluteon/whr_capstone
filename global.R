
# Setup library

library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(glue)
library(lubridate)
library(forcats)
library(tidyr)
library(stringr)
library(countrycode)
library(rgdal)
library(raster)
library(leaflet)
library(RColorBrewer)
library(zoo)
library(shiny)
library(shinydashboard)
library(htmltools)


# Read data

# Data for all years
whr_all <- read.csv("whr_all.csv", encoding = "UTF-8") %>% 
  rename(country_name = X.U.FEFF.Country.name,
         life_ladder = Life.Ladder,
         log_gdp = Log.GDP.per.capita,
         social_support = Social.support,
         healthy_life_expectancy = Healthy.life.expectancy.at.birth,
         freedom_to_make_life_choices = Freedom.to.make.life.choices,
         generosity = Generosity,
         perceptions_of_corruption = Perceptions.of.corruption,
         positive_affect = Positive.affect,
         negative_affect = Negative.affect)

# 2021 report only
whr_2021 <- read.csv("whr2021.csv", encoding = "UTF-8") %>%
  dplyr::select(c(X.U.FEFF.country_name,
                  ladder_score,
                  explainedby_log_gdp_per_capita,
                  explainedby_social_support,
                  explainedby_healthy_life_expectancy,
                  explainedby_freedom_to_make_life_choices,
                  explainedby_generosity,
                  explainedby_perceptions_of_corruption,
                  dystopia_residual)) %>%
  rename(country_name = X.U.FEFF.country_name,
         log_gdp_per_capita = explainedby_log_gdp_per_capita,
         social_support = explainedby_social_support,
         healthy_life_expectancy = explainedby_healthy_life_expectancy,
         freedom_to_make_life_choices = explainedby_freedom_to_make_life_choices,
         generosity = explainedby_generosity,
         perceptions_of_corruption = explainedby_perceptions_of_corruption)

# Read data for leaflet
leaflet_data <- read.csv("leaflet_data.csv", encoding = "UTF-8") %>%
  dplyr::select(c(X.U.FEFF.country_name,
                  log_gdp_per_capita,
                  ladder_score
  )) %>%
  rename(NAME = X.U.FEFF.country_name) %>%
  filter(!NAME %in% c("Kosovo", "Congo (Brazzaville)", "Ivory Coast", "Laos", "North Cyprus", "Somaliland region"))


leaflet_data$NAME[leaflet_data$NAME == "Congo (Kinshasa)"] <- "Congo"
leaflet_data$NAME[leaflet_data$NAME == "Hong Kong S.A.R. of China"] <- "Hong Kong"
leaflet_data$NAME[leaflet_data$NAME == "Iran"] <- "Iran (Islamic Republic Of)"
leaflet_data$NAME[leaflet_data$NAME == "Laos"] <- "Lao People's Democratic Republic"
leaflet_data$NAME[leaflet_data$NAME == "Libya"] <- "Libyan Arab Jamahiriya"
leaflet_data$NAME[leaflet_data$NAME == "Palestinian Territories"] <- "Palestine"
leaflet_data$NAME[leaflet_data$NAME == "South Korea"] <- "Korea, Republic of"
leaflet_data$NAME[leaflet_data$NAME == "Taiwan Province of China"] <- "Taiwan"
leaflet_data$NAME[leaflet_data$NAME == "Tanzania"] <- "United Republic of Tanzania"
leaflet_data$NAME[leaflet_data$NAME == "Vietnam"] <- "Viet Nam"


# Wrangling

# Replace NA with 0
whr_all[is.na(whr_all)] = 0

# Further wrangling for whr_all
# exclude Kosovo as it is not ISO-recognized country
whr_all <- whr_all %>% 
  filter(country_name != "Kosovo")

# match regions
whr_all$region <- countrycode(sourcevar = whr_all[,"country_name"],
                              origin = "country.name",
                              destination = "region")

# match continents
whr_all$continent <- countrycode(sourcevar = whr_all[,"country_name"],
                                 origin = "country.name",
                                 destination = "continent")


# Further wrangling for whr_2021
# exclude Kosovo as it is not ISO-recognized country
whr_2021 <- whr_2021 %>% 
  filter(country_name != "Kosovo")

# match region
whr_2021$region <- countrycode(sourcevar = whr_2021[,"country_name"],
                               origin = "country.name",
                               destination = "region")

# match continent
whr_2021$continent <- countrycode(sourcevar = whr_2021[,"country_name"],
                                  origin = "country.name",
                                  destination = "continent")


# Further wrangling for leaflet
# Read shape file
shape <- raster::shapefile("TM_WORLD_BORDERS_SIMPL-0.3.shp")

# Clean the data object
shape@data <- shape@data %>% left_join(leaflet_data, by = "NAME")

# Create color palette
bins <- c(0, 2, 4, 6, 8)
pal <- colorBin("RdBu", domain = shape@data$ladder_score, na.color = "transparent", bins = bins)

# Prepare text for tooltips
mytext <- paste(
  "Country: ", shape@data$NAME,"<br/>", 
  "Happiness score: ", round(shape@data$ladder_score, 2), "<br/>",   
  "Log GDP/capita: ", shape@data$log_gdp_per_capita, 
  sep = "") %>%
  lapply(htmltools::HTML)


## For Tab 1 - Country: Happiest & Unhappiest Countries

# Happiest countries
top10countries <- whr_2021 %>% 
  dplyr::select(country_name, ladder_score, continent, region) %>%
  arrange(desc(ladder_score)) %>% 
  mutate(label = glue("Score: {ladder_score}
                      Continent: {continent}
                     Region: {region}")) %>% 
  head(10)

happiest_country <- top10countries[1:1,1:1]

#Unhappiest countries
lowest10countries <- whr_2021 %>% 
  dplyr::select(country_name, ladder_score, continent, region) %>%
  arrange(desc(ladder_score)) %>% 
  mutate(label = glue("Score: {ladder_score}
                      Continent: {continent}
                     Region: {region}")) %>% 
  tail(10)

unhappiest_country <- lowest10countries[10:10,1:1]


## For Tab 2: Through the years

# Happiness score through the years, by country (choose)
whr_hist <- whr_all %>% 
  dplyr::select(country_name, year, life_ladder) %>% 
  mutate(label = glue("Year: {year}
                      Score: {life_ladder}"))

# Happiness score through the years, by region (choose)
regions <- whr_all %>% 
  dplyr::select(c(country_name, year, life_ladder, continent, region)) %>% 
  mutate(label = glue("Continent: {continent}"))
# year = lubridate::year(as.Date(zoo::as.yearmon(year))))

selectCountry1 <- unique(whr_all$country_name)
selectCountry2 <- unique(whr_all$country_name)
selectRegion <- unique(whr_all$region)
year_range <- range(whr_all$year)


## For Tab 3: Factors

# Gather factors
whr_factors <- whr_2021 %>% 
  dplyr::select(-c(region, continent)) %>% 
  gather(factors, values, 3:9) %>% 
  arrange(country_name)


whr_factors$factors[whr_factors$factors == "log_gdp_per_capita"] <- "Log GDP per capita"
whr_factors$factors[whr_factors$factors == "social_support"] <- "Social support"
whr_factors$factors[whr_factors$factors == "healthy_life_expectancy"] <- "Healthy life expectancy"
whr_factors$factors[whr_factors$factors == "freedom_to_make_life_choices"] <- "Freedom to make life choices"
whr_factors$factors[whr_factors$factors == "generosity"] <- "Generosity"
whr_factors$factors[whr_factors$factors == "perceptions_of_corruption"] <- "Perceptions of corruption"
whr_factors$factors[whr_factors$factors == "dystopia_residual"] <- "Dystopia + residual"


whr_factors <- whr_factors %>% 
  mutate(label = glue("Explained by {factors}: {values}"))

whr_factors$orders <- recode(whr_factors$factors,
                             "Log GDP per capita" = 7, 
                             "Social support" = 6, 
                             "Healthy life expectancy" = 5, 
                             "Freedom to make life choices" = 4, 
                             "Generosity" = 3, 
                             "Perceptions of corruption" = 2, 
                             "Dystopia + residual" = 1)

selectContinent1 <- unique(whr_all$continent)
selectContinent2 <- unique(whr_all$continent)
