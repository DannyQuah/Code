#!/usr/bin/env R
# @(#) covid-base-2021.01.R
# Last-edited: Wed 2021.02.10.1731  -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sat 2021.01.30.1610  -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to ...
# ----------------------------------------------------------------
library(RCurl)
library(data.table)
library(countrycode)
library(ggthemes)
library(tidyverse)
## remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)
source("./dl_owid_covid_data.R", echo = FALSE)

theOWID.dt <- dl_owid_covid_data(cached = FALSE, silent = FALSE,
                            readOnline = TRUE)

theOWID.dt <- theOWID.dt %>%
    rename(iso3c = iso_code) %>%
    select(iso3c, continent, date, total_cases, total_deaths,
           total_deaths_per_million, population)

thisDate <- "2021-01-31"
theEconomies <- c("AUS", "BRA", "CAN", "CHN", "FIN", "FRA",
  "DEU", "GRC", "IND", "IDN", "JPN", "MYS", "NZL", "SGP", "SWE",
  "TWN", "TJK", "THA", "GBR", "USA", "VNM", "OWID_WRL") 

myOWID_covid.dt <- theOWID.dt %>%
  filter(date == thisDate) %>%
  filter(iso3c %in% theEconomies) %>%
  mutate(lives_per_covid_death = 10^6 / total_deaths_per_million)

# eof covid-base-2021.01.R

