#!/usr/bin/env R
# @(#) covid-base-2021.01.R
# Last-edited: Mon 2021.05.24.1731  -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sat 2021.01.30.1610  -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to ...
# ----------------------------------------------------------------
library(RCurl)
library(data.table)
library(countrycode)
library(ggthemes)
library(ggrepel)
library(tidyverse)
## remotes::install_github("joachim-gassen/tidycovid19")
# library(tidycovid19)
library(lubridate)
library(scales)

# My functions
source("./dl_owid_covid_data.R", echo=FALSE)
source("./utilfuncs.R", echo=FALSE)
## IMF WEO functions
theDlIMFweoIndivsSrc <-
  file.path("~", "0", "Light", "1", "j", "Code",
            "IMF-WEO-Dynamics-2021.01", "dl-imf-weo-indivs.R")
source(theDlIMFweoIndivsSrc, echo=FALSE)

# Read in the data
theOWID.dt <- dl_owid_covid_data(cached=TRUE, readOnline=FALSE,
                                 silent=FALSE)
myWEOindivs <- dlIMFweoIndivs(WEOcurrIndivs="WEOApr2021all",
                              cached=TRUE, readOnline=FALSE,
                              silent=FALSE)
myWEOeconomies.dt      <- myWEOindivs$myWEOeconomies.dt
myWEOeconomiesRefCodes <- myWEOindivs$myEconomiesRefCodes
remove(myWEOindivs)

# Previously selecting first but 
# let's now work with all the variables to begin
# theOWID.dt <- theOWID.dt %>%
#     rename(theISO3c = iso_code) %>%
#     select(theISO3c, continent, date, total_cases, total_deaths,
#            total_deaths_per_million, population)
theOWID.dt <- theOWID.dt %>%
    rename(theISO3c = iso_code) %>% rename(theDate = date) %>%
    mutate(theDate = as.Date(theDate))
myWEOeconomies.dt <- myWEOeconomies.dt %>%
  rename(theISO3c = ISO) %>% rename(theYear = year)

econSelect   <- c("GBR", "USA", "IND", "BRA", "DEU", "CHN", "SGP")

# thisCovidInd <- "total_cases_per_million"
thisCovidInd <- "total_deaths_per_million"

beginDate    <- ymd("2020-04-01")
endDate      <- ymd("2021-05-23")
dateEndPts   <- c(beginDate, endDate)
showDailyGraph(theOWID.dt, thisCovidInd, econSelect, dateEndPts)

thisEconInd  <- "NGDP_RPCH"
beginYear    <- 2010
endYear      <- 2022
yearEndPts   <- c(beginYear, endYear)
showAnnualGraph(myWEOeconomies.dt, thisEconInd, econSelect, yearEndPts)

xSect.dt <- crossShow(theOWID.dt, thisCovidInd,
                      myWEOeconomies.dt, thisEconInd,
                      NULL, econSelect) %>%
  mutate(econName=countrycode(theISO3c, origin="iso3c",
                              destination="cldr.name.en"))

thisDate <- "2021-01-11"
theEconomies <- c("AUS", "BRA", "CAN", "CHN", "FIN", "FRA",
  "DEU", "GRC", "IND", "IDN", "JPN", "MYS", "NZL", "SGP", "SWE",
  "TWN", "TJK", "THA", "GBR", "USA", "VNM", "OWID_WRL") 

myOWID_covid.dt <- theOWID.dt %>%
  filter(date == thisDate) %>%
  filter(theISO3c %in% theEconomies) %>%
  mutate(lives_per_covid_death = 10^6 / total_deaths_per_million)

# eof covid-base-2021.01.R

