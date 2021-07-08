#!/usr/bin/env R
# @(#) covid-base-2021.01.R
# Last-edited: Fri 2021.07.02.2313  -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sat 2021.01.30.1630  -- Danny Quah (me@DannyQuah.com)
## remotes::install_github("joachim-gassen/tidycovid19")
# library(tidycovid19)
#  % Sat 2021.01.30.1610  -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to ...
# ----------------------------------------------------------------
## remotes::install_github("WIDworld/wid-r-tool") 
library(wid)
library(RCurl)
library(data.table)
library(countrycode)
library(ggthemes)
library(ggrepel)
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(correlation)

# My functions
source("./dl-owid-covid-data.R", echo=FALSE)
source("./utilfuncs.R", echo=FALSE)
source("../Routines/stats-dq.R", echo=FALSE)

## IMF WEO functions
theDlIMFweoIndivsSrc <-
  file.path("~", "0", "Light", "1", "j", "Code",
            "IMF-WEO-Dynamics-2021.01", "dl-imf-weo-indivs.R")
source(theDlIMFweoIndivsSrc, echo=FALSE)
## WID functions
theDlWIDsrc <-
  file.path("~", "0", "Light", "1", "j", "Code",
            "Inequality-2021.02", "dl-wid-data.R")
source(theDlWIDsrc, echo=FALSE)

# Read in the data
theOWID.dt <- dl_owid_covid_data(blCached=TRUE, blReadOnline=FALSE,
                                 blSilent=FALSE)
myWEOindivs <- dlIMFweoIndivs(blCached=TRUE, blReadOnline=FALSE,
                              blSilent=FALSE,
                              WEOcurrIndivs="WEOApr2021all")
myWEOeconomies.dt      <- myWEOindivs$myWEOeconomies.dt
myWEOeconomiesRefCodes <- myWEOindivs$myEconomiesRefCodes
remove(myWEOindivs)

useAreas      <- "all"
useYears      <- 1980:2019
theWIDdata.dt <- dl_wid_data(blCached=TRUE, blReadOnline=FALSE,
                             blSilent=FALSE, theAreas=useAreas,
                             theYears=useYears)
theWIDdata.dt <- theWIDdata.dt %>% mutate(exchRateLCU=1.0)

theOWID.dt <- theOWID.dt %>%
    rename(theISO3c=iso_code) %>% rename(theDate=date) %>%
    mutate(theDate=as.Date(theDate))
myWEOeconomies.dt <- myWEOeconomies.dt %>%
  rename(theISO3c=ISO) %>% rename(theYear=year)

econMain  <- c("GBR", "USA", "IND", "BRA", "DEU", "CHN", "SGP")
econASEAN <- c("BRN", "KHM", "IDN", "LAO", "MYS",
               "MMR", "PHL", "SGP", "THA", "VNM")  
econSelect <- unique(c(econMain, econASEAN))

# thisCovidInd <- "total_cases_per_million"
thisCovidInd <- "total_deaths_per_million"

# Use the latest date the last day recorded common to
# all economies of interest  
tmp.dt <- theOWID.dt %>% filter(theISO3c %in% econSelect) %>%
  group_by(theISO3c) %>%
  filter(!is.na(total_cases_per_million)) %>%
  filter(!is.na(total_deaths_per_million)) %>%
  arrange(theDate) %>% slice_tail() %>%
  ungroup() %>%
  arrange(theDate) %>% slice_head()
endDate <- tmp.dt$theDate
remove(tmp.dt)

beginDate    <- ymd("2020-04-01")
dateEndPts   <- c(beginDate, endDate)
showDailyGraph(theOWID.dt, thisCovidInd, econMain, dateEndPts)

thisEconInd  <- "NGDP_RPCH"
beginYear    <- 2010
endYear      <- 2022
yearEndPts   <- c(beginYear, endYear)
showAnnualGraph(myWEOeconomies.dt, thisEconInd, econMain, yearEndPts)

xPerfIndics.dt <- crossBuildPerfs(theOWID.dt, thisCovidInd,
                                  myWEOeconomies.dt, thisEconInd)

xPerfIndics.dt <- xPerfIndics.dt %>%
  crossShow(., NULL, econSelect)

xPerfIndics.dt %>%
  select(covidPerf, econPerf) %>%
  corrSignif()

# eof covid-base-2021.01.R

