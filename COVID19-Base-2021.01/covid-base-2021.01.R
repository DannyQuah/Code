#!/usr/bin/env R
# @(#) covid-base-2021.01.R
# Last-edited: Fri 2021.07.16.1322  -- Danny Quah (me@DannyQuah.com)
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

# ----------------------------------------------------------------
# -- My functions ------------------------------------------------
# ----------------------------------------------------------------
source("./mng-owid-covid-data.R", echo=FALSE)
source("./covid-utilfuncs.R", echo=FALSE)
source("../Routines/stats-dq.R", echo=FALSE)

## IMF WEO functions
theDlIMFweoIndivsSrc <-
  file.path("~", "0", "Light", "1", "j", "Code",
            "IMF-WEO-Dynamics-2021.01", "mng-imf-weo-indivs.R")
source(theDlIMFweoIndivsSrc, echo=FALSE)
## WID functions
theDlWIDsrc <-
  file.path("~", "0", "Light", "1", "j", "Code",
            "Inequality-2021.02", "mng-wid-data.R")
source(theDlWIDsrc, echo=FALSE)

# ----------------------------------------------------------------
# -- Read in the data --------------------------------------------
# ----------------------------------------------------------------

  ## OWID data straightforward but also some renaming
theOWID.dt <- dl_owid_covid_data(blCached=TRUE, blReadOnline=FALSE,
                                 blSilent=FALSE)
theOWID.dt <- theOWID.dt %>%
    rename(theISO3c=iso_code) %>% rename(theDate=date) %>%
    mutate(theDate=as.Date(theDate))


  ## IMF WEO data; some processing, and some renaming
myWEOindivs <- dlIMFweoIndivs(blCached=TRUE, blReadOnline=FALSE,
                              blSilent=FALSE,
                              WEOcurrIndivs="WEOApr2021all")
myWEOeconomies.dt      <- myWEOindivs$myWEOeconomies.dt
myWEOeconomiesRefCodes <- myWEOindivs$myEconomiesRefCodes
remove(myWEOindivs)
myWEOeconomies.dt <- myWEOeconomies.dt %>%
  rename(theISO3c=ISO) %>% rename(theYear=year)

  # World Inequality Data
useAreas      <- "all"
useYears      <- 1980:2019
theWID.dt <- dl_wid_data(blCached=TRUE, blReadOnline=FALSE,
                         blSilent=FALSE, theAreas=useAreas,
                         theYears=useYears)
theWID.dt <- theWID.dt %>% ntlEntitiesClean()
# Choose one of the following to determine currency denomination to use.
# Because the underlying data are already LCU in constant prices,
# setting the exchange rate to 1.0 catches that.
currUse.dt <- data.table(
  thisCurr = c("exchRateLCU", "exchRateUS", "exchRateEU"),
  nameCurr = c("LCU", "USD", "EUR")
  )
  # Choose index into currencies: 1- LCU, 2 - USD, 3 - EU (don't use)
ndxCurr <- 2
useCurr <- currUse.dt$thisCurr[ndxCurr]
theWID.dt <- makeDistrStats(theWID.dt, useCurr)


econMain  <- c("GBR", "USA", "IND", "BRA", "DEU", "CHN", "SGP")
econASEAN <- c("BRN", "KHM", "IDN", "LAO", "MYS",
               "MMR", "PHL", "SGP", "THA", "VNM")  
econSelect <- unique(c(econMain, econASEAN))


# ----------------------------------------------------------------
# -- Relevant indicators from different datatables ---------------
# ----------------------------------------------------------------

# -- OWID // COVID -----------------------------------------------
# thisCovidInd <- "total_cases_per_million"
thisCovidInd <- "total_deaths_per_million"

# Use the latest date the last day recorded common to
# all economies of interest  
endDate <- lastAvail(theOWID.dt, econSelect, thisCovidInd)

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
# Sanity check to confirm everything is OK
showDailyGraph(theOWID.dt, thisCovidInd, econMain, dateEndPts)

# -- IMF // WEO --------------------------------------------------
# - Percent growth rate in inflation-adjusted USD
thisEconInd  <- "NGDP_RPCH"
beginYear    <- 2010
endYear      <- 2022
yearEndPts   <- c(beginYear, endYear)
# Sanity check to confirm everything is OK
showAnnualGraph(myWEOeconomies.dt, thisEconInd, econMain, yearEndPts)

# -- WID ---------------------------------------------------------
# Inequality, B50, average
theDistrInds <- c("avgNIc", "ineqQ", "avgB50c")

# ----------------------------------------------------------------
# -- Analysis ----------------------------------------------------
# ----------------------------------------------------------------
# A lot happens in crossBuildPerfs:
#  calculate performance indicators covidPerf, econPerf, ...
#  join datatables for the different performance indicators
xPerfIndics.dt <- crossBuildPerfs(theOWID.dt, thisCovidInd,
                                  myWEOeconomies.dt, thisEconInd,
                                  theWID.dt, theDistrInds)

xPerfIndics.dt <- xPerfIndics.dt %>%
  crossShow(., NULL, econSelect)

xPerfIndics.dt %>%
  select(covidPerf, econPerf) %>%
  corrSignif()

# eof covid-base-2021.01.R

