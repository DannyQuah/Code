#!/usr/bin/env R
# @(#) inequality-mobility-2021.02.R
# Last-edited: Wed 2021.07.14.2210 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Fri 2021.02.12.1733  -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to analyse inequality and social mobility
#    directly in cloud data
# ----------------------------------------------------------------
library(data.table)
library(countrycode)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(tidyverse)
library(lubridate)
## remotes::install_github("WIDworld/wid-r-tool") 
library(wid)
source("./mng-wid-data.R", echo = FALSE)
source("./utilfuncs.R", echo = FALSE)
source("../Routines/stats-dq.R", echo=FALSE)

# In dl_wid_data() call, these "use*" arguments are used only 
# when cached is FALSE and readOnline is TRUE so we're pulling
# from the source.
# Use LCU/USD for longest timeseries (EUR doesn't go back far enough)
useAreas      <- "all"
# useAreas      <- c("CN", "SG", "US")
useYears      <- 1980:2019

theWIDdata.dt <- dl_wid_data(blCached=TRUE, blReadOnline=FALSE,
                             blSilent=FALSE, theAreas=useAreas,
                             theYears=useYears)
theWIDdata.dt <- theWIDdata.dt %>% ntlEntitiesClean()

# Choose one of the following to determine currency denomination to use.
# Because the underlying data are already LCU in constant prices,
# setting the exchange rate to 1.0 catches that.

currUse.dt <- data.table(
  thisCurr = c("exchRateLCU", "exchRateUS", "exchRateEU"),
  nameCurr = c("LCU", "USD", "EUR")
  )
  # Choose index into currencies: 1- LCU, 2 - USD, 3 - EU (don't use)
ndxCurr <- 2
lblCurr <- currUse.dt$nameCurr[ndxCurr]
useCurr <- currUse.dt$thisCurr[ndxCurr]
theWIDdata.dt <- makeDistrStats(theWIDdata.dt, useCurr)

currAxis <- paste0(lblCurr, "x10^3")
# Selected year breaks
theYears <- c(1980, 2000, 2019)
# Selected economies
myEconomies <- c("SG", "US", "CN")
graphThem   <- TRUE
for (theEconomy in myEconomies) {
  summaryShow(theWIDdata.dt, theEconomy, currAxis, theYears, graphThem)
}

labelEconomies <- c("SG", "US", "CN", "NO",
                    "EE", "TH",
                    "BG", "LT")

# Cross section of economies
theTimeSpan <- c(2000:2019)
xSect.dt <- crossShow(theWIDdata.dt, labelEconomies, theTimeSpan)

# Take a look at the economies
xSect.dt %>% filter(theISO2c %in% labelEconomies) %>%
  select(theISO2c, econName, mltB50c, mltIneqQ, locIneqQ)
xSect.dt %>% arrange(desc(mltB50c)) %>%
  select(theISO2c, econName, mltB50c, mltIneqQ, locIneqQ)
xSect.dt %>% arrange(desc(mltIneqQ)) %>%
  select(theISO2c, econName, mltB50c, mltIneqQ, locIneqQ)
xSect.dt %>% arrange(desc(locIneqQ)) %>%
  select(theISO2c, econName, mltB50c, mltIneqQ, locIneqQ)
xSect.dt %>% filter(mltB50c > 1.0) %>% count()
xSect.dt %>% filter(mltIneqQ > 1.0) %>% count()
xSect.dt %>% filter((mltB50c > 1.0) & (mltIneqQ > 1.0)) %>% count()
xSect.dt %>% filter((mltB50c > 1.0) & (mltIneqQ <= 1.0))


xSect.dt %>% select(mltB50c, mltIneqQ, locIneqQ) %>%
  corrSignif()


# Just additional checks below; no need to run each time
#
addlChecks <- FALSE
if (addlChecks) {
  addChecks(theWIDdata.dt, theEconomy, vrbCurr)
}

# eof inequality-mobility-2021.02.R

