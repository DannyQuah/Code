#!/usr/bin/env R
# @(#) inequality-mobility-2021.02.R
# Last-edited: Tue 2021.07.13.1829 -- Danny Quah (me@DannyQuah.com)
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

theWIDdata.dt <- ntlEntitiesClean(theWIDdata.dt)

# Choose one of the following to determine currency denomination to use.
# Because the underlying data are already LCU in constant prices,
# setting the exchange rate to 1.0 catches that.

inLCU <- "exchRateLCU"
inUSD <- "exchRateUS"
inEUR <- "exchRateEU"

currUse.dt <- data.table(
  thisCurr = c(inLCU, inUSD, inEUR),
  nameCurr = c("LCU", "USD", "EUR")
                        )
theCurr  <- 2 # Choose index into currUse.dt // 1 - LCU, 2 - USD, ...
useCurr  <- currUse.dt$thisCurr[theCurr]
nameCurr <- currUse.dt$nameCurr[theCurr]
vrbCurr  <- theWIDdata.dt[[useCurr]]
currAxis <- paste0(nameCurr, "x10^3")

theWIDdata.dt <- theWIDdata.dt %>%
  mutate(avgNIuse = avgNatlInc / (1000 * vrbCurr)) %>%
  mutate(avgB50c  = shrB50 * avgNIuse / 0.5) %>%
  mutate(avgT10c  = shrT10 * avgNIuse / 0.1) %>%
  mutate(ineqQ    = avgT10c - avgB50c) %>%
  mutate(ineqq    = ineqQ / avgB50c)

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

