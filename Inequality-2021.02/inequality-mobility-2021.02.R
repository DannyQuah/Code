#!/usr/bin/env R
# @(#) inequality-mobility-2021.02.R
# Last-edited: Sat 2021.07.03.0912 -- Danny Quah (me@DannyQuah.com)
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
source("./dl-wid-data.R", echo = FALSE)
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

# Sensible variable names in quickly
theWIDdata.dt <- theWIDdata.dt %>%
  rename(theYear=year) %>%
  rename(theISO2c=economy)

# Name the economies;
# drop an explicit selection, subnational regions, and
# world region PPP and Market Exchange Rate observations; and
# for consistent usage, add a variable that is all 1's
worldRegions <- 1
subntRegions <- 2
# Exceptional economies to exclude
# DD - GDR
# IQ - Iraq
# KS - ?
# TM - Turkmenistan
# VE - USD exchange rate goes through the roof; don't use
#      if theCurr points to USD. Trap this and others in crossShow()
# ZZ - Zanzibar
xcptEconomies <- c("DD", "IQ", "KS", "TM", "VE", "ZZ")
exclEconomies <- c(regionList(subntRegions, theWIDdata.dt),
                   regionList(worldRegions, theWIDdata.dt),
                   xcptEconomies
                  )
theWIDdata.dt <- theWIDdata.dt %>%
  filter(! theISO2c %in% exclEconomies) %>%
  mutate(econName=countrycode(theISO2c, origin="iso2c",
                              destination="cldr.name.en")) %>%
  relocate(econName, .after=theISO2c) %>%
  mutate(exchRateLCU=1.0)

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

theYears <- c(1980, 2000, 2019)

theWIDdata.dt <- theWIDdata.dt %>%
  mutate(avgNIuse = avgNatlInc / (1000 * vrbCurr)) %>%
  mutate(avgB50c  = shrB50 * avgNIuse / 0.5) %>%
  mutate(avgT10c  = shrT10 * avgNIuse / 0.1) %>%
  mutate(ineqQ    = avgT10c - avgB50c) %>%
  mutate(ineqq    = ineqQ / avgB50c)

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
xSect.dt <- crossShow(theWIDdata.dt, labelEconomies,
                      exclEconomies, theTimeSpan)

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

