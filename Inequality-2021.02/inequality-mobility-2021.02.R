#!/usr/bin/env R
# @(#) inequality-mobility-2021.02.R
# Last-edited: Sun 2021.02.21.1101-- Danny Quah (me@DannyQuah.com)
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
library(tidyverse)
library(lubridate)
## remotes::install_github("WIDworld/wid-r-tool") 
library(wid)
source("./dl_wid_data.R", echo = FALSE)
source("./utilfuncs.R", echo = FALSE)

# In dl_wid_data() call, these "use*" arguments are used only 
# when cached is FALSE and readOnline is TRUE so we're pulling
# from the source.
# Use LCU/USD for longest timeseries (EUR doesn't go back far enough)
useAreas      <- "all"
# useAreas      <- c("CN", "SG", "US")
useYears      <- 1980:2019

theWIDdata.dt <- dl_wid_data(silent = FALSE, cached = TRUE,
  readOnline = FALSE, theAreas = useAreas, theYears = useYears)

# For consistent usage, add a variable that is all 1's
theWIDdata.dt <- theWIDdata.dt %>%
  mutate(exchRateLCU = 1.0)

# Choose one of the following to determine currency denomination to use.
# Because the underlying data are LCU in constant prices,
# setting the exchange rate to 1.0 catches that.

inLCU <- "exchRateLCU"
inUSD <- "exchRateUS"
inEUR <- "exchRateEU"


currUse.dt <- data.table(
  thisCurr = c(inLCU, inUSD, inEUR),
  nameCurr = c("LCU", "USD", "EUR")
                        )
theCurr  <- 1 # Choose index into currUse.dt

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

theEconomy <- "US"
theWIDdata.dt %>%
  filter(economy == theEconomy) %>%
  ggplot(., aes(x = year)) +
  labs(title = paste0(theEconomy, " - Inequality, B50"),
                      y = currAxis) +
  geom_line(aes(y = avgB50c), color = "darkred",
            linetype = "solid", size = 1.5) +
  geom_line(aes(y = ineqQ), color = "darkblue",
            linetype = "longdash", size = 1.5) +
  theme_economist(base_size = 14)

theWIDdata.dt %>%
  filter(economy == theEconomy) %>%
  ggplot(., aes(x = year)) +
  labs(title = paste0(theEconomy, " - B50"),
                      y = currAxis) +
  geom_line(aes(y = avgB50c), color = "darkred",
            linetype = "solid", size = 1.5) +
  theme_economist(base_size = 14)

theIndics.dt <- theWIDdata.dt %>%
  filter(economy == theEconomy) %>%
  filter(year %in% theYears) %>%
  select(ineqQ)
theOutp.str <- " Inequality"
showChangeGrowth(theIndics.dt, theEconomy, theOutp.str, theYears)

theIndics.dt <- theWIDdata.dt %>%
  filter(economy == theEconomy) %>%
  filter(year %in% theYears) %>%
  select(avgB50c)
theOutp.str <- " B50c"
showChangeGrowth(theIndics.dt, theEconomy, theOutp.str, theYears)

theIndics.dt <- theWIDdata.dt %>%
  filter(economy == theEconomy) %>%
  filter(year %in% theYears) %>%
  select(avgT10c)
theOutp.str <- " T10C"
showChangeGrowth(theIndics.dt, theEconomy, theOutp.str, theYears)

# Just additional checks below; no need to run each time
#
addlChecks <- FALSE
if (addlChecks) {
  theWIDdata.dt <- theWIDdata.dt %>%
    mutate(avgB50d  = avgB50 / (1000 * vrbCurr)) %>%
    mutate(trhB50d  = trhB50 / (1000 * vrbCurr)) %>%
    mutate(avgT10d  = avgT10 / (1000 * vrbCurr)) %>%
    mutate(trhT10d  = trhT10 / (1000 * vrbCurr))

  theWIDdata.dt %>%
    filter(economy == "US") %>%
    ggplot(., aes(x = year)) +
    geom_line(aes(y = avgB50c), color = "darkred",
              linetype = "solid", size = 1.5) +
    geom_line(aes(y = avgB50d), color = "steelblue",
              linetype = "longdash", size = 1.5) +
    theme_economist(base_size = 14)
}

# eof inequality-mobility-2021.02.R

