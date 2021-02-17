#!/usr/bin/env R
# @(#) inequality-mobility-2021.02.R
# Last-edited: Wed 2021.02.17.1709-- Danny Quah (me@DannyQuah.com)
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

# In dl_wid_data() call, these "use*" arguments are used only 
# when cached is FALSE and readOnline is TRUE so we're pulling
# from the source.
# Use LCU/USD for longest timeseries (EUR doesn't go back far enough)
useAreas      <- "all"
# useAreas      <- c("CN", "SG", "US")
useYears      <- 1980:2019

theWIDdata.dt <- dl_wid_data(silent = FALSE, cached = TRUE,
  readOnline = FALSE, theAreas = useAreas, theYears = useYears)

# Choose one of these to decide which currency denomination to use.
# Because the underlying data are LCU, setting useCurr to 1.0 catches
# that. Alternatively, inflation-deflated values derive from
# setting useCurr to the national income price index
# useCurr <- exchRateUS
# useCurr <- exchRateEU
useCurr <- 1.0

theWIDdata.dt <- theWIDdata.dt %>%
  mutate(avgB50d  = avgB50 / (1000 * useCurr)) %>%
  mutate(trhB50d  = trhB50 / (1000 * useCurr)) %>%
  mutate(avgT10d  = avgT10 / (1000 * useCurr)) %>%
  mutate(trhT10d  = trhT10 / (1000 * useCurr)) %>%
  mutate(avgNIuse = avgNatlInc / (1000 * useCurr)) %>%
  mutate(avgB50c  = shrB50 * avgNIuse / 0.5) %>%
  mutate(avgT10c  = shrT10 * avgNIuse / 0.1) %>%
  mutate(ineqQ    = avgT10c - avgB50c) %>%
  mutate(ineqq    = ineqQ / avgB50c)

theWIDdata.dt %>%
  filter(economy == "US") %>%
  ggplot(., aes(x = year)) +
  geom_line(aes(y = avgB50c), color = "darkred",
            linetype = "solid", size = 1.5) +
  geom_line(aes(y = avgB50d), color = "steelblue",
            linetype = "longdash", size = 1.5) +
  theme_economist(base_size = 14)

theEconomy <- "SG"
theWIDdata.dt %>%
  filter(economy == theEconomy) %>%
  ggplot(., aes(x = year)) +
  labs(title = paste0(theEconomy, " - Inequality, B50"),
                      y = "USDx10^3") +
  geom_line(aes(y = avgB50c), color = "darkred",
            linetype = "solid", size = 1.5) +
  geom_line(aes(y = ineqQ), color = "darkblue",
            linetype = "longdash", size = 1.5) +
  theme_economist(base_size = 14)

theWIDdata.dt %>%
  filter(economy == theEconomy) %>%
  ggplot(., aes(x = year)) +
  labs(title = paste0(theEconomy, " - B50"),
                      y = "USDx10^3") +
  geom_line(aes(y = avgB50c), color = "darkred",
            linetype = "solid", size = 1.5) +
  theme_economist(base_size = 14)


# eof inequality-mobility-2021.02.R

