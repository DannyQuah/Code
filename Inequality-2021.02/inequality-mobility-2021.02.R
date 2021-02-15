#!/usr/bin/env R
# @(#) inequality-mobility-2021.02.R
# Last-edited: Sun 2021.02.14.2351-- Danny Quah (me@DannyQuah.com)
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
useAreas      <- c("CN", "SG", "US")
useYears      <- 1980:2019

theWIDdata.dt <- dl_wid_data(silent = FALSE, cached = FALSE,
  readOnline = TRUE, theAreas = useAreas, theYears = useYears)

theWIDdata.dt <- theWIDdata.dt %>%
  mutate(avgB50d  = avgB50 / (1000 * exchRateUS)) %>%
  mutate(trhB50d  = trhB50 / (1000 * exchRateUS)) %>%
  mutate(avgT10d  = avgT10 / (1000 * exchRateUS)) %>%
  mutate(trhT10d  = trhT10 / (1000 * exchRateUS)) %>%
  mutate(avgNIusd = avgNatlInc / (1000 * exchRateUS)) %>%
  mutate(avgB50c  = shrB50 * avgNIusd / 0.5) %>%
  mutate(avgT10c  = shrT10 * avgNIusd / 0.1) %>%
  mutate(ineqQ    = avgT10c - avgB50c) %>%
  mutate(ineqq    = ineqQ / avgB50c)

theWIDdata.dt %>%
  filter(economy == "US") %>%
  ggplot(., aes(x = year)) +
  geom_line(aes(y = avgB50c), color = "darkred",
            linetype = "solid", size = 1.5) +
  geom_line(aes(y = avgB50), color = "steelblue",
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

