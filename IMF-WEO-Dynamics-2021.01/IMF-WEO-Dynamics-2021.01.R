#!/usr/bin/env R
# @(#) IMF-WEO-Dynamics-2021.01.R
# Last-edited: Sun 2021.04.18.2024  -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Fri 2021.01.01.1836  -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to work with IMF World Economic Outlook
#    not reachable apparently from the JSON api and imfr
# ----------------------------------------------------------------
library(RCurl)
library(data.table)
library(lubridate)
library(countrycode)
library(imfr)
library(tidyverse)
library(zoo)
library(ggthemes)

source("./dl-imf-weo-aggrts.R", echo = FALSE)
source("./dl-imf-weo-indivs.R", echo = FALSE)

# My call conventions given in World-Economies-2021.01.R
################################################################
## Get in the data #############################################
################################################################
myWEOaggrts <- dlIMFweoAggrts(WEOcurrAggrts = "WEOApr2021alla",
                              silent = FALSE, cached = TRUE,
                              readOnline = FALSE)
myWEOindivs <- dlIMFweoIndivs(WEOcurrIndivs = "WEOApr2021all",
                              silent = FALSE, cached = TRUE,
                              readOnline = FALSE)

myWEOeconGrps.dt     <- myWEOaggrts$myWEOeconGrps.dt
myEconGrpsNamesCodes <- myWEOaggrts$myEconGrpsNamesCodes
myWEOeconomies.dt    <- myWEOindivs$myWEOeconomies.dt

myEconGrpsRefCodes   <- myWEOaggrts$myEconGrpsRefCodes
myEconomiesRefCodes  <- myWEOindivs$myEconomiesRefCodes
# These last are not used in numerical calculation but,
# for reference, they contain descriptions of the
# different variables otherwise identified only by
# WEO.Subject.Code like NGDP_RPCH, NGDPD, PPPGDP

## Output selected groups and economies

################################################################
#  Selected groups of economies
#  G7 - Canada, France, Germany, Italy, Japan, UK, US
################################################################
myWEOeconGroups <- c("World", "Advanced economies",
	"Major advanced economies (G7)",
	"ASEAN-5", "European Union",
	"Emerging market and developing economies")
myWEOeconGroupCodes <- myEconGrpsNamesCodes %>%
	filter(Country.Group.Name %in% myWEOeconGroups) %>%
	select(WEO.Country.Group.Code)
myWEOeconGroupCodes <- as.numeric(unlist(myWEOeconGroupCodes))
myWEOeconGrps.dt %>%
	filter(WEO.Country.Group.Code %in% myWEOeconGroupCodes) %>%
	select(Country.Group.Name, WEO.Country.Group.Code, year, NGDP_RPCH) %>%
	ggplot(., aes(x = year, y = NGDP_RPCH)) +
	geom_line(aes(group = Country.Group.Name,
				  colour = Country.Group.Name)) +
	geom_point() +
	labs(title = "Economic growth", y = "%") + 
	theme_economist(base_size = 14)
myWEOeconGrpsForecast.dt <- myWEOeconGrps.dt %>%
	filter(WEO.Country.Group.Code %in% myWEOeconGroupCodes) %>% 
	filter(year >= 2018) %>%
	select(Country.Group.Name, WEO.Country.Group.Code, year, NGDP_RPCH)
myWEOeGPast.dt <- myWEOeconGrps.dt %>%
	filter(WEO.Country.Group.Code %in% myWEOeconGroupCodes) %>%
	filter(year >= 2010 & year <= 2019) %>%
	select(Country.Group.Name, WEO.Country.Group.Code, year, NGDP_RPCH) %>%
	group_by(Country.Group.Name) %>%
	summarise(pastDecGr = mean(NGDP_RPCH, na.rm = TRUE))

################################################################
#  Update the Globalisation Lift (DQ, FDKM)
#  NGDPD  - GDP, current prices USD billions
#  PPPGDP - GDP, current prices PPP International Dollars billions 
################################################################
myWEOeconGroups <- c("Major advanced economies (G7)"
                     "Emerging market and developing economies")
myWEOeconGroupCodes <- myEconGrpsNamesCodes %>%
  filter(Country.Group.Name %in% myWEOeconGroups) %>%
  select(WEO.Country.Group.Code)
myWEOeconGroupCodes <- as.numeric(unlist(myWEOeconGroupCodes))
myWEOeconGrps.dt %>%
  filter(WEO.Country.Group.Code %in% myWEOeconGroupCodes) %>%
  select(Country.Group.Name, WEO.Country.Group.Code, year, NGDPD) %>%
  ggplot(., aes(x = year, y = NGDPD) +
         geom_line(aes(group = Country.Group.Name,
                       colour = Country.Group.Name)) +
         geom_point() +
         labs(title = "GDP", y = "Bn$") +
         theme_economist(base_size = 14)

blocNations.dt <- myWEOeconGrps.dt %>%


################################################################
#  Selected economies
################################################################
myWEOeconISO <- c("USA", "CHN", "JPN", "AUS", "NZL", "SGP")
myWEOeconomies.dt %>%
	filter(ISO %in% myWEOeconISO) %>%
	select(Country, ISO, year, NGDP_RPCH) %>%
	ggplot(., aes(x = year, y = NGDP_RPCH)) +
	geom_line(aes(group = Country, colour = Country)) +
	geom_point() +
	labs(title = "Economic growth", y = "%") +
	theme_economist(base_size = 14)
myWEOeconomiesForecast.dt <- myWEOeconomies.dt %>%
	filter(ISO %in% myWEOeconISO) %>%
	filter(year >= 2018) %>%
	select(Country, ISO, year, NGDP_RPCH)
myWEOecPast.dt <- myWEOeconomies.dt %>%
	filter(ISO %in% myWEOeconISO) %>%
	filter(year >= 2010 & year <= 2019) %>%
	select(Country, ISO, year, NGDP_RPCH) %>%
	group_by(Country) %>%
	summarise(pastDecGr = mean(NGDP_RPCH, na.rm = TRUE))
################################################################


# eof IMF-WEO-Dynamics-2021.01.R

