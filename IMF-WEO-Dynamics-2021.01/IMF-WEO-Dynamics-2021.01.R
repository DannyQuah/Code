#!/usr/bin/env R
# @(#) IMF-WEO-Dynamics-2021.01.R
# Last-edited: Sun 2021.01.31.1905  -- Danny Quah (me@DannyQuah.com)
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

# My call conventions given in World-Economies-2021.01.R

################################################################
## Get in the data #############################################
################################################################

# Drawing from IMF WEO database, not available online at
# the usual IMF API. I downloaded the xls from
# https://www.imf.org/en/Publications/WEO/weo-database/2020/October/download-entire-database
# and converted the .xls to .csv. Now I can use them from my
# GitHub or locally
readOnline <- FALSE
if (!readOnline) {
# If local ####################################################
  localDataPath <- file.path("~", "0", "Light", "1", "j", "Data-Cloud", "IMF-WEO")
  myWEOlocalFile <- file.path(localDataPath, "WEOOct2020all.csv")
  myWEOeconomies.df <- read.csv(myWEOlocalFile,
	 stringsAsFactors = FALSE, na.strings = c("n/a", "--", ""))
  myWEOlocalFile <- file.path(localDataPath, "WEOOct2020alla.csv")
  myWEOeconGrps.df <- read.csv(myWEOlocalFile,
	 stringsAsFactors = FALSE, na.strings = c("n/a", "--", ""))
} else {
# If online ####################################################
  myDataCloudHeader <- "https://raw.githubusercontent.com/DannyQuah/Data-Cloud/main/"
  myWEO.url <- getURL(paste0(myDataCloudHeader, "IMF-WEO/WEOOct2020all.csv"))
  myWEOeconomies.df <- read.csv(text = myWEO.url,
	 stringsAsFactors = FALSE, na.strings = c("n/a", "--", ""))
  myWEO.url <- getURL(paste0(myDataCloudHeader, "IMF-WEO/WEOOct2020alla.csv"))
  myWEOeconGrps.df <- read.csv(text = myWEO.url,
	 stringsAsFactors = FALSE, na.strings = c("n/a", "--", ""))
}

# Reference Codebooks
# // different for single economies and for economy groupings 
myEconomiesRefCodes <- myWEOeconomies.df %>% select(WEO.Subject.Code,
		Subject.Descriptor, Units, Scale, Subject.Notes) %>% 
	drop_na(WEO.Subject.Code) %>% distinct()
# Not the same for economy groupings
myEconGrpsRefCodes <- myWEOeconGrps.df %>% select(WEO.Subject.Code,
		Subject.Descriptor, Units, Scale, Subject.Notes,
		Country.Series.specific.Notes) %>%
	drop_na(WEO.Subject.Code) %>% distinct()

# Make Country.Group.Name / WEO.Country.Group.Code concordance
# for reference as well
myEconGrpsNamesCodes <- myWEOeconGrps.df %>%
	select(Country.Group.Name, WEO.Country.Group.Code) %>%
	drop_na(Country.Group.Name) %>% distinct()

# Clean data, move into data tables
# Take out Units, Scale, Estimates.Start.After as well,
# as otherwise pivot_wider will use those as additional 
# way to categorise the observations
myWEOeconomies.dt <- setDT(myWEOeconomies.df) %>%
	select(-c(WEO.Country.Code, Subject.Descriptor, Subject.Notes,
			   Country.Series.specific.Notes, Units, Scale,
			   Estimates.Start.After)) %>%
	mutate_at(vars(starts_with("X")), as.numeric)

# Economies have ISO but economic groups (appropriately enough) don't.
# So need to keep the identifier WEO.Country.Group.Code
myWEOeconGrps.dt <- setDT(myWEOeconGrps.df) %>%
	select(-c(Subject.Descriptor, Subject.Notes,
			  Country.Series.specific.Notes, Units, Scale,
			  Estimates.Start.After)) %>%
	mutate_at(vars(starts_with("X")), as.numeric)

# Reshape into tidy data
myWEOeconomies.dt <- myWEOeconomies.dt %>%
	pivot_longer(cols = starts_with("X"),
		names_to = "year", names_prefix = "X",
		names_transform = list(year = as.integer),
		values_to = "value", values_drop_na = TRUE)

myWEOeconomies.dt <- myWEOeconomies.dt %>% 
	pivot_wider(names_from = WEO.Subject.Code, values_from = value)

myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
	pivot_longer(cols = starts_with("X"),
		names_to = "year", names_prefix = "X",
		names_transform = list(year = as.integer),
		values_to = "value", values_drop_na = TRUE)

myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
	pivot_wider(names_from = WEO.Subject.Code, values_from = value)

## Not strictly necessary, but easier to scan
## sort years within economies 
myWEOeconomies.dt <- myWEOeconomies.dt %>%
	group_by(ISO) %>% arrange(year, .by_group = TRUE)

myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
	group_by(WEO.Country.Group.Code) %>% arrange(year, .by_group = TRUE)

## Output selected groups and economies


################################################################
#  Selected groups of economies
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

