#!/usr/bin/env R
# @(#) IMF-WEO-imf-gen-2021.01.R
# Last-edited: Mon 2021.04.12.1721  -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Fri 2021.01.01.1836  -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to work with IMF data, WEO piecing together
#    from general IMF databases
# ----------------------------------------------------------------
library(RCurl)
library(data.table)
library(lubridate)
library(countrycode)
library(imfr)
library(tidyverse)

# My call conventions given in World-Economies-2021.01.R

################################################################
## Get in the data #############################################
################################################################

# Piecing together from general IMF databases
myStartYear <- 2015
myREOindicators <- c("NGDP_RPCH") 
myAPeconomies <- c("1C_ALLC", "1C_229", "S2", "1C_9502", "R4",
				   "CN", "JP", "SG")
myWHeconomies <- c("1C_ALLC", "A10", "A2", "A7", "US", "CA")

myDatabaseID <- "APDREO"
imf_reo.dt <- imf_data(database_id = myDatabaseID,
	indicator = myREOindicators, country = myAPeconomies,
	freq = "A", start = myStartYear, end = current_year()
	)
imf_reo.dt <- setDT(imf_reo.dt)
imf_reo.dt[, Region := myDatabaseID]

myDatabaseID <- "WHDREO"
tmp_reo.dt <- imf_data(database_id = myDatabaseID,
	indicator = myREOindicators, country = myWHeconomies,
	freq = "A", start = myStartYear, end = current_year()
	)
tmp_reo.dt <- setDT(tmp_reo.dt)
tmp_reo.dt[, Region := myDatabaseID]

imf_reo.dt <- bind_rows(imf_reo.dt, tmp_reo.dt)

# eof IMF-WEO-imf-gen-2021.01.R

