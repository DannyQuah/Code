#!/usr/bin/env R
# @(#) World-Economies-2021.01.R
# Last-edited: Mon 2021.01.04.0705  -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sun 2021.01.03.1037  -- Danny Quah (me@DannyQuah.com)
#    First draft: R code snippets to set up analysis for
#    economies across the world
# ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(countrycode)
library(imfr)
library(data.table)

##                             countrycode ---------------------
data("codelist", package = "countrycode")
idEconomies.dt <- setDT(codelist)
idEconomies.dt <- idEconomies.dt %>%
	select(country.name.en, iso2c, iso3c, imf, continent, region) %>%
	filter(!is.na(imf))

myEconomiesISO <- countrycode(c("US", "China",
	"UK", "Singapore"),
	origin = "country.name", destination = "iso3c")

# myRegionsISO <- countrycode (c("64", "217", "219"),
#	origin = "imf", destination = "iso3c")

##                             imfr ----------------------------

# General form
#  Look for database IDs ...
  myIMFids <- imf_ids()
#  ... then select a specific IMF database in RHS below
  myDatabaseID <- "IFS"
# Find this database's codelists (or dimensions)
  myListCodelists <- imf_codelist(database_id = myDatabaseID,
								  return_raw = FALSE, times = 3)
# Find codelist codes to use
  myCodelist <- "CL_AREA_IFS"
  myCodes <- imf_codes(codelist = myCodelist, return_raw = FALSE, times = 3)

# Alternatives:
# IFS
  myDatabaseID <- "IFS"
#  CL_INDICATOR_IFS // "NGDP_R_XDC", "NGDP_R_PC_PP_PT"
#  CL_AREA_IFS 
## 

# DOT // Direction of Trade Statistics
  myDatabaseID <- "DOT"

# SNA // System of National Accounts
  myDatabaseID <- "SNA"


# APDREO // Asia Pacific Regional Economic Outlook 
  myDatabaseID <- "APDREO"
#  myCodelist <- "CL_INDICATOR_APDREO", "CL_AREA_APDREO"
# CL_INDICATOR_APDREO // NGDP_RPCH, NGDP_R_PPP_PC_PCH, LUR, All_Indicators
# CL_AREA_APDREO // 1C_229, R4, CN, SG 

# WHDREO // Western Hemisphere Regional Economic Outlook
  myDatabaseID <- "WHDREO"
# CL_INDICATOR_WHDREO // NGDP_RPCH, NGDP_R_PPP_PC_PCH, LUR 
# CL_AREA_WHDREO // 1C_ALLC, A2, A7, A10, US, CA 

# EUDREO // Europe Regional Economic Outlook
# Couldn't find this in imf_ids(); DQ guessed


# MCDREO // Middle East and Central Asia Regional Economic Outlook
  myDatabaseID <- "MCDREO"
# AFRREO / Sub Saharan Africa Regional Economic Outlook 
  myDatabaseID <- "AFRREO"


# followed by these, filling in the RHS of myCodelist with the relevant entry in myCodelist, e.g., CL_AREA_IFS,  
  myListCodelists <- imf_codelist(database_id = myDatabaseID,
								  return_raw = FALSE, times = 3)
  myCodelist <- " ... "
  myCodes <- imf_codes(codelist = myCodelist, return_raw = FALSE, times = 3)

# Illustrate ISO2 code usage, with entities that are not countries
library(imfr)
myAP_all.df <- imf_data("APDREO", indicator = "NGDP_RPCH",
	country = "all",
	freq = "A", start = 2015, end = current_year()
	)

# But the above will not pick up non-country entities like this below
# (denoting "Asia")
myAP_Asia.df <- imf_data("APDREO", indicator = "NGDP_RPCH",
	 country = "1C_229",
	freq = "A", start = 2015, end = current_year()
	)

# Use instead `country = ""`, not `country = "all"`. 
# This usage is undocumented but emerged in 
# my GitHub discussion on issue 29 
# https://github.com/christophergandrud/imfr/issues/29 Sun 2021.01.03.2256


# devtools::install_github('mingjerli/IMFData')
# library(IMFData)

# imfData.df <- DataflowMethod()
# imfData.df$DatabaseID
# imfData.df$DatabaseText

# eof World-Economies-2021.01.R

