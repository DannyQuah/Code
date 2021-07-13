# @(#) mng-wid-data.R // World Inequality Database
# Last-edited: Tue 2021.07.13.1830 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Tue 2021.07.13.1741 -- Danny Quah (me@DannyQuah.com)
#    This used to be dl-wid-data.R but I have now added other functions
#    beyond just downloading, so it's now munging (or managing) WID
#    data
#  % Fri 2021.02.12.1817 -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to download World Inequality Database
#    data from GitHub repo
# ----------------------------------------------------------------

# ----------------------------------------------------------------
ntlEntitiesClean <- function(useWID.dt) {
# Clean up WID data:
# - some sensible names
# - useable national entities (not collections; not unusual places);
# - symmetrize currencies
# ----------------------------------------------------------------
# Sensible variable names quickly
  myWID.dt <- useWID.dt %>%
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
  exclEconomies <- c(regionList(subntRegions, myWID.dt),
                     regionList(worldRegions, myWID.dt),
                     xcptEconomies
                    )
  myWID.dt <- myWID.dt %>%
    filter(! theISO2c %in% exclEconomies) %>%
    mutate(econName=countrycode(theISO2c, origin="iso2c",
                                destination="cldr.name.en")) %>%
    relocate(econName, .after=theISO2c) %>%
    mutate(exchRateLCU=1.0)

  return(myWID.dt)

# end ntlEntitiesClean
}

# ----------------------------------------------------------------
regionList <- function(selectRegion, widData.dt) {
# Return vector of namestrings
# selectRegion is one of
#  worldRegions // world regions
#  subntRegions // subnational regions
# At some point I need to read this in from official WID sources
# but for now I'm combining hard-coding this off of "2.2. COUNTRY CODES"
# in
#  https://wid.world/codes-dictionary/#packages
# and extracting off 'economy' in widData.dt
# ----------------------------------------------------------------
# No header files
  worldRegions <- 1
  subntRegions <- 2

  useData.dt <- widData.dt %>% group_by(theISO2c) %>%
    slice(1) %>% select(theISO2c) %>% ungroup()
  if (selectRegion == worldRegions) {
    useData.dt <- useData.dt %>% filter(grepl('-MER', theISO2c))
    theNamesStr <- c("QB", "QD", "QE", "QF", "QJ",
                     "QK", "QM", "QN", "QO", "QP",
                     "QS", "QT", "QU", "QV", "QW",
                     "QX", "QY", "WO", "XA", "XF",
                     "XL", "XM", "XN", "XR",
                     useData.dt[[1]])
  } else if (selectRegion == subntRegions) {
    useData.dt <- useData.dt %>%
      filter(grepl('CN-|DE-|US-', theISO2c))
    theNamesStr <- c(useData.dt [[1]])
  } else {
    stop("regionList: unknown selectRegion", selectRegion)
  }

  return(theNamesStr)

}

# ----------------------------------------------------------------
dl_wid_data <- function(blCached = TRUE, blReadOnline = FALSE,
                        blSilent = FALSE, theAreas = "all",
                        theYears = "all") {
#' Download World Inequality Database data from GitHub repo
#' p0p50, p90p100 - hardwired below because that's the easiest
#' way to slam together the datatables
#' Source information at
#' (\url{https://github.com/WIDworld/wid-r-tool/tree/master/}).
#' @param blCached Whether to download the cached version of the data
#'     from my own GitHub repo instead of retrieving data from the
#'     authorative source. Downloading the cached version is faster.
#'     Defaults to \code{TRUE}.
#' @param blReadOnline Whether to read online or from local disk,
#'     to save network bandwidth. Defaults to \code{FALSE}.
#' @param blSilent Whether the function should send status messages to
#'     console. Informative as downloading will take some time.
#'     Defaults to \code{FALSE}.
#' @return Data table
#'
#' @examples
#' @export
# ----------------------------------------------------------------
  if (length(blSilent) > 1 || !is.logical(blSilent)) stop(
    "'blSilent' has to be a single logical value."
  )
  if (length(blCached) > 1 || !is.logical(blCached)) stop(
    "'blCached' has to be a single logical value."
  )

# Reminders // Notes
# What WID reports as net national income is actually real, i.e.,
# measured in inflation-deflated LCUs.
# The reason for this is the key statement in 2.1.2
# "All monetary amounts are in local currency at constant prices
# for countries and country subregions."
#
# WID's reporting  inyixxx999i is then actually to allow researchers
# to undo what they've done, i.e., if a research wants LCUs in
# nominal values. 

# Pulling their data off in spreadsheet format (using dropdown menus)
# allows setting all data to constant inflation-adjusted Euros,
# even going back past 1999.  
# In contrast, using the direct extraction via the API, that
# denomination is not available, at least not before 1999,
# which is actually the sensible thing, since the Euro did not
# exist before 1999. Not officially anyway. 
# I think what their spreadsheet/dropdown menu interface is
# backward extrapolation using the ECU, not Euros.  In any case,
# I am not using that any longer.

# String constants for portability
  aggrNames <- data.table(
    indicAggr = c("anninc", "xlcusx", "xlceux", "inyixx"), 
    namesAggr = c("avgNatlInc", "exchRateUS", "exchRateEU", "niPrcIndex")
                          )
  distNames <- data.table(
    indicDistr = c("sptinc", "aptinc", "tptinc"),
    namesDistr = c("shr",    "avg",    "trh")
                          )
  percNames <- data.table(
    uP = c("p0p50", "p90p100"),
    nP = c("B50",   "T10")
                          )
  thePercAll     <- "p0p100"
  theAges20P     <- "992"
  theAgesAll     <- "999"
  thePopEqSplit  <- "j"
  thePopIndivs   <- "i"
  strDataName <- "World Inequality Database"
# Since you are not me, you'll want to change strLocalRDS and
# strLocalVersion to point to the appropriate locations on
# your local drive
  strLocalRDS <- file.path("~", "0", "Light", "1", "j", "Data-Cloud",
                           "WID", "wid-data.RDS")
  strLocalVersion <- file.path("~", "0", "Light", "1", "j", "Data-Cloud",
                               "WID", "wid-data.csv")
#
  strMyOnlineRDS <- "https://raw.githubusercontent.com/DannyQuah/Data-Cloud/master/WID/wid-data.RDS"
  strOnlineCache <- "https://raw.githubusercontent.com/widWORLD/data/master/public/data/widWORLD.csv"
  myNAstrings <- c("n/a", "--", "")

  if (blCached) {
    if (!blSilent) message("Cached version of ", strDataName, " data",
                         appendLF = FALSE)
    if (blReadOnline) {
      if (!blSilent) message(" online.", appendLF = TRUE)
      theData.dt <- readRDS(gzcon(url(strMyOnlineRDS)))
    } else {
      if (!blSilent) message(" local.", appendLF = TRUE)
      theData.dt <- readRDS(strLocalRDS)
    }
    if (!blSilent) message(sprintf("Done: Timestamp %s",
                         theData.dt$timestamp[1]))
  }

  if (!blCached) {
    if (!blReadOnline) {
      stop("This isn't available - I have no CSV - but shouldn't be needed anyway.")
      if (!blSilent) message("Local disk version of ", strDataName,
                           " data", appendLF = FALSE)
      data_raw <- read.csv(strLocalVersion,
                           stringsAsFactors = FALSE,
                           na.strings = myNAstrings)
    } else {
      if (!blSilent) message("Downloading ", strDataName, " data...",
                           appendLF = FALSE)
# Distributional data
      for (jIter in 1:length(percNames$uP)) { 
        data_rw0 <- download_wid(
  indicators = distNames$indicDistr,
  areas = theAreas,
  years = theYears,
  perc = percNames$uP[jIter],
  ages = theAges20P,
  pop = thePopEqSplit,
  metadata = FALSE,
  include_extrapolations = TRUE,
  verbose = TRUE
        )
        data_rw0 <- data_rw0 %>%
          select(-percentile) %>%
          spread(variable, value)
        for (jName in 1:length(distNames$indicDistr)) {
          newName <- paste0(distNames$namesDistr[jName],
                           percNames$nP[jIter])
          oldName <- paste0(distNames$indicDistr[jName],
                            theAges20P, thePopEqSplit)
          if (!blSilent) message ("Changing ", oldName, " at ",
                                percNames$uP[jIter], " to ", newName)
#  Can't use %>% rename() here as newName as rename thinks I'm
#  naming the column newName rather than the value of newName
          names(data_rw0)[names(data_rw0) == oldName] <- newName
        }
        if (jIter == 1) {
          data_raw <- data_rw0
        } else {
          data_raw <- merge(data_raw, data_rw0,
                            by = c("country", "year"))
        }
      }
      remove(data_rw0)
#
      if (length(aggrNames$indicAggr) > 1 ||
          aggrNames$indicAggr != "all" &
          aggrNames$indicAggr != "") {
# Aggregate data; bind below
# (cannot take the same arguments in perc so different call) 
        data_agg <- download_wid(
  indicators = aggrNames$indicAggr,
  areas = theAreas,
  years = theYears,
  perc = thePercAll,
  ages = theAgesAll,
  pop  = thePopIndivs,
  metadata = FALSE,
  include_extrapolations = TRUE,
  verbose = TRUE
        )
        data_agg <- data_agg %>%
          select(-percentile) %>%
          spread(variable, value)
        for (jName in 1:length(aggrNames$indicAggr)) {
          newName <- aggrNames$namesAggr[jName]
          oldName <- paste0(aggrNames$indicAggr[jName], theAgesAll,
                             thePopIndivs)
           if (!blSilent) message ("Changing ", oldName, " to ", newName)
#  Can't use %>% rename() here as newName as rename thinks I'm
#  naming the column newName rather than the value of newName
#          names(data_agg)[names(data_agg) == oldName] <- newName
          data_agg <- data_agg %>%
            setnames(old = oldName, new = newName)
        }
#
        data_raw <- merge(data_raw, data_agg,
                          by = c("country", "year"))
        remove(data_agg)
      }
    }
#  Additional cleanups: rename to politically-neutral "economy";
#  sort by years, just to be careful 
    theData.dt <- setDT(data_raw) %>%
      rename("economy" = "country") %>%
      group_by(economy) %>%
      arrange(year, .by_group = TRUE) %>%
      ungroup() %>%
      mutate(timestamp = Sys.time())

    remove(data_raw)
    saveRDS(theData.dt, strLocalRDS)
  }

  if (!blSilent) message("Done downloading ", strDataName, " data\n")

  # Clean up 
  remove (myNAstrings, strOnlineCache, strLocalVersion,
          strMyOnlineRDS, strLocalRDS, strDataName,
          percNames, distNames, aggrNames)

  return(theData.dt)
}


# eof mng-wid-data.R

