#!/usr/bin/env R
# @(#) mng-imf-weo-aggrts.R
# Last-edited: Wed 2021.07.14.2225 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
##  % Wed 2021.07.14.2204 -- Danny Quah (me@DannyQuah.com)
#    This used to be dl-imf-weo-aggrts.R but I am adding other
#    functions beyond just downloading, so it's now munging (or
#    managing) IMF-WEO data
#  % Sun 2021.04.18.1726 -- Danny Quah (me@DannyQuah.com)
#    I had to add last col "Estimates Start After" in WEOApr2021alla.xls
#    (even if all blank), as its absence messes up the code
#    in comparison to, say, WEOOCt2020alla.xls
#  % Mon 2021.04.12.1825 -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to download data from IMF WEO World 
#    Economic Outlook data (from my GitHub repo as needed, since
#    IMF won't, at time of writing, provide an API for this.)
#    Previously inline IMF-WEO-Dynamics-2021.01.R but pulled out for
#    general use
# ----------------------------------------------------------------
dlIMFweoAggrts <- function(blCached = FALSE, blReadOnline = FALSE,
                           blSilent = FALSE, WEOcurrAggrts) {
# Returns list of objects:
#  Econ Groupings - myEconGrpsRefCodes
#                   myWEOeconGrps.dt
#                   myEconGrpsNamesCodes
#'
#' @examples
#' tbd
#' @export
# ----------------------------------------------------------------
  if (length(blCached) > 1 || !is.logical(blCached)) stop(
    "'blCached' has to be a single logical value."
  )
  if (length(blSilent) > 1 || !is.logical(blSilent)) stop(
    "'blSilent' has to be a single logical value."
  )
# Reminders // Notes
# My call conventions given in World-Economies-2021.01.R

################################################################
## Get in the data #############################################
################################################################

# Drawing from IMF WEO database, not available online at
# the usual IMF API. I download the xls from
# https://www.imf.org/en/Publications/WEO/weo-database/$year/$month/download-entire-database
# where $year is replaced by the full 4-digit year (e.g., 2021)
# and $month by the full month (so either April or October),
# and then convert the .xls to .csv (if needed; might just rename).
# For some reason IMF names the tab-delimited csv file an xls;
# this confuses R and other software. Convert tab to comma for csv
# (I use sed but you can use whatever you like.) 
# Now I can use this from my GitHub or local hard disk storage.
# (The unofficial URL, e.g.,
# https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2021/WEOApr2021all.xls
# starts an immediate download rather than online display.)
  strDataName  <- "IMF WEO Aggregates Database"
  myNAstrings        <- c("n/a", "--", "")
  myDataCloudHeader  <- "https://raw.githubusercontent.com/DannyQuah/Data-Cloud/main/"
  WEOcurrAggrtsRDS   <- paste0(WEOcurrAggrts, ".RDS")
  WEOcurrAggrtsCSV   <- paste0(WEOcurrAggrts, ".csv")
# Since you are not me, you'll want to change strLocalRDS and
# strLocalVersion to point to the appropriate locations on
# your local drive
  strLocalRDS <-
    file.path("~", "0", "Light", "1", "j", "Data-Cloud",
              "IMF-WEO", WEOcurrAggrtsRDS)
  strLocalCSV <-
    file.path("~", "0", "Light", "1", "j", "Data-Cloud",
              "IMF-WEO", WEOcurrAggrtsCSV) 
#
  strMyOnlineRDS <-
    paste0("https://raw.githubusercontent.com/DannyQuah/Data-Cloud/master/IMF-WEO/", WEOcurrAggrtsRDS)
#  strOnlineCache <- "https://raw.githubusercontent.com/...

  if (blCached) {
    if (!blSilent) message("Cached version of ", strDataName, " data",
                          appendLF = FALSE)
    if (blReadOnline) {
      if (!blSilent) message(" online.", appendLF=TRUE)
      myWEOaggrts <- readRDS(gzcon(url(strMyOnlineRDS)))
    } else {
      if (!blSilent) message(" local.", appendLF =TRUE)
      myWEOaggrts <- readRDS(strLocalRDS)
    }
    myWEOeconGrps.dt <- myWEOaggrts$myWEOeconGrps.dt
  }
  if (!blCached) {
    if (!blReadOnline) {
      # stop("This isn't available, and shouldn't be needed anyway.")
      myWEOeconGrps.df <-
        read.csv(strLocalCSV, sep = ",",
                 stringsAsFactors = FALSE, na.strings = myNAstrings)
    } else {
      if (!blSilent) message("Downloading ", strDataName, " data...",
                           appendLF = FALSE)
      myWEO.url <- getURL(paste0(myDataCloud.Header,
                                 "IMF-WEO/WEO-Current-Aggrts.csv"))
      myWEOeconGrps.df <-
        read.csv(text = myWEO.url, sep = "\t", stringsAsFactors = FALSE,
                 na.strings = myNAstrings)
    }

# Reference Codebooks
# // different for individual economies and aggregates
    myEconGrpsRefCodes <- myWEOeconGrps.df %>%
     select(WEO.Subject.Code, Subject.Descriptor, Units, Scale,
            Subject.Notes, Country.Series.specific.Notes) %>%
     drop_na(WEO.Subject.Code) %>% distinct()

# Make Country.Group.Name / WEO.Country.Group.Code concordance
# for reference
    myEconGrpsNamesCodes <- myWEOeconGrps.df %>%
     select(Country.Group.Name, WEO.Country.Group.Code) %>%
     drop_na(Country.Group.Name) %>% distinct()

# Individual economies have ISO but economy aggregates (appropriately)
# don't. So keep the identifier WEO.Country.Group.Code
    myWEOeconGrps.dt <- setDT(myWEOeconGrps.df) %>%
     select(-c(Subject.Descriptor, Subject.Notes,
               Country.Series.specific.Notes, Units, Scale)) %>%
    mutate_at(vars(starts_with("X")), as.numeric)

# IMF took out "Estimates Start After" for their Apr 2021 WEO;
# I don't know if this is permanent. So I'm just coding in the
# special case.
    if ("Estimates.Start.After" %in% colnames(myWEOeconGrps.dt)) {
      myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
        select(-c(Estimates.Start.After))
    }

# Reshape into tidy data
    myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
      pivot_longer(cols = starts_with("X"),
                   names_to = "year", names_prefix = "X",
                   names_transform = list(year = as.integer),
                   values_to = "value", values_drop_na = TRUE)
    myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
      pivot_wider(names_from = WEO.Subject.Code, values_from = value)

# Not strictly necessary but easier to scan if we
# sort years within each economy aggregate
    myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
      group_by(WEO.Country.Group.Code) %>%
      arrange(year, .by_group = TRUE) %>%
      ungroup()

# Timestamp before returning
    strTimeNow <- Sys.time()
    myWEOeconGrps.dt <- myWEOeconGrps.dt %>%
      mutate(timestamp = strTimeNow)
    myEconGrpsRefCodes <- myEconGrpsRefCodes %>%
      mutate(timestamp = strTimeNow)
    myEconGrpsNamesCodes <- myEconGrpsNamesCodes %>%
      mutate(timestamp = strTimeNow)

    myWEOaggrts <- list(myWEOeconGrps.dt = myWEOeconGrps.dt,
                        myEconGrpsRefCodes = myEconGrpsRefCodes,
                        myEconGrpsNamesCodes = myEconGrpsNamesCodes)
    saveRDS(myWEOaggrts, strLocalRDS)
  }

  if (!blSilent) message(sprintf("Done: Timestamp %s",
                               myWEOeconGrps.dt$timestamp[1]))
  if (!blSilent) message("Done downloading ", strDataName, " data\n")

  return(myWEOaggrts)
}

# eof mng-imf-weo-aggrts.R

