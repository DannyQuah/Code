#!/usr/bin/env R
# @(#) mng-imf-weo-indivs.R
# Last-edited: Wed 2021.07.14.2225 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
##  % Wed 2021.07.14.2204 -- Danny Quah (me@DannyQuah.com)
#    This used to be dl-imf-weo-indivs.R but I am adding other
#    functions beyond just downloading, so it's now munging (or
#    managing) IMF-WEO data
#  % Mon 2021.04.12.1825 -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to download data from IMF WEO World 
#    Economic Outlook data (from my GitHub repo as needed, since
#    IMF won't, at time of writing, provide an API for this.)
#    Previously inline IMF-WEO-Dynamics-2021.01.R but pulled out for
#    general use
# ----------------------------------------------------------------
dlIMFweoIndivs <- function(blCached=FALSE, blReadOnline=FALSE,
                           blSilent=FALSE, WEOcurrIndivs) {
# Returns list of objects:
#  Economies      - myEconomiesRefCodes
#                   myWEOeconomies.dt
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
  strDataName  <- "IMF WEO Individual Economies Database"
  myNAstrings        <- c("n/a", "--", "")
  myDataCloudHeader  <- "https://raw.githubusercontent.com/DannyQuah/Data-Cloud/main/"
  WEOcurrIndivsRDS   <- paste0(WEOcurrIndivs, ".RDS")
  WEOcurrIndivsCSV   <- paste0(WEOcurrIndivs, ".csv")
# Since you are not me, you'll want to change strLocalRDS and
# strLocalVersion to point to the appropriate locations on
# your local drive
  strLocalRDS <-
    file.path("~", "0", "Light", "1", "j", "Data-Cloud",
              "IMF-WEO", WEOcurrIndivsRDS)
  strLocalCSV <-
    file.path("~", "0", "Light", "1", "j", "Data-Cloud",
              "IMF-WEO", WEOcurrIndivsCSV) 
#
  strMyOnlineRDS <-
    paste0("https://raw.githubusercontent.com/DannyQuah/Data-Cloud/master/IMF-WEO/", WEOcurrIndivsRDS)
#  strOnlineCache <- "https://raw.githubusercontent.com/...

  if (blCached) {
    if (!blSilent) message("Cached version of ", strDataName, " data",
                          appendLF=FALSE)
    if (blReadOnline) {
      if (!blSilent) message(" online.", appendLF=TRUE)
      myWEOindivs <- readRDS(gzcon(url(strMyOnlineRDS)))
    } else {
        if (!blSilent) message(" local.", appendLF=TRUE)
        myWEOindivs <- readRDS(strLocalRDS)
    }
    myWEOeconomies.dt <- myWEOindivs$myWEOeconomies.dt
  }
  if (!blCached) {
    if (!blReadOnline) {
      # stop("This isn't available, and shouldn't be needed anyway.")
      myWEOeconomies.df <-
        read.csv(strLocalCSV, sep=",",
                 stringsAsFactors=FALSE, na.strings=myNAstrings)
    } else {
      if (!blSilent) message("Downloading ", strDataName, " data...",
                           appendLF=TRUE)
      myWEO.url <- getURL(paste0(myDataCloud.Header,
                                 "IMF-WEO/WEO-Current-Indivs.csv"))
      myWEOeconomies.df <-
        read.csv(text=myWEO.url, sep="\t", stringsAsFactors=FALSE,
                 na.strings=myNAstrings)
    }

# Reference Codebooks
# // different for individual economies and aggregates
    myEconomiesRefCodes <- myWEOeconomies.df %>%
      select(WEO.Subject.Code, Subject.Descriptor, Units, Scale,
             Subject.Notes) %>%
      drop_na(WEO.Subject.Code) %>% distinct()

# Clean data, move into data tables
# Take out Units, Scale, Estimates.Start.After as well,
# as otherwise pivot_wider will use those as additional
# way to categorise the observations
    myWEOeconomies.dt <- setDT(myWEOeconomies.df) %>%
      select(-c(WEO.Country.Code, Subject.Descriptor, Subject.Notes,
                Country.Series.specific.Notes, Units, Scale,
                Estimates.Start.After)) %>%
      mutate_at(vars(starts_with("X")), as.numeric)

# Reshape into tidy data
    myWEOeconomies.dt <- myWEOeconomies.dt %>%
      pivot_longer(cols=starts_with("X"),
                   names_to="year", names_prefix="X",
                   names_transform=list(year=as.integer),
                   values_to="value", values_drop_na=TRUE)
    myWEOeconomies.dt <- myWEOeconomies.dt %>%
      pivot_wider(names_from=WEO.Subject.Code, values_from=value)

# Not strictly necessary but easier to scan if we
# sort years within each economy aggregate
    myWEOeconomies.dt <- myWEOeconomies.dt %>%
      group_by(ISO) %>% arrange(year, .by_group=TRUE) %>%
      ungroup()

# Timestamp before returning
    strTimeNow <- Sys.time()
    myWEOeconomies.dt <- myWEOeconomies.dt %>%
      mutate(timestamp=strTimeNow)
    myEconomiesRefCodes <- myEconomiesRefCodes %>%
      mutate(timestamp=strTimeNow)

    myWEOindivs <- list(myWEOeconomies.dt=myWEOeconomies.dt,
                        myEconomiesRefCodes=myEconomiesRefCodes)
    saveRDS(myWEOindivs, strLocalRDS)
  }
  if (!blSilent) message(sprintf("Done: Timestamp %s",
                               myWEOeconomies.dt$timestamp[1]))
  if (!blSilent) message("Done downloading ", strDataName, " data\n")

  return(myWEOindivs)

# end of dlIMFweoIndivs 
}

# eof mng-imf-weo-indivs.R

