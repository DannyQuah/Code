# @(#) dl-owid-covid-data.R // Our World in Data
# Last-edited: Sun 2021.06.06.2249 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Fri 2021.02.12.1955 -- Danny Quah (me@DannyQuah.com)
#    Refactor with string constants for portability
#  % Sun 2021.01.31.1101 -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to download COVID data from Our World in Data
# ----------------------------------------------------------------
#' Download Our World in Data COVID data
#' Source information at
#' (\url{https://github.com/owid/covid-19-data/tree/master/public/data}).
#' @param blSilent Whether the function should send status messages to
#'     console. Informative as downloading will take some time.
#'     Defaults to \code{FALSE}.
#' @param blCached Whether to download the cached version of the data
#'     from my own GitHub repo instead of retrieving data from the
#'     authorative source. Downloading the cached version is faster.
#'     Defaults to \code{FALSE}.
#' @param blReadOnline Whether to read online or from local disk,
#'     to save network bandwidth. Ignored if blCached is \code{TRUE}.
#'     Defaults to \code{FALSE}.
#' @return Data table
#'
#' @examples
#' tbd
#'
#' @export
dl_owid_covid_data <- function(blCached = FALSE, blReadOnline = FALSE,
                               blSilent = FALSE) {
  if (length(blCached) > 1 || !is.logical(blCached)) {
    stop("'blCached' has to be a single logical value.")
  }
  if (length(blSilent) > 1 || !is.logical(blSilent)) {
    stop("'blSilent' has to be a single logical value.")
  }

# Reminders // Notes
#

# String constants for portability
  strDataName <- "Our World in Data"
# Since you are not me, you'll want to change strLocalRDS and
# strLocalVersion to point to the appropriate locations on
# your local drive
  strLocalRDS <- file.path("~", "0", "Light", "1", "j", "Data-Cloud",
                           "COVID", "owid_covid_data.RDS")
  strLocalVersion <- file.path("~", "0", "Light", "1", "j", "Data-Cloud",
                               "COVID", "owid-covid-data.csv")
#
  strMyOnlineRDS <- "https://raw.githubusercontent.com/DannyQuah/Data-Cloud/blob/main/COVID/owid_covid_data.RDS"
  strOnlineCache <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
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
      if (!blSilent) message("Local disk version of ", strDataName,
                           " data", appendLF = TRUE)
      data_raw <- read.csv(strLocalVersion,
                           stringsAsFactors = FALSE,
                           na.strings = mNAstrings)
    } else {
      if (!blSilent) message ("Downloading ", strDataName, " data...",
                            appendLF = TRUE)
      data_raw <- read.csv(strOnlineCache,
                           stringsAsFactors = FALSE,
                           na.strings = myNAstrings)
    }
    theData.dt <- setDT(data_raw) %>%
      mutate(timestamp = Sys.time())
    remove(data_raw)
    saveRDS(theData.dt, strLocalRDS)
  }

  if (!blSilent) message("Done downloading ", strDataName, " data\n")

# Clean up
  remove (myNAstrings, strOnlineCache, strLocalVersion,
          strMyOnlineRDS, strLocalRDS, strDataName)

  return(theData.dt)
}

# eof dl-owid-covid-data.R

