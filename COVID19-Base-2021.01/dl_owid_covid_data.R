# @(#) dl_owid_covid_data.R // Our World in Data
# Last-edited: Fri 2021.02.12.2306 -- Danny Quah (me@DannyQuah.com)
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
#' @param silent Whether the function should send status messages to
#'     console. Informative as downloading will take some time.
#'     Defaults to \code{TRUE}.
#' @param cached Whether to download the cached version of the data
#'     from my own GitHub repo instead of retrieving data from the
#'     authorative source. Downloading the cached version is faster.
#'     Defaults to \code{FALSE}.
#' @param readOnline Whether to read online or from local disk,
#'     to save network bandwidth. Ignored if cached is \code{TRUE}.
#'     Defaults to \code{FALSE}.
#' @return A data table containing the data.
#'
#' @examples
#' tbd
#'
#' @export
dl_owid_covid_data <- function(silent = FALSE, cached = FALSE, readOnline = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' has to be a single logical value."
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' has to be a single logical value."
  )

# String constants for portability
  strDataName <- "Our World in Data"
  strLocalRDS <- file.path("..", "..", "Data-Cloud", "COVID",
                           "owid_covid_data.RDS")
  strMyOnlineRDS <- "https://raw.githubusercontent.com/DannyQuah/Data-Cloud/blob/main/COVID/owid_covid_data.RDS"
  strLocalVersion <- file.path("~", "0", "Light", "1", "j", "Data-Cloud", "COVID", "owid-covid-data.csv")
  strOnlineCache <- "nttps://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
  myNAstrings <- c("n/a", "--", "")

  if (cached) {
    if (!silent) message("Cached version of ", strDataName, " data",
                         appendLF = FALSE)
    if (readOnline) {
      if (!silent) message(" online.", appendLF = FALSE)
      theData.dt <- readRDS(gzcon(url(strMyOnlineRDS)))
    } else {
        if (!silent) message(" local.", appendLF = FALSE)
        theData.dt <- readRDS(strLocalRDS)
    }
    if (!silent) message(sprintf(" Done: Timestamp %s",
                         theData.dt$timestamp[1]))
  }

  if (!cached) {
    if (!readOnline) {
      if (!silent) message("Local disk version of ", strDataName,
                           " data", appendLF = FALSE)
      data_raw <- read.csv(strLocalVersion,
                           stringsAsFactors = FALSE,
                           na.strings = mNAstrings)
    } else {
      if (!silent) message ("Downloading ", strDataName, " data...",
                            appendLF = FALSE)
      data_raw <- read.csv(strOnlineCache,
        stringsAsFactors = FALSE, na.strings = myNAstrings)
    }
    theData.dt <- setDT(data_raw) %>%
      mutate(timestamp = Sys.time())
    remove(data_raw)
    saveRDS(theData.dt, strLocalRDS)
  }

  if (!silent) {
    message("Done downloading ", strDataName, " data\n")
#    data_info("owid_covid_data")
  }

# Clean up
  remove (myNAstrings, strOnlineCache, strLocalVersion,
          strMyOnlineRDS, strLocalRDS, strDataName)

  return(theData.dt)
}

# eof dl_owid_covid_data.R

