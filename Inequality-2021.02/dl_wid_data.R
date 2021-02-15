# @(#) dl_wid_data.R // World Inequality Database
# Last-edited: Sun 2021.02.14.1745-- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Fri 2021.02.12.1817 -- Danny Quah (me@DannyQuah.com)
#    First draft: R script to download World Inequality Database
#    data from GitHub repo
# ----------------------------------------------------------------
#' Download World Inequality Database data from GitHub repo
#' p0p50, p90p100 - hardwired below because that's the easiest
#' way to slam together the datatables
#' Source information at
#' (\url{https://github.com/WIDworld/wid-r-tool/tree/master/}).
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
dl_wid_data <- function(silent = FALSE, cached = FASLE, readOnline = FALSE,
  theAreas = "all", theYears = "all") {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' has to be a single logical value."
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' has to be a single logical value."
  )

# String constants for portability
  aggrNames <- data.table(
                          indicAggr = c("anninc",     "xlcusx"),
                          namesAggr = c("avgNatlInc", "exchRateUS")
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
  strLocalRDS <- file.path("~", "0", "Light", "1", "j", "Data-Cloud", "WID", "wid_data.RDS")
  strMyOnlineRDS <- "https://raw.githubusercontent.com/DannyQuah/Data-Cloud/blob/main/WID/wid_data.RDS"
  strLocalVersion <- file.path("~", "0", "Light", "1", "j", "Data-Cloud", "WID", "wid_data.csv")
  strOnlineCache <- "nttps://raw.githubusercontent.com/widWORLD/data/master/public/data/widWORLD.csv"
  myNAstrings <- c("n/a", "--", "")

  if (cached) {
    if (!silent) message("Cached version of ", strDataName, " data",
                         appendLF = FALSE)
    if (readOnline) {
      if (!silent) message (" online.", appendLF = FALSE)
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
                           na.strings = myNAstrings)
    } else {
      if (!silent) message ("Downloading ", strDataName, " data...",
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
          if (!silent) message ("Changing ", oldName, " at ",
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
           if (!silent) message ("Changing ", oldName, " to ", newName)
#  Can't use %>% rename() here as newName as rename thinks I'm
#  naming the column newName rather than the value of newName
          names(data_agg)[names(data_agg) == oldName] <- newName
        }
#
        data_raw <- merge(data_raw, data_agg,
                          by = c("country", "year"))
        remove(data_agg)
      }
    }
    theData.dt <- setDT(data_raw) %>%
      rename("economy" = "country") %>%
      mutate(timestamp = Sys.time())

    remove(data_raw)
    saveRDS(theData.dt, strLocalRDS)
  }

  if (!silent) message("Done downloading ", strDataName, " data\n")

  # Clean up 
  remove (myNAstrings, strOnlineCache, strLocalVersion,
          strMyOnlineRDS, strLocalRDS, strDataName,
          percNames, distNames, aggrNames)

  return(theData.dt)
}


# eof dl_wid_data.R

