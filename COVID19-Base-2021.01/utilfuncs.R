#!/usr/bin/env R
# @(#) utilfuncs.R
# Last-edited: Mon 2021.05.24.1755 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sat 2021.05.22.1435 -- Danny Quah (me@DannyQuah.com)
#    First draft: Utility functions. Output. Calculate
# ----------------------------------------------------------------

# ----------------------------------------------------------------
showDailyGraph <- function(theData.dt, theVrbl, theEcons,
                           theDateEndPts) {
# For vector of economies, timeseries plot a daily indicator
# variable
# ----------------------------------------------------------------
  beginDate <- theDateEndPts[1]
  endDate   <- theDateEndPts[2]
  if (beginDate > endDate)
    stop("showDailyGraph: Date endpoints misconfigured.")
  plotThis <- sym(theVrbl)
  myPlot   <- theData.dt %>%
    filter(theISO3c %in% theEcons) %>%
    filter(theDate >= beginDate & theDate <= endDate) %>%
    ggplot(., aes(x=theDate, y=!!plotThis)) +
    geom_line(aes(group=theISO3c, color=theISO3c), linetype="solid",
              size=1.2) +
    labs(title=theVrbl, x="", y="") +
    theme_economist(base_size=13)
  print(myPlot)

# end of showDailyGraph
}

# ----------------------------------------------------------------
showAnnualGraph <- function(theData.dt, theVrbl, theEcons,
                            theYearEndPts) {
# ----------------------------------------------------------------
# For vector of economies, timeseries plot an annual indicator
# variable
# ----------------------------------------------------------------
  beginYear <- theYearEndPts[1]
  endYear   <- theYearEndPts[2]
  if (beginYear > endYear)
    stop("ShowAnnualGraph: Year endpoints misconfigured.")
  plotThis <- sym(theVrbl)
  myPlot   <- theData.dt %>%
    filter(theISO3c %in% theEcons) %>%
    filter(theYear >= beginYear & theYear <= endYear) %>%
    ggplot(., aes(x=theYear, y=!!plotThis)) +
    geom_line(aes(group=theISO3c, color=theISO3c), linetype="solid",
              size=1.2) +
    scale_x_continuous(name="", breaks=seq(beginYear, endYear, 2)) +
    labs(title=theVrbl, y="") +
    theme_economist(base_size=13)
  print(myPlot)

# end of showAnnualGraph
}

crossShow <- function(covidInd.dt, theCovidInd, econsInd.dt, theEconInd,
                      plotEcons = NULL, labelEcons = NULL) {
# ----------------------------------------------------------------
# For vector of economies, cross-section plot COVID and economic
# indicators. Return cross-section datatable.
# If vector plotEcons is NULL (the default), then 
# use all available economies;
# otherwise, restrict selection of economies to plotEcons.
# If labelEcons is !NULL, label those economies in the plot.
# Trim sample at econPerf \in [-10.0, +1.5] - otherwise Guyana
# problem
# ----------------------------------------------------------------
  econPerfMin <- -10.0
  econPerfMax <- 1.5
  yLogged     <- TRUE

  covidIndVrbl <- sym(theCovidInd)
  theSumm.dt <- covidInd.dt %>%
    select(theISO3c, theDate, !!covidIndVrbl) %>%
    group_by(theISO3c) %>%
    summarize(covidPerf=last(!!covidIndVrbl)) %>%
    ungroup() %>%
    mutate(covidPerf = 1000.0/covidPerf) %>%
    select(theISO3c, covidPerf)

  if (length(plotEcons)>0) {
    theSumm.dt <- theSumm.dt %>%
      filter(theISO3c %in% plotEcons)
  }

  econsVrbl      <- sym(theEconInd)
  econsSpanYears <- c(2020:2021)
  econsBaseYears <- c(2016:2019)
  theHold.dt <-econsInd.dt %>%
    filter(theYear %in% econsSpanYears) %>%
    select(theISO3c, theYear, !!econsVrbl) %>%
    group_by(theISO3c) %>%
    summarize(econPerf=mean(!!econsVrbl)) %>%
    ungroup()
  theHol1.dt <- econsInd.dt %>%
    filter(theYear %in% econsBaseYears) %>%
    select(theISO3c, theYear, !!econsVrbl) %>%
    group_by(theISO3c) %>%
    summarize(econBase=mean(!!econsVrbl)) %>%
    ungroup()

  theHold.dt <- theHold.dt %>%
    left_join(theHol1.dt, by="theISO3c") %>%
    mutate(econPerf=econPerf - econBase) %>%
    select(theISO3c, econPerf)
    
  theSumm.dt <- theSumm.dt %>%
    left_join(theHold.dt, by="theISO3c") %>%
    filter(econPerf >= econPerfMin & econPerf <= econPerfMax) 

  myPlot <- theSumm.dt %>%
    ggplot(., aes(x=econPerf, y=covidPerf)) +
    labs(x="Economic performance", y="COVID-19 performance",
         title=today()) +
    geom_point(color="black", size=1.5)
  if (yLogged) {
    myPlot <- myPlot +
    scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
                  labels=trans_format("log10", math_format(10^.x)))
  }
  if (length(plotEcons) <= 0) {
    myPlot <- myPlot + geom_smooth(method=loess)
  }
  if (length(labelEcons) > 0) {
    myPlot <- myPlot +
    geom_label_repel(data = subset(theSumm.dt, theISO3c %in% labelEcons),
                     aes(label=theISO3c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both"
                     )
  }
  myPlot <- myPlot + theme_economist(base_size=14)
  print(myPlot)

  return(theSumm.dt)

# end of crossShow
}


# eof utilfuncs.R

