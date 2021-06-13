#!/usr/bin/env R
# @(#) utilfuncs.R
# Last-edited: Sat 2021.06.12.2225 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sat 2021.05.22.1435 -- Danny Quah (me@DannyQuah.com)
#    First draft: Utility functions. Output. Calculate
#
# ----------------------------------------------------------------

# ----------------------------------------------------------------
crossBuildPerfs <- function(covidInd.dt, theCovidInd,
                             econsInd.dt, theEconInd) {
# ----------------------------------------------------------------
# Build and return cross section of performance indicators.
# Prune cross-section sample
#  - by lastDate, theCovidInd (total cases or deaths per mn)
#      >= 0.50 (Burundi BDI at 0.505, Laos LAO 0.41, VNM 0.54)
#  - must be in both covidInd.dt and econsInd.dt
#  - total population in 2020 >= minPopln 1mn
# ----------------------------------------------------------------

# Latest day on COVID indicator; COVID performance is
# reciprocal of deaths/cases per thousand on that day
  covidIndVrbl <- sym(theCovidInd)
  minCovidInd  <- 0.50 # min for COVID indicator to include

  theSumm.dt <- covidInd.dt %>%
    select(theISO3c, theDate, !!covidIndVrbl) %>%
    group_by(theISO3c) %>%
    filter(!is.na(!!covidIndVrbl)) %>%
    arrange(theDate) %>% slice_tail() %>%
    ungroup() %>%
    filter(!!covidIndVrbl >= minCovidInd) %>%
    mutate(covidPerf=1000.0/!!covidIndVrbl) %>%
    rename(lastDate=theDate) %>%
    select(theISO3c, lastDate, covidPerf)

# Economic performance is how much growth changed over the course of
# the pandemic compared to growth before the pandemic.
# Take the course of the pandemic to be 2020:2021 and before to be
# 2016:2019
  econsVrbl      <- sym(theEconInd)
  econsSpanYears <- c(2020:2021)
  econsBaseYears <- c(2016:2019)
  minPopln       <- 1.0  # LP is measured in million; min threshold

  theHold.dt <- econsInd.dt %>%
    group_by(theISO3c) %>%
    mutate(maxPopln=max(LP, na.rm=TRUE)) %>%
    ungroup() %>%
    filter(theYear %in% econsSpanYears) %>%
    select(theISO3c, theYear, maxPopln, !!econsVrbl) %>%
    group_by(theISO3c) %>%
    mutate(econPerf=mean(!!econsVrbl)) %>%
    slice_tail() %>%
    ungroup() %>%
    filter(maxPopln >= minPopln) %>%
    select(theISO3c, econPerf)

  theHol1.dt <- econsInd.dt %>%
    filter(theYear %in% econsBaseYears) %>%
    select(theISO3c, theYear, !!econsVrbl) %>%
    group_by(theISO3c) %>%
    summarize(econBase=mean(!!econsVrbl)) %>%
    ungroup()

  theHold.dt     <- theHold.dt %>%
    left_join(theHol1.dt, by="theISO3c") %>%
    mutate(econPerf=econPerf-econBase) %>%
    select(theISO3c, econPerf)

# Join the two datatables, COVID, IMF; add friendlyname for
# ISO3c code
  theSumm.dt <- theSumm.dt %>%
    left_join(theHold.dt, by="theISO3c") %>%
    filter(complete.cases(.)) %>%
    mutate(econName=countrycode(theISO3c, origin="iso3c",
                                destination="cldr.name.en")) %>%
    relocate(econName, .before=theISO3c)

  return(theSumm.dt)

# end of crossBuildPerfs
}

# ----------------------------------------------------------------
crossShow <- function(theSumm.dt, onlyTheseEcons = NULL,
                      labelEcons = NULL) {
# ----------------------------------------------------------------
# For vector of economies, cross-section plot COVID and economic
# indicators.  Return fitted values
# If vector onlyTheseEcons is NULL (the default), then 
# use all available economies;
# otherwise, restrict selection of economies to onlyTheseEcons
# If labelEcons is !NULL, label those economies in the plot.
# Trim sample at econPerf \in [-10.0, +1.5] - This ends up
# excluding
# worse than -10 (Lebanon)
# better than 1.5 (Congo Brazzaville, Venezuela, South Sudan,
#                  Equatorial Guinea, Libya)
# ----------------------------------------------------------------
  econPerfMin <- -10.0
  econPerfMax <- 1.5
  blLogged    <- TRUE # plot log axis the non-economic variable
  useSumm.dt  <- theSumm.dt


  if (length(onlyTheseEcons)>0) {
    useSumm.dt <- useSumm.dt %>%
      filter(theISO3c %in% onlyTheseEcons)
  }

  useSumm.dt <- useSumm.dt %>%
    filter(econPerf >= econPerfMin & econPerf <= econPerfMax)

  tmp.dt <- useSumm.dt %>%
    arrange(lastDate) %>% slice_head()
  datePlot <- tmp.dt$lastDate
  remove(tmp.dt)

## First economics on the horizontal
  myPlot <- useSumm.dt %>%
    ggplot() +
    aes(x=econPerf, y=covidPerf) +
    labs(x="Economic performance", y="COVID-19 performance",
         title=datePlot) +
    geom_point(color="black", size=1.5)
  if (blLogged) {
    myPlot <- myPlot +
      scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
                    labels=trans_format("log10", math_format(10^.x)))
  }

  if (length(onlyTheseEcons) <= 0) {
    myPlot <- myPlot + geom_smooth(method=loess)
  }
  if (length(labelEcons) > 0) {
    myPlot <- myPlot +
    geom_label_repel(data = subset(useSumm.dt, theISO3c %in% labelEcons),
                     aes(label=theISO3c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both",
                     max.overlaps=15
                     )
  }
  myPlot <- myPlot + theme_economist(base_size=14)
  print(myPlot)
  remove(myPlot)



## Then COVID on the horizontal
  myPlot <- useSumm.dt %>%
    ggplot() +
    aes(x=covidPerf, y=econPerf) +
    labs(x="COVID-19 performance", y="Economic performance",
         title=datePlot) +
    geom_point(color="black", size=1.5)
  if (blLogged) {
    myPlot <- myPlot +
      scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
                    labels=trans_format("log10", math_format(10^.x)))
    }

  if (length(onlyTheseEcons) <= 0) {
    myPlot <- myPlot + geom_smooth(method=loess)
  }
  if (length(labelEcons) > 0) {
    myPlot <- myPlot +
    geom_label_repel(data = subset(useSumm.dt, theISO3c %in% labelEcons),
                     aes(label=theISO3c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both",
                     max.overlaps=15
                     )
  }
  myPlot <- myPlot + theme_economist(base_size=14)
  print(myPlot)
  remove(myPlot)

## Four-quadrant diagram
  useSumm.dt <- useSumm.dt %>%
    mutate(lCovidPerf = log10(covidPerf))
  myEstLo <- loess(lCovidPerf~econPerf, data=useSumm.dt)
  myPreds <- predict(myEstLo)
  useSumm.dt <- useSumm.dt %>%
    mutate(covidPerfPred=10^myPreds)

  myEstLo <- loess(econPerf~lCovidPerf, data=useSumm.dt)
  myPreds <- predict(myEstLo)
  useSumm.dt <- useSumm.dt %>%
    mutate(econPerfPred=myPreds)

  myPlot <- useSumm.dt %>%
    ggplot() +
    aes(x=econPerf, y=covidPerf) +
    labs(x="Economic performance", y="COVID-19 performance",
         title=datePlot) +
    geom_point(color="black", size=1.5) +
    geom_line(aes(x=econPerf, y=covidPerfPred), color="blue", size=1.0) +
    geom_point(aes(x=econPerfPred, y=covidPerf), color="blue", size=1.0)
  if (blLogged) {
    myPlot <- myPlot +
      scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
                    labels=trans_format("log10", math_format(10^.x)))
  }
  if (length(labelEcons) > 0) {
    myPlot <- myPlot +
    geom_label_repel(data = subset(useSumm.dt, theISO3c %in% labelEcons),
                     aes(label=theISO3c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both",
                     max.overlaps=15
                     )
  }
  myPlot <- myPlot + theme_economist(base_size=14)
  print(myPlot)
  remove(myPlot)

  return(useSumm.dt)

# end of crossShow
}


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

# ----------------------------------------------------------------
corrSignif <- function(theData.dt,
                       theMethod="pearson",
                       theMSL=0.05,
                       theOrder="original",
                       blDiag=FALSE,
                       theType="upper",
                       theTLsrt=90,
                       theNmbFont=1,
                       theNmbCEX=1,
                       theMargins=c(0.5, 0.5, 0.5, 0.5)) {
# ----------------------------------------------------------------
# Compactly show correlation and marginal significance level
# Modified from
# https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/
# ----------------------------------------------------------------
#  library(corrplot)
  print(correlation(theData.dt))

  dataMatrix <- as.matrix(theData.dt)
  corrMatrix <- cor(dataMatrix, method = theMethod)
#
# function to build up correlation test matrix
  cor.mtest <- function(theMatrix, corMethod) {
    theMatrix <- as.matrix(theMatrix)
    theNmbCols <- ncol(theMatrix)
    prbMatrix <- matrix(NA, theNmbCols, theNmbCols)
    diag(prbMatrix) <- 0
    for (i in 1:(theNmbCols-1)) {
      for (j in (i+1):theNmbCols) {
        tmp <- cor.test(theMatrix[, i], theMatrix[, j], method=corMethod)
        prbMatrix[i, j] <- prbMatrix[j, i] <- tmp$p.value
      }
    }
    colnames(prbMatrix) <- rownames(prbMatrix) <- colnames(theMatrix)
    prbMatrix
  }
#
  thePrbMatrix <- cor.mtest(dataMatrix, theMethod)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(corrMatrix,
    method = "color", col = col(200), number.font = theNmbFont,
    mar = theMargins, number.cex = theNmbCEX,
    type = theType, order = theOrder,
    addCoef.col = "black", # add correlation coefficient
    tl.col = "black", tl.srt = theTLsrt, # rotation of text labels
    # combine with significance level
    p.mat = thePrbMatrix, sig.level = theMSL, insig = "blank",
    # hide correlation coefficients on the diagonal
    diag = blDiag
  )


# end of corrSignif
}
# eof utilfuncs.R

