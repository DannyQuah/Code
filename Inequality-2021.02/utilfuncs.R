# @(#) utilfuncs.R
# Last-edited: Tue 2021.07.13.1819 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sat 2021.02.20.1935 -- Danny Quah (me@DannyQuah.com)
#    Utility functions. Calculate and Output
# ----------------------------------------------------------------

# ----------------------------------------------------------------
crossShow <- function(widData.dt, lablEcons, timeSpan) {
# For cross section of economies, plot indicators together with
# B50c growth. Returns the summary datatable
# If the max of mltB50c and mltIneqQ falls below currDropThresh
# (I suggest 25%) drop that economy. This will happen pretty much
# only if that economy collapses against the US dollar or Euro or
# whichever international currency we're using
# ----------------------------------------------------------------
  currDropThresh <- 0.25
  timeLength <- max(timeSpan) - min(timeSpan)
  timeLabel  <- paste0(min(timeSpan), "-", max(timeSpan))
  indics.dt <- widData.dt %>%
    filter(theYear %in% timeSpan) %>%
    select(theISO2c, econName, theYear, avgB50c, ineqQ)
  theSumm.dt <- indics.dt %>%
    group_by(theISO2c) %>% 
    summarize(econName = first(econName),
              locB50c  = median(avgB50c),
              grrB50c  = longGrowth(avgB50c, theYear),
              mltB50c  = exp((grrB50c/100) * timeLength),
              locIneqQ = median(ineqQ),
              grrIneqQ = longGrowth(ineqQ, theYear),
              mltIneqQ = exp((grrIneqQ/100) * timeLength)
             ) %>%
    ungroup()

  theSumm.dt <- theSumm.dt %>%
    filter((mltB50c >= currDropThresh) | (mltIneqQ >= currDropThresh))

  pltLabel <- paste0(timeLabel,
                     " When the bottom 50% rise, so too inequality")
  myPlot <- theSumm.dt %>%
    ggplot(., aes(x=mltB50c, y=mltIneqQ)) +
    labs(x="B50c-X", y="ineqQ-X", title=pltLabel) +
    geom_point(color="black", size=1.5) +
    geom_label_repel(data =
                     subset(theSumm.dt, theISO2c %in% lablEcons),
                     aes(label=theISO2c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both"
                     ) +
    geom_smooth(method=loess) +
    theme_economist(base_size=13)
  print(myPlot)


  theSumm.dt <- theSumm.dt %>%
    mutate(mltIneqQfit = predict(loess(mltIneqQ ~ mltB50c)))

  pltLabel <-paste0(timeLabel,
                    " Residual change: inequality and B50")
  myPlot <- theSumm.dt %>%
    ggplot(., aes(x=mltB50c, y=mltIneqQ-mltIneqQfit)) +
    labs(x="B50c-X", y="Residual ineqQ-X", title=pltLabel) +
    geom_point(color="black", size=1.5) +
    geom_label_repel(data =
                     subset(theSumm.dt, theISO2c %in% lablEcons),
                     aes(label=theISO2c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both"
                     ) +
    theme_economist(base_size=14)
  print(myPlot)

  pltLabel <- paste0(timeLabel,
                     " Does inequality condition the change in B50?")
  myPlot <- theSumm.dt %>%
    ggplot(., aes(x=locIneqQ, y=mltB50c)) +
    labs(x="ineqQ", y="B50c-X", title=pltLabel) +
    geom_point(color="black", size=1.5) +
    geom_label_repel(data =
                     subset(theSumm.dt, theISO2c %in% lablEcons),
                     aes(label=theISO2c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both"
                     ) +
    geom_smooth(method=loess) +
    theme_economist(base_size=13)
  print(myPlot)

  pltLabel <- paste0(timeLabel,
                     " Does inequality condition its rise?")
  myPlot <- theSumm.dt %>%
    ggplot(., aes(x=locIneqQ, y=mltIneqQ)) +
    labs(x="ineqQ", y="ineqQ-X", title=pltLabel) +
    geom_point(color="black", size=1.5) +
    geom_label_repel(data =
                     subset(theSumm.dt, theISO2c %in% lablEcons),
                     aes(label=theISO2c),
                     box.padding=unit(2.0, "lines"),
                     point.padding=unit(0.35, "lines"),
                     label.padding=unit(0.25, "lines"),
                     arrow=arrow(length=unit(0.25, "cm"),
                                 ends="last", type="closed"),
                     segment.color="grey10",
                     segment.size=0.5,
                     direction="both"
                     ) +
    geom_smooth(method=loess) +
    theme_economist(base_size=13)
  print(myPlot)
  return(theSumm.dt)

# end crossShow
}

# ----------------------------------------------------------------
longGrowth <- function(theValues, theYears) {
# Annual growth rate % over longest possible stretch,
# assuming theYears are sorted sequentially, even if gaps
# ----------------------------------------------------------------
  indxsNonNA  <- which(!is.na(theValues) & !is.na(theYears))
  if (length(indxsNonNA) <= 1) {
    grRate <- NA
  }
  else {
    indxFirst <- indxsNonNA[1]
    indxLast  <- indxsNonNA[length(indxsNonNA)]
    grRate <- log(theValues[indxLast] / theValues[indxFirst]) /
      (indxLast - indxFirst)
  }
  return(100 * grRate)
}


# ----------------------------------------------------------------
summaryShow <- function(widData.dt, useEconomy, useCurr, useYears,
                        graphIt) {
# For useEconomy, graph Inequality, B50; show summary statistics
# ----------------------------------------------------------------
  if (graphIt) {
    graphIneqB50(widData.dt, useEconomy, useCurr)
  }

  theIndics.dt <- widData.dt %>%
    filter(theISO2c == useEconomy) %>% filter(theYear %in% useYears) %>%
    select(avgB50c, ineqQ, avgT10c)
  theIndics.v <- theIndics.dt[[1]]
  theOutp.str <- " B50 "
  showChangeGrowth(theIndics.v, useEconomy, theOutp.str, useYears)
  theIndics.v <- theIndics.dt[[2]]
  theOutp.str <- " Ineq"
  showChangeGrowth(theIndics.v, useEconomy, theOutp.str, useYears)
  theIndics.v <- theIndics.dt[[3]]
  theOutp.str <- " T10 "
  showChangeGrowth(theIndics.v, useEconomy, theOutp.str, useYears)

# end summaryShow
}

# ----------------------------------------------------------------
showChangeGrowth <- function(useIndics.v, useEconomy, useVrbl.str,
                             useYears) {
# Display change ratio and annual rate of change in indicators
# ----------------------------------------------------------------
  x12 <- useIndics.v[2] / useIndics.v[1]
  g12 <- 100*(log(useIndics.v[2]) - log(useIndics.v[1])) /
              (useYears[2] - useYears[1])
  x23 <- useIndics.v[3] / useIndics.v[2]
  g23 <- 100*(log(useIndics.v[3]) - log(useIndics.v[2])) /
              (useYears[3]-useYears[2])
  x13 <- useIndics.v[3] / useIndics.v[1]
  g13 <- 100*(log(useIndics.v[3]) - log(useIndics.v[1])) /
              (useYears[3]-useYears[1])
  message(useEconomy, useVrbl.str,
    " (", useYears[1], "-", useYears[2], " ",
    format(round(x12, 2), nsmall=2), "X",
    ", aGR ", format(round(g12, 2), nsmall=2, width=5), "%)",
    " (", useYears[2], "-", useYears[3], " ",
    format(round(x23, 2), nsmall=2), "X",
    ", aGR ", format(round(g23, 2), nsmall=2, width=5), "%)",
    " (", useYears[1], "-", useYears[3], " ",
    format(round(x13, 2), nsmall=2), "X",
    ", aGR ", format(round(g13, 2), nsmall=2, width=5), "%)"
        )

# end showChangeGrowth
}

# ----------------------------------------------------------------
graphIneqB50 <- function(widData.dt, useEconomy, useCurr) {
# Graph inequality and B50, and B50
# ----------------------------------------------------------------
# Graph inequality and B50
  myPlot <- widData.dt %>%
    filter(theISO2c == useEconomy) %>%
    ggplot(., aes(x = theYear)) +
      labs(title = paste0(useEconomy, " Ineq, B50"),
          y = useCurr) +
      geom_line(aes(y = avgB50c), color = "darkred",
                linetype = "solid", size = 1.5) +
      geom_line(aes(y = ineqQ), color = "darkblue",
                linetype = "longdash", size = 1.5) +
      expand_limits(y = 0) +
      theme_economist(base_size = 14)
  print(myPlot)

    # Graph B50
  myPlot <- widData.dt %>%
      filter(theISO2c == useEconomy) %>%
      ggplot(., aes(x = theYear)) +
        labs(title = paste0(useEconomy, " B50"), y = useCurr) +
        geom_line(aes(y = avgB50c), color = "darkred",
                  linetype = "solid", size = 1.5) +
        expand_limits(y = 0) +
        theme_economist(base_size = 14)
  print(myPlot)

# end graphIneqB50
}

# ----------------------------------------------------------------
addChecks <- function(widData.dt, useEconomy, useVrbCurr) {
# Additional checks to run, optional dump
# ----------------------------------------------------------------
  widData.dt <- widData.dt %>%
    mutate(avgB50d  = avgB50 / (1000 * vrbCurr)) %>%
    mutate(trhB50d  = trhB50 / (1000 * vrbCurr)) %>%
    mutate(avgT10d  = avgT10 / (1000 * vrbCurr)) %>%
    mutate(trhT10d  = trhT10 / (1000 * vrbCurr))

  widData.dt %>%
    filter(theISO2c == useEconomy) %>%
    ggplot(., aes(x = theYear)) +
    geom_line(aes(y = avgB50c), color = "darkred",
              linetype = "solid", size = 1.5) +
    geom_line(aes(y = avgB50d), color = "steelblue",
              linetype = "longdash", size = 1.5) +
    theme_economist(base_size = 14)

}


# eof utilfuncs.R

