## ---- eyetrend-maddp
# This works only for my Maddison Project DF;
# I haven't found it worth coding more generally
# Thu Jan 07 16:33:21 2016 - Danny Quah
olsFIT <- lm(logPerCapGDP ~ Year,
              data=MaddP.DF[(MaddP.DF$Economy == theEconomy) &
              (MaddP.DF$Year >= theBegFit) & (MaddP.DF$Year <= theEndFit), ])

thisTitle <- paste0(theEconomy,
 ": log Per Capita GDP in constant 1990 Int. GK$")

this.DF <-
  MaddP.DF[(MaddP.DF$Economy == theEconomy) &
           (MaddP.DF$Year >= theBegFit) & (MaddP.DF$Year <= theEndSmp),
           c("Year", "perCapitaGDP", "logPerCapGDP")]

 ggplot(data=this.DF, aes(x=Year, y=logPerCapGDP)) + geom_line(size=2) +
 geom_segment(data=this.DF, aes(x=theBegFit, xend=theEndFit,
              y=coef(olsFIT)[1]+coef(olsFIT)[2]*theBegFit,
               yend=coef(olsFIT)[1]+coef(olsFIT)[2]*theEndFit),
              linetype=1, colour="blue", size=1.1) +
 geom_segment(data=this.DF, aes(x=theEndFit+1, xend=theEndSmp,
              y=coef(olsFIT)[1]+coef(olsFIT)[2]*(theEndFit+1),
               yend=coef(olsFIT)[1]+coef(olsFIT)[2]*theEndSmp),
              linetype=2, colour="blue", size=1.05) +
 myTStheme + ggtitle(thisTitle)

#
thisTitle      <- paste0(theEconomy,
": Per Capita GDP in constant 1990 Int. GK$")

expTrendFitted <- function(x) {ifelse (x>=theBegFit & x<=theEndFit,
  exp(coef(olsFIT)[1] + (x * coef(olsFIT)[2])), NA)
}
expTrendExtrap <- function(x) {ifelse (x>=theEndFit+1 & x<=theEndSmp,
  exp(coef(olsFIT)[1] + (x * coef(olsFIT)[2])), NA)
}
ggplot(data=this.DF, aes(x=Year, y=perCapitaGDP)) + geom_line(size=2) +
stat_function(fun=expTrendFitted, linetype=1,
               colour="blue", size=1.1) +
stat_function(fun=expTrendExtrap, linetype=2,
               colour="blue", size=1.05) +
myTStheme + ggtitle(thisTitle)

rm(olsFIT, expTrendFitted, expTrendExtrap)
