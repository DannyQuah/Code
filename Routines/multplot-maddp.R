## ---- multplot-maddp
# This works only for my Maddison Project DF;
# I haven't found it worth coding more generally
# Thu Jan 07 16:33:21 2016 - Danny Quah
getSeries <- c("Year", "Economy", theSeries)
theAES    <- aes_string(x="Year", y=theSeries, group="Economy",
                         colour="Economy")
this.DF   <-
  MaddP.DF[(MaddP.DF$Economy %in% theEconomies) &
           (MaddP.DF$Year >= theBegSmpl) & (MaddP.DF$Year <= theEndSmpl),
           getSeries]

ggplot(data=this.DF, theAES) + geom_line(size=2) + 
       myTStheme + ggtitle(thisTitle)

rm(this.DF, theAES, getSeries)
