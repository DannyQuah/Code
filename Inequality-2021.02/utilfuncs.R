# @(#) utilfuncs.R
# Last-edited: Sat 2021.02.20.1947-- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Sat 2021.02.20.1935 -- Danny Quah (me@DannyQuah.com)
#    Utility functions. Output changes and rates of increase
# ----------------------------------------------------------------

showChangeGrowth <- function(useIndics.dt, useEconomy, useVrbl.str, useYears) {
  message(useEconomy, useVrbl.str,
    " (", useYears[3], "/", useYears[1], " ",
    format(useIndics.dt[3]/useIndics.dt[1], digits=3),
    ", annGR ",
    format(100*(log(useIndics.dt[3]) - log(useIndics.dt[1])) /
           (useYears[3]-useYears[1]), digits=2),
    "%)",
    " (", useYears[3], "/", useYears[2], " ",
    format(useIndics.dt[3]/useIndics.dt[2], digits=3),
    ", annGR ",
    format(100*(log(useIndics.dt[3]) - log(useIndics.dt[2])) /
           (useYears[3]-useYears[2]), digits=2),
    "%)"
        )
}


# eof utilfuncs.R

