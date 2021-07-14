#!/usr/bin/env R
# @(#) stats-dq.R
# Last-edited: Fri 2021.07.02.2313 -- Danny Quah (me@DannyQuah.com)
# ----------------------------------------------------------------
# Revision History:
#  % Fri 2021.07.02.2257 -- Danny Quah (me@DannyQuah.com)
#    First draft: R script for my stats routines
# ----------------------------------------------------------------
library(corrplot)
library(correlation)

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



# eof stats-dq.R

