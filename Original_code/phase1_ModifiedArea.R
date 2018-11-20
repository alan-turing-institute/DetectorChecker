# JAB 2018/07/18, updated 2018/10/03, 2018/10/25, 2018/10/29, 2018/11/18
#
# CONTENT:
# LIBRARIES, PATHS
#
# WORKFLOW: Assumes already run (including auxiliary functions)
#    - phase0_LayoutExamples.R (for matching Layout example)
#    - phase0_LayoutPixel.R (do not run the plots again, though)
#    - phase1_ReadingPixels.R, including consistency checks there
#    - phase1_PixelsEvents.R 
#
# INPUT:
#    - pePPP (with $Pixels, $Events)
#    - Layout (object built in Phase0 for suitable detector type)
#
# 0UTPUT:
#    - peModPPP after eliminating high density areas based on Pixels (not Events)
#    - plots, text files for each Pixels and Events
#
# RUN: wrapper function as below
#
# What this does: 
# Determines area with high occurrance of damaged pixels and remove it by defining a mask.
# Define modified ppp object with that area cut out.
# Visualises this.

##### LIBRARIES, PATHS

dirAnalysis<-"/Users/julia/Dropbox/Research/Projects2014+/DetectorChecker/Phase1/"
setwd(dirAnalysis)

dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_PixelEvents/Misc/", sep="")
if (Layout$name == "Pilatus"){
  dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_PixelEvents/Pilatus/", sep="")
}

if (substring(Layout$name,1,11) == "PerkinElmer"){
  dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_ModifiedArea/",Layout$name,Layout$date,"/", sep="")
}

library(spatstat)
library(tiff)
library(stringr)
library(fields)
library(splancs)
library(raster)
library(plyr)
#library(maptools)
library(igraph)


makeModMask <- function(pppPixels, adj=0.1, thres=0.05, makeplots=FALSE){
  # runs for about 1 min, but there are two loops that could be replaced by more efficient code
  # adj: adjusting the bandwidth in kernel approximation to capture shapes tightly
  # thres: default: cut out areas with more than 5% dead pixels
  # from experience: 0.01 too low, would include all lines, 0.025 some
  nr <- Layout$detector.height
  nc <- Layout$detector.width
  densityObj <- density(pppPixels, adjust=adj, kernel="gaussian")  # makes 128 x 128, why?
  densityObjvec <- as.vector(densityObj[1:128,1:128], mode="numeric")
    
  IndRegDensity <- densityObj < thres #has small format 128x12
  # technical implementation of the mask
  RegDensityMask <- matrix(NA, nrow=nc, ncol=nr)
  # scale IndRegDensity up to full format  --- too slow!!!--- make more elegant using apply() etc
  for (k in 1:nr){
    for (j in 1:nc){
      RegDensityMask[j,k] <- IndRegDensity[ceiling(k*128/nr), ceiling(j*128/nc)]
    }
  }
  if (makeplots==TRUE){
     par(mfrow=c(1,1), mar=c(3,3,4,1))
     pdf(file = paste(dirOut,"Pixels", "_", adj, "_", thres,".pdf", sep=""))
     plot(pppPixels, cex=0.001, pch='.', main=paste("Damaged Pixels") )
     dev.off()
     pdf(file = paste(dirOut,"Density", "_", adj, "_", thres,".pdf", sep=""))
     image(densityObj, main="Density")
     dev.off()
     pdf(file = paste(dirOut,"Histogram", "_", adj, "_", thres,".pdf", sep=""))
     hist(densityObj, breaks=20, main="Histogram of density")
     dev.off()
     pdf(file = paste(dirOut,"DensityCutoff", "_", adj, "_", thres,".pdf", sep=""))
     image(IndRegDensity, main="Density cut off")
     dev.off()
     pdf(file = paste(dirOut,"Mask", "_", adj, "_", thres,".pdf", sep=""))
     image(RegDensityMask, main="Mask")
     dev.off()
  }
  return(RegDensityMask)
}


# Function modify.PPP() modifies dead pixel patterns: 
# Cut out high density area from ppp using binary mask.
# The mask is based on pixel(!) level information, not event level.
# However, both pixel and event ppp pattern have to be adjusted based on that same pixel based mask.

modify.PPP <- function(pePPP, Mask, adj=0.1, thres=0.05, makeplots=FALSE){

  pePPPMod <- list(Pixels=NA, Events=NA)
  pePPPMod$Pixels <-ppp(pePPP$Pixels$x, pePPP$Pixels$y, mask=t(Mask), c(1,nc),c(1,nr))
  pePPPMod$Events <-ppp(pePPP$Events$x, pePPP$Events$y, mask=t(Mask), c(1,nc),c(1,nr))  
 
  bound <- owin(xrange=c(1,nc), yrange=c(1,nr), mask=t(Mask))
   
  ppptmp <- pePPP$Pixels
  dfp <- data.frame(x=ppptmp$x, y=ppptmp$y)
  pppbound <- ppp(dfp$x, dfp$y, mask=t(Mask), c(1,nc), c(1,nr))
  isin <- inside.owin(x=dfp$x, y=dfp$y, w=bound)
  in_pixels <- dfp[isin, ]
  out_pixels <- dfp[!isin, ]
  
  ppptmp <- pePPP$Events
  dfe <- data.frame(x=ppptmp$x, y=ppptmp$y)
  pppbound <- ppp(dfe$x, dfe$y, mask=t(Mask), c(1,nc), c(1,nr))
  isin <- inside.owin(x=dfe$x, y=dfe$y, w=bound)
  in_events <- dfe[isin, ]
  out_events <- dfe[!isin, ]
  
  rm(ppptmp)
  
  if (makeplots==TRUE){
    pdf(file = paste(dirOut,"ModifiedPointPatternPixels", "_", adj, "_", thres,".pdf", sep=""))
    par(mfrow=c(1,1), mar=c(3,3,4,1))
    plot(bound, main=paste("Damaged pixels, masked in red"))
  #  points(pePPPMod$Pixels, cex=0.001, pch='.')
    points(dfp)
    points(in_pixels,col="green",cex = 3 )
    points(out_pixels,col="red",cex = 2 )
    dev.off()
    
    pdf(file = paste(dirOut,"ModifiedPointPatternEvents", "_", adj, "_", thres,".pdf", sep=""))
    par(mfrow=c(1,1), mar=c(3,3,4,1))
    plot(bound, main=paste("Damaged events, masked in red"))
   # points(pePPPMod$Events, cex=0.001, pch='.')
    points(dfe)
    points(in_events,col="green",cex = 3 )
    points(out_events,col="red",cex = 2 )
    dev.off()
  }
  return(pePPPMod)
}


modifiedArea.PPP <- function(pePPP, makeMask=TRUE, adj=0.1, thres=0.05)
{
  if (makeMask==TRUE){
     Mask <- makeModMask(pePPP$Pixels, adj=adj, thres=thres, makeplots=TRUE)  # slow, but could probably be improved
  }
  pePPPMod <- modify.PPP(pePPP, Mask, adj=adj, thres=thres, makeplots=TRUE) # R gives warning message about points outside window, but they are supposed to be outside!

  sink(paste(dirOut, "/", "CountsEventsPixelsModifiedArea.txt", sep=""), append=TRUE)
  cat("\n") 
  cat("Total number of damaged pixels: ", pePPP$Pixels$n, "\n")
  cat("Total number of dameged pixels after removing areas of : ", pePPPMod$Pixels$n, "\n")
  cat("Total number of damage events: ", pePPP$Events$n, "\n")
  cat("Total number of damege events after removing areas: ", pePPPMod$Events$n, "\n")
  sink()
  
  return(pePPPMod)
}  

##################################################
##### RUN ########################################

pePPPMod <- modifiedArea.PPP(pePPP, makeMask=TRUE, adj=0.1, thres=0.05)


##################################################




