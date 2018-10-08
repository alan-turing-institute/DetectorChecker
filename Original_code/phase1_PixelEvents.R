# JAB 2018/07/18, updated 2018/10/03
#
# CONTENT:
# LIBRARIES, PATHS
#
# WORKFLOW: Assumes already run (including auxiliary functions)
#    - phase0_LayoutExamples.R (for matching Layout example)
#    - phase0_LayoutPixel.R (do not run the plots again, though)
#    - phase1_ReadingPixels.R, including consistency checks there
#
# INPUT:
#    - coo.dead (dead pixel coordinates with module indices built phase1_ReadingPixels)
#    - Layout (object built in Phase0 for suitable detector type)
#
# 0UTPUT:
#    - PixelsEvents
#    - pePPP
#    - plots, text files
#
# RUN: all


##### LIBRARIES, PATHS

dirAnalysis<-"/Users/julia/Dropbox/Research/Projects2014+/DetectorChecker/Phase1/"
setwd(dirAnalysis)

dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_PixelEvents/Misc/", sep="")
if (Layout$name == "Pilatus"){
  dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_PixelEvents/Pilatus/", sep="")
}

if (substring(Layout$name,1,11) == "PerkinElmer"){
  dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_PixelEvents/",Layout$name,Layout$date,"/", sep="")
}

library(spatstat)
library(tiff)
library(stringr)
library(fields)
library(splancs)
library(raster)
library(plyr)
library(maptools)
library(igraph)


##### AUXILARY

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



##### HANDLING INDICES/MASKS

coo2bin <- function(coo){
  Bin <- matrix(0, nrow=Layout$detector.height, ncol=Layout$detector.width)
  for (i in c(1:dim(coo)[1])){
    Bin[coo[i,1],coo[i,2]] <- 1
  }
  coo2bin <- Bin
}
# Above needs to be also corrected in phase1_pixelRead, too. See entry in issues.



####### DETECTING AND CLASSIFYING CLUSTERS

classify.clump <- function(x,y){
  # x and y are vectors of the same length containing the x,y coordinates of a clump
  # x=columns=horizontale positions
  # y=rows=verticale positions
  # Note that size of clump is given by length(x) (=length(y)).
  # range(x) length shows expansion in x-direction, range(y) expansion in y-direction.
  nr <- Layout$detector.height
  nc <- Layout$detector.width
  clc <- 0  # a priori assignment for class label is 0, which means nothing and is used later to check if class label assignment failed
  if (length(x)==1) clc<-1 # type 1: singleton
  if (length(x)==2) clc<-2 # type 2: double
  if (length(x)==3) clc<-3 # type 3: triplet (L-shapes or line of 3, could be singleton + 1d infected nn)
  if (length(x)>=4) clc<-4 # type 4: larger cluster (a priori assignment) with lines as special cases reassigned below to other class labels
  # In preparation for identifying special cases of larger clusters:
  xrange <- max(x)-min(x) + 1 # maximal horizontal range
  yrange <- max(y)-min(y) + 1 # maximal vertical range
  # Note: The scenario of other litte bits attached to the line apart from the cluster at the end is rather tedious,
  # but is neglegiable and also may not pose problem as we delete line and then retranslate making them singletons
  # So, define larger cluster as line if enough pixel are on the mode (which defines line location).
  if (yrange > 4 & xrange < 4){
    xline <- getmode(x)  # x-coo of the potential line can be determined by mode(x)
    if (length(x)-2 < length(x==xline)*1.1){
      # vertical line (with up to 10% nn allowed, apart from 2 for the cluster at the end)
      if ( nr-max(y) + 1 < min(y) ) {clc <- 5} # type 5: closest to upper edge
      else {clc <- 6}                          # type 6: closest to lower edge
    }
  }
  if (xrange > 4 & yrange < 4){
    yline <- getmode(y)  # x-coo of the potential line can be determined by mode(x)
    if (length(y)-2 < length(y==yline)*1.1){
      # horizontal line (with up to 10% nn allowed, apart from 2 for the cluster at the end)
      if (nc-max(x) + 1 < min(x)) {clc <- 7}   # type 7: nearest to right side
      else {clc <- 8}                          # type 8: nearest to left side
    }
  }
  return(clc)
  if (clc==0){cat("Error in class assignment: no class could be identified for clump ", i,
                  ". Double check image. Trace where that clump is. Enumeration goes from
                  top left to bottom right. However, original image may have been transposed
                  and flipped.")}
}
# Explanation: This function assign each dead pixel location a cluster type coded as follows:
# 1: singleton
# 2: double
# 3: triplet
# 4: larger cluster, unless it actually has the shape of a line
# 5 (6): vertical line where closest edge is the upper (lower) one
# 7 (8): horizontal line where closest edge is the right (left) one

xyc.ply <- function(xyc){
  xycply <- ddply(xyc, .(id),
                  summarise,                        # 1
                  class = classify.clump(x,y),      # 2
                  size = length(x),                 # 3
                  xct = min(x) + (max(x)-min(x))/2, # 4 not always a pixel (see below)
                  yct = min(y) + (max(y)-min(y))/2, # 5 not always a pixel
                  xctpix = round(median(x)),        # 6 better than above, avoids centre pulled by hairs sticking out
                  yctpix = round(median(y)),        # 7 dito
                  xmin = min(x),                    # 8
                  xmax = max(x),                    # 9
                  xrange = max(x)-min(x) + 1,       #10
                  ymin = min(y),                    #11
                  ymax = max(y),                    #12
                  yrange = max(y)-min(y) + 1,       #13
                  xmode=getmode(x),                 #14
                  ymode=getmode(y))                 #15
  return(xycply)
}
# Explanations:
# Clumps have IDs and come with a set of xy coordinates.
# For each clump, extract features and store in data frame using dplyr.
# summarise() looks at all xy with same ID and then applies functions.
# Here apply functions to the pair of vectors x,y.
# In particularly, each clump now has a class.
# Size: just use length of the vector of all coordinates (x or y same, as they are pairs)
# Pixels are described by their centres.
# xct, yct: do not have to be a pixel, but could be in between two.
# Specifically, even: on line between two central pixels, odd: centre of (central) pixel
# Hence we use xctpix, yctpix instead: not perfect, can easily construct pathological scenarios, but good enough for our data!
# xlength, ylength: max extension in these directions
# One a general note, while plyr seems to be so popular, it makes the code not transparent to refer to column numbers.
# Can we do column names?



####### MODIFYING CLUSTERS TO EVENTS (CONSISTING OF 1 PIXEL REPRESENTING THE CLUSTER)

# Make into a point pattern of just events rather than pixels. Using xyc.ply object.
# Collapse in one point using centres for clusters, but end points for lines, type dependend:
# type 5 (closest to upper edge): ymin
# type 6 (closest to lower edge): ymax
# type 7 (closest to right edge): xmin
# type 8 (closest to left edge):  xmax
# This is inspired by Perkin Elmer Layout and be replaced by other choices if desired.

xyc.pixels2events <- function(xycply){
  xycevents <- xycply[,c(6,7,1,2,3)]
  # This defines the default for representing a cluster by centres xctpix, yctpix.
  # For lines, this is replaced by endpoints (choice how explained above).
  for (i in 1:dim(xycevents)[1]){
    #cat("---", "ID ",i,"|")
    if (xycevents[i,4]==5) {
      xycevents[i,1]<-xycply[i,14]   # xmode
      xycevents[i,2]<-xycply[i,12]   # ymin
    }
    if (xycevents[i,4]==6) {
      xycevents[i,1]<-xycply[i,14]   # xmode
      xycevents[i,2]<-xycply[i,11]   # ymax
    }
    if (xycevents[i,4]==7) {
      xycevents[i,1]<-xycply[i,9]    # ymode
      xycevents[i,2]<-xycply[i,15]   # xmin
    }
    if (xycevents[i,4]==8) {
      xycevents[i,1]<-xycply[i,8]    # ymode
      xycevents[i,2]<-xycply[i,15]   # xmax
    }
  }
  return(xycevents)
}



# CREATE PPP OBJECT CONTAINING BOTH PIXELS AND EVENTS FROM BINARY MASK

bin2cooEvents <- function(Tbin, makeplots=T, makefiles=T){

  nr <- Layout$detector.height
  nc <- Layout$detector.width

  # Raster object preparation:
  # Detect & classify into lines, clusters, duplicates etc using functions above
  # May do this approximately to save time!!! Really!!!!!
  # identify & group all adjacent pixels:
  # extract clusters of pixels (that are not NA)

  rr <- raster(nrow=nr, ncol=nc, xmn=0, xmx=nc,ymn=0,ymx=nr)
  values(rr) <- t(Tbin[,c(nr:1)])  #need to both transpose matrix and flip about horizontal axis
  rrc <- clump(rr, directions=4)

  # Optional: Test plots using these tmp objects
  if (makeplots==T){
      # Using plot(): should result in two identically oriented images.
      # Note hat using image() instead of plot() above did not give the correct images, but leaves lines/pixel out!
      tiff(file=paste(dirOut,"rr_plot.tif",sep=""), width=nc, height=nr)
      par(mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
      plot(rr, xlim=c(0,nc), ylim=c(0,nr), cex.main=1.5, maxpixels=nr*nc,legend=F, nc=nc, nr=nr)
      dev.off()
      tiff(file=paste(dirOut,"rrc_plot.tif",sep=""), width=nc, height=nr)
      par(mfrow=c(1,1), mar=c(5,5,2,2)+0.1)
      plot(rrc, xlim=c(0,nc), ylim=c(0,nr), cex.main=1.5, maxpixels=nr*nc,legend=F, nc=nc, nr=nr)
      dev.off()
  }

  ### Make data frame with all pixels part of clusters (clumps) and their clump ID and centre's coordinates.
  xycPixels <- data.frame(ceiling(xyFromCell(rrc, which(!is.na(getValues(rrc))))),
                          id = getValues(rrc)[!is.na(getValues(rrc))])  # x, y
              # Note we use ceiling, because xyc would have coordinates converted from integers to .5 ending

  xycEvents <- xyc.pixels2events(xyc.ply(xycPixels))

  ### Optional: Write cluster results into files.
  if (makefiles==T){

  write.table(xycPixels[order(xycEvents[,3]),],
              file=paste(dirOut,"xycEvents.txt",sep=""))

  write.table(xycEvents[order(xycEvents[,3]),],
              file=paste(dirOut,"xycEvents.txt",sep=""))
  }

  list(Pixels=xycPixels, Events=xycEvents)
}



### MAKE POINT PATTERN

pixelsEvents2ppp <- function(PixelsEvents){

  nr <- Layout$detector.height
  nc <- Layout$detector.width

  # Make into a point pattern and plot ppp pattern and density
  Pixelppp<-ppp(PixelsEvents$Pixels[,1],PixelsEvents$Pixels[,2],c(1,nc),c(1,nr))
  Eventppp<-ppp(PixelsEvents$Events[,1],PixelsEvents$Events[,2],c(1,nc),c(1,nr))

  list(Pixels=Pixelppp, Events=Eventppp)
}



### PLOTS VISUALISING BOTH PIXELS AND EVENTS

plots.pixels.events <- function(pePPP){

  ### Visualise pixels and events in one plot and separately
  pdf(file = paste(dirOut,"ppp_pixelsEvents.pdf",sep=""))
  par(mfrow=c(1,1), mar=c(0,0,4,0)+0.1, oma=c(0,0,0,0))
  plot(pePPP$Pixels, pch=22, main="Defective pixels (black) and events (red)")
  points(pePPP$Events, pch=1, col=2)
  dev.off()

  pdf(file = paste(dirOut,"ppp_pixels.pdf", sep=""))
  par(mfrow=c(1,1), mar=c(0,0,4,0)+0.1, oma=c(0,0,0,0))
  plot(pePPP$Pixels, pch=22, main="Defective pixels")
  dev.off()

  pdf(file = paste(dirOut,"ppp_events.pdf", sep=""))
  par(mfrow=c(1,1), mar=c(0,0,4,0)+0.1, oma=c(0,0,0,0))
  plot(pePPP$Events, pch=22, main="Defective events")
      # plot(Eventppp, pch=22, col=2, main="Defective events") doesn't work, instead cheat:
  points(pePPP$Events, pch=22, col=2)
  dev.off()

  ### Visualise densities of pixels and events
  pdf(file = paste(dirOut,"density_pixels.pdf", sep=""))
  par(mfrow=c(1,1), mar=c(0,0,4,2)+0.1, oma=c(0,0,0,0))
  plot(density(pePPP$Pixels), main="Density Pixels")
  dev.off()

  pdf(file = paste(dirOut,"density_events.pdf", sep=""))
  par(mfrow=c(1,1), mar=c(0,0,4,2)+0.1, oma=c(0,0,0,0))
  plot(density(pePPP$Pixels), main="Density Pixels")
  dev.off()
}



##### RUN

# We first translate dead pixel coordinates into a binary mask,
# because this form makes it easy to use neighbourhood structure and other geometry
# to detect and classify clusters in different types.
# We use the original dead pixel coordinates for a consistency check.

Tbin <- coo2bin(coo.dead)                 # make binary mask from coordinates
PixelsEvents <- bin2cooEvents(Tbin)    # make object with dead pixel and event coo (test plots, files optional)
# Check: data.frame(PixelsEvents$Pixels[1:20,], PixelsEvents$Events[1:20,])
pePPP <- pixelsEvents2ppp(PixelsEvents)
plots.pixels.events(pePPP)

