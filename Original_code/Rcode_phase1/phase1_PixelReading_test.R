# phase1_PixelReading_test.R: JAB 2018/07/11, edited WSK 2018/07/24
#
# CONTENT:
# LIBRARIES, PATHS 
# AUXILIARY FUNCTIONS
# READ DAMAGED PIXEL DATA - choose one example!
# COORDINATES WITH MODULES
# CONSISTENCY CHECKS


# WORKFLOW: Assumes already run
#    - phase0_LayoutExamples.R (for matching Layout example)
#    - phase0_LayoutPixels.R (for aux functions e.g. module.which.indices())
#    
# INPUT: Layout (object built in Phase0 for suitable detector type)
#
# 0UTPUT: 
#     - coo.dead (list of pixel locations, can be used later to make ppp object)
#
# RUN: 
#   - LIBRARIES, PATHS (user has to adjust some paths names referring to directories in his own computer!)
#   - AUXILIARY FUNCTIONS
#   - Pick only one example in READ DAMAGED PIXEL DATA and, 
#     after matching running matching parts of phase0_LayoutExamples (to the end!),
#     run it
#   - COORDINATES WITH MODULES
#   - CONSISTENCY CHECKS


##### LIBRARIES, PATHS

library(spatstat)
library(tiff)

###### Paths
# Assumes we start in DetectorChecker directory!
source ("paths.R")

###### Appropriate data directory
dirData <- file.path(location, "data")

# ###### Appropriate output directory
# dirOut <- file.path(location, "Output_phase1")
# 
###### Source the definition of Layout
source (file.path(location, "Rcode_phase0", "phase0_LayoutExamples_test.R"))
###### Source the definition of some auxiliary functions
source (file.path(location, "Rcode_phase0", "phase0_LayoutPixel_test_auxiliary.R"))



##### READ DAMAGED PIXEL DATA 

### SELECT ONE & RUN THAT ONE ONLY

#####################################################################
### Example Pilatus
Layout <- generate.layout("Pilatus", layout.list)

### Read tiff images to doublecheck images dimensions and dead pixel coo
# T is an image serving as a mask indicating dead pixels.
# xy are swapped in T obtained as tiff.
# Swap back to assing x to horizontal and y to vertical.

T <- readTIFF(file.path(dirData, "Pilatus", paste("badpixel_mask.tif", sep="")))

# Give warning (see below), but works anyway for our purposes
# Warning message:
#  In readTIFF(paste(dirData, "pilatus_badpixel_mask.tif", sep = "")) :
#  tiff package currently only supports unsigned integer or float sample formats in direct mode, but the image contains signed integer format - it will be treated as unsigned (use as.is=TRUE, native=TRUE or convert=TRUE depending on your intent)

# First consistency check: Detector dimensions okay?

if (Layout$detector.height != dim(T)[2]){
  stop("Error: Number of rows in row data file (tif) incorrect.
      Please check the file and check if your Layout parameters match your damaged pixel data.")
}
if (Layout$detector.width != dim(T)[1]){
  stop("Error: Number of columns in row data file (tif) incorrect. 
      Please check the file and check if your Layout parameters match your damaged pixel data.")
}

# To Do: Figure out what to do with the Warning message:
# In readTIFF(paste(dirData, "pilatus_badpixel_mask.tif", sep = "")) :
# tiff package currently only supports unsigned integer or float sample formats in direct 
# mode, but the image contains signed integer format - it will be treated as unsigned 
# (use as.is=TRUE, native=TRUE or convert=TRUE depending on your intent)
# *** HERE *** we simply stop execution!

# Check range of T.
# Assumption: max values in T are dead pixels, others are fine (only two categories).
# If not binary, transform into a binary matrix Tbin.

table(as.vector(T))
Tbin<-round(T/max(T)) 
table(as.vector(Tbin))
# 0          1 
# 6222840    1161 

coo.dead <- which(Tbin==1, arr.ind = T) # Matrix of damaged pixels coordinates
dim(coo.dead)
# [1] 1161    2  
colnames(coo.dead) <- c("col", "row")
dead.n <- length(as.vector(coo.dead[,2]))

T <- t(T)

# The first col of coo.dead (coo.dead[,1]) corresponds to the detector width dimension (col in Layout).
# The second col of coo.dead (coo.dead[,2]) corresponds to the detector height dimension (row in Layout) 
#####################################################################


#####################################################################
### Example Excalibur - does not currently work (since R update)

# To do:
# Existing codes needs package rhdf5, which worked on previous R version, 
# but that package doesn't yet(?) exist for updated R:
# package 'rhdf5' is not available (for R version 3.5.1)

#T1 <- H5Fopen("pixelmask.fem1.hdf")$data
#T2 <- H5Fopen("pixelmask.fem2.hdf")$data
#T3 <- H5Fopen("pixelmask.fem3.hdf")$data
#T4 <- H5Fopen("pixelmask.fem4.hdf")$data
#T5 <- H5Fopen("pixelmask.fem5.hdf")$data
#T6 <- H5Fopen("pixelmask.fem6.hdf")$data
#T<-cbind(T1,T2,T3,T4,T5,T6)
#####################################################################


#####################################################################
### Examples by Perkin Elmer
Layout <- generate.layout("PerkinElmerFull", layout.list)

# Choose one of 6 examples (from different dates) by specifying k 
# Then run load corresponding Layout:
# For k=1,2,5,6: name = "PerkinElmerFull"
# For k=3,4:     name = "PerkinElmerCropped1600" 
# (though use of the full detector Layout for the cropped one would not give errors at this stage, can't be detected from dead pixel coo)

# Folder PerkinElmer contains 6 folders name
#   BadPixelMap_0 [2013-06-13 13.31.51].bpm etc
# Each includes an xml files with coordinates (to be extracted) named
#   BadPixelMap.bpm.xml

k <- 1 # default choice, may choose any k in 1:6 # done testrun for 5,6,1, 2,3

## Names, aux function

nameBpm <- c(
  "BadPixelMap_0 [2013-06-13 13.31.51].bpm", 
  "BadPixelMap_0 [2013-07-01 11.49.29].bpm", 
  "BadPixelMap_0 [2013-10-02 13.41.00].bpm", 
  "BadPixelMap_0 [2013-11-22 10.54.30].bpm", 
  "BadPixelMap_0 [2014-01-28 11.48.00].bpm", 
  "BadPixelMap_0 [2014-01-28 15.14.02].bpm"  
)

timestamp <- substr(nameBpm,15,35)

Layout$date <- timestamp[k]

extract.number <- function(s){
  s<-substring(s,4,4+nchar(s)-5)   
  s<-as.numeric(s)
}

## Raw data

# decode bad pixel map list from xml file (pedestrian way...)
filename<-file.path(dirData, "PerkinElmer", paste(nameBpm[k],"/BadPixelMap.bpm.xml", sep=""))
d<-matrix(scan(filename, what=c("","","","","",""), skip=3), ncol=6, byrow=TRUE)
  # Warning message: e.g. for k=1 get: Read 56714 items
  # In matrix(scan(filename, what = c("", "", "", "", "", ""), skip = 3),  :
  # data length [56714] is not a sub-multiple or multiple of the number of rows [9453]
  # But(!), warning is just because of end of file, hence just remove last two lines and it works.
D<-d[1:(nrow(d)-2),c(2,3)]
D<-apply(D,2,extract.number)
  # these are coordinates of dead pixels, with 
  # D[,1] for detector cols (width) and D[,2] for detector rows (height) 

## Convert into dead pixel matrix

coo.dead <- matrix(NA, nrow = dim(D)[1], ncol = 2) # Matrix of damaged pixels coordinates
dim(coo.dead)
# [1] 1161    2  
colnames(coo.dead) <- c("col", "row")
coo.dead[,1] <- D[,1]+1
coo.dead[,2] <- D[,2]+1
  # PerkinElmer counts differently: start with 0 in both dimentions.
  # To conform to standard here, adjust dead pixel coordinates accordingly:
dead.n <- length(as.vector(coo.dead[,2]))

## Create damaged pixel mask

Tbin <- matrix(0, nrow=Layout$detector.height, ncol=Layout$detector.width)

for (i in c(1:dim(coo.dead)[1])){
  Tbin[coo.dead[i,]] <- 1
}

#####################################################################


##### COORDINATES WITH MODULES

coo.dead.module <- data.frame(coo.dead, NA, NA)
colnames(coo.dead.module) <- c("pixcol", "pixrow", "modcol", "modrow")

for (i in 1: dim(coo.dead)[1]){
  coo.dead.module[i,c(3,4)] <- module.which.indices(coo.dead[i,1], coo.dead[i,2], Layout$module.edges.col, Layout$module.edges.row)
}

##### CONSISTENCY CHECKS

# Count damaged pixel locations (coo.dead) outside detector (Layout) 
# and in gaps between modules and give warnings

inconsistency.dead.layout <- function(coo.dead, Layout){
  
  outleft <- sum(coo.dead[,1] < 1)
  outright <- sum(coo.dead[,1] > Layout$detector.width)
  if ( (outleft != 0) | (outright != 0) ){
      cat(paste("Warning: One or more of the coordinates of damaged pixels are outside of the detector. \n",
          outleft," are to the left of the detector and ", outright, " are to the right of the detector.\n", sep=""))
  }
  outtop <- sum(coo.dead[,2] < 1)
  outbottom <- sum(coo.dead[,2] > Layout$detector.height)
  if ( (outtop != 0) | (outbottom != 0) ){
    cat(paste("Warning: One or more of the coordinates of damaged pixels are outside of the detector. \n",
              outtop," are above the top of the detector and ", outright, " are below the detector.\n", sep=""))
  }
  colgaps <- c()
  if (sum(Layout$gap.col.sizes) != 0){
    for (i in 1:(Layout$module.col.n-1)){
      colgaps <- c( colgaps, (Layout$module.edges.col[2,i]+1):(Layout$module.edges.col[1,i+1]-1) )
    }
  }  
  rowgaps <- c()
  if (sum(Layout$gap.row.sizes) != 0){
    for (i in 1:(Layout$module.row.n-1)){
      rowgaps <- c(rowgaps, (Layout$module.edges.row[2,i]+1):(Layout$module.edges.row[1,i+1]-1) )
    }
  } 

  in.gaps.dead <- c()
  
   in.gaps <- function(i,coo){
      return((coo[i,1] %in% colgaps) | (coo[i,2] %in% rowgaps))
   }   
   in.gaps.dead <- vector(length=dim(coo.dead)[1])
   for (i in 1:length(in.gaps.dead)){
     in.gaps.dead[i] <- in.gaps(i,coo.dead)
   }
   
  if (sum(in.gaps.dead) != 0){ 
    cat(paste("Warning: ", sum(in.gaps.dead.), 
        " of the coordinates of damaged pixels correspond to locations in gaps between modules of the detector.\n", sep=""))
  }

  inconsistency <- list(outleft, outtop, outright, outbottom, sum(in.gaps.dead))
  names(inconsistency) <- c("left", "top","right","bottom","gaps")
  return(inconsistency)
}
  
inconsistency.dead.layout(coo.dead, Layout)






