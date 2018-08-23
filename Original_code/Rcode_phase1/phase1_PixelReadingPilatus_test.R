# phase1_PixelReadingPilatus_test.R: JAB 2018/07/11, edited WSK 2018/07/25
#
# CONTENT:
# LIBRARIES, PATHS 
# AUXILIARY FUNCTIONS
# READ DAMAGED PIXEL DATA - choose one example!
# COORDINATES WITH MODULES
# CONSISTENCY CHECKS

# Only includes Pilatus material!


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


##### COORDINATES WITH MODULES

coo.dead.module <- data.frame(coo.dead, NA, NA)
colnames(coo.dead.module) <- c("pixcol", "pixrow", "modcol", "modrow")

for (i in 1: dim(coo.dead)[1]){
  coo.dead.module[i,c(3,4)] <- module.which.indices(coo.dead[i,1], coo.dead[i,2], Layout$module.edges.col, Layout$module.edges.row)
}





