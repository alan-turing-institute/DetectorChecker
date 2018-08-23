# JAB 2018/07/09
#
# CONTENT:
# LIBRARIES, PATHS AND FILE NAMES
# AUXILIARY FUNCTIONS
# CALCULATING PIXEL SPECIFIC LAYOUT INFORMATION
# PLOTS OF PIXEL SPECIFIC LAYOUT INFORMATION

# INPUT: Layout (object)
#        use code in phase0_LayoutExamples.R
#
# 0UTPUT: LayoutPixel (object), plots
#
# RUN: Minimum, to obtain complete LayoutPixel object, run 
#     - LIBRARIES, PATHS AND FILE NAMES (user has to adjust some paths names referring to directories in his own computer!)
#     - AUXILIARY FUNCTIONS
#     - CALCULATING PIXEL SPECIFIC LAYOUT INFORMATION
#    
# RUN optional:
#     - PLOTS OF PIXEL SPECIFIC LAYOUT INFORMATION to make images of LayoutPixel elements


##### LIBRARIES, PATHS AND FILE NAMES

library(spatstat)

dirAnalysis<-"/Users/julia/Dropbox/Research/Projects2014+/DetectorChecker/Phase0/"
dirPlots<-"/Users/julia/Dropbox/Research/Projects2014+/DetectorChecker/Phase0/Output_phase0/Phase0_LayoutPixels/"
setwd(dirAnalysis)


##### AUXILIARY FUNCTIONS

# Function is in 1d context to be applied to rows and cols separately.
# Returns number of col (or row) of the module wrt layout grid.
# Given x (or y) coordinate it finds nearest col (or row) edge. 
# Determined based on m.e matrix containing positions of relevant edges (later used: module.edges.col, module.edges.row)
# If outside modules (in gaps or outside detector) assign NA.

module.which <- function(coo, m.e){
  module.which <- NA
  for (i in 1:dim(m.e)[2]){
    if ((coo>=m.e[1,i]) & (coo<=m.e[2,i])){
      module.which <- i
    }
  }
  return(module.which)
}

# Function returns both col and row wrt layout grid.
# Given xy coo of pixel and matrices with positions of edges in both directions.

module.which.indices <- function(x, y, module.edges.col, module.edges.row){
  tmp <- list(col=module.which(x, module.edges.col), row=module.which(y, module.edges.row))
  return(tmp)
}

# Function returns distance to both edges.
# Given xy coo of pixel and matrices with positions of edges in both directions.

module.dist.edge <- function(x, y, module.edges.col, module.edges.row){
  ix <- module.which(x, module.edges.col)    
  iy <- module.which(y, module.edges.row)  
  tmp <- list( distcol=min(x - module.edges.col[1,ix], module.edges.col[2,ix] - x),
               distrow=min(y - module.edges.row[1,iy], module.edges.row[2,iy] - y))
  return(tmp)
}


##### CALCULATING PIXEL SPECIFIC LAYOUT INFORMATION

# Matrices that contain, for each pixels, functions of the layout
# Later used as predictors in logistic regression models for pixel death probability

if (Layout$detector.inconsistency > 0){
  cat("There are one or more inconsistencies in the detector layout parameters. Please correct these.")
}

dummy.matrix.col <- matrix(0, nrow=Layout$detector.height, ncol=Layout$detector.width)
dummy.matrix.col[ , as.vector(Layout$module.edges.col)] <- 1

dummy.matrix.row <- matrix(0, nrow=Layout$detector.height, ncol=Layout$detector.width)
dummy.matrix.row[as.vector(Layout$module.edges.row), ] <- 1

dist.matrix.centre.eucl <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.centre.linf <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.corner <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.col <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.row <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.min <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width)

xx <- matrix( abs(1:Layout$detector.width - Layout$detector.width/2), nrow=Layout$detector.height, ncol=Layout$detector.width, byrow = TRUE)
yy <- matrix( abs(1:Layout$detector.height - Layout$detector.height/2), nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.centre.eucl <- sqrt( xx^2 + yy^2 )
dist.matrix.centre.linf <- pmax(xx,yy, na.rm=TRUE)

dist.closest.edge <- function(x, size){
  if ((x<1) | (x>size)){ 
    cat("Pixel coordinate outside detector.")
  }
  return(min(x-1,size-x)) # Why x-1? Because pixel locations start in 1, but we want both edges inside detector for symmetry
}
xx <- matrix( sapply(c(1:Layout$detector.width), function(x) dist.closest.edge(x,Layout$detector.width)), nrow=Layout$detector.height, ncol=Layout$detector.width, byrow = TRUE)
yy <- matrix( sapply(c(1:Layout$detector.height), function(x) dist.closest.edge(x,Layout$detector.height)), nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.corner <- sqrt( xx^2 + yy^2 )

# Semi-pedestrian method

dist.matrix.col <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width) # dist to nearest vertital edge, all rows equal
dist.matrix.row <- matrix(NA, nrow=Layout$detector.height, ncol=Layout$detector.width) # dist to nearest horizontal edge, all cols equal

for (y in 1:Layout$detector.height){
  dist.matrix.row[y,1] <- module.dist.edge(1, y, Layout$module.edges.col, Layout$module.edges.row)$distrow
}
dist.matrix.row[ ,2:Layout$detector.width] <- dist.matrix.row[ ,1]    # all cols are like first col

for (x in 1:Layout$detector.width){
  dist.matrix.col[1,x] <- module.dist.edge(x, 1, Layout$module.edges.col, Layout$module.edges.row)$distcol
}
# Mystery: should work but does not! dist.matrix.col[2:detector.height, ] <- dist.matrix.col[1, ]    # all rows are like first row
# Replace by pedestrain version:
for (y in 2:Layout$detector.height){
  dist.matrix.col[y, ] <- dist.matrix.col[1, ] 
}

dist.matrix.min <- pmin(dist.matrix.col, dist.matrix.row)

# To check alternative calculation methods use things like sum(as.vector(dist.matrix.min)-as.vector(dist.matrix.min1), na.rm=TRUE)

LayoutPixel <- list(
  dummy.matrix.col =  dummy.matrix.col,
  dummy.matrix.row =  dummy.matrix.row,
  dist.matrix.centre.eucl =  dist.matrix.centre.eucl,
  dist.matrix.centre.linf = dist.matrix.centre.linf,
  dist.matrix.corner = dist.matrix.corner,
  dist.matrix.col = dist.matrix.col,
  dist.matrix.row = dist.matrix.row,
  dist.matrix.min = dist.matrix.min
)  


###########################################################################
##### PLOTS OF PIXEL SPECIFIC LAYOUT INFORMATION

# Check & visualise of predictors using colored image plot
# check with alternative calculations use sum(as.vector(dist.matrix.col2)-as.vector(dist.matrix.col), na.rm=TRUE)
# note t() used to get orientations right in plots

# To Do: 
# Is image in correct orientation wrt reality?
# Produced pdf load very slowly in pdf viewer. Other file format better? (in the meantime, it's enough to just look at the thumb nails!)

Iw <- c(1:Layout$detector.width)
Ih <- c(1:Layout$detector.height)

pdf(paste(dirPlots, "/", "dummycol_", Layout$name, ".pdf", sep=""))
image(Iw,Ih, t(dummy.matrix.col[Ih,Iw]))
dev.off()   

pdf(paste(dirPlots, "/", "dummyrow_", Layout$name, ".pdf", sep=""))
image(Iw,Ih, t(dummy.matrix.row[Ih,Iw]))
dev.off()   

pdf(paste(dirPlots, "/", "distcentreeucl_", Layout$name, ".pdf", sep=""))
image(Iw,Ih, t(dist.matrix.centre.eucl[Ih,Iw]))
dev.off()    

pdf(paste(dirPlots, "/", "distcentrelinf_", Layout$name, ".pdf", sep=""))
image(Iw, Ih, t(dist.matrix.centre.linf[Ih,Iw]))
dev.off()    

pdf(paste(dirPlots, "/", "distcorner_", Layout$name, ".pdf", sep=""))
image(Iw,Ih, t(dist.matrix.corner[Ih,Iw]))
dev.off()    

pdf(paste(dirPlots, "/", "distedgescol_", Layout$name, ".pdf", sep=""))
image(Iw, Ih, t(dist.matrix.col[Ih,Iw]))
dev.off()

pdf(paste(dirPlots, "/", "distedgesrow_", Layout$name, ".pdf", sep=""))
image(Iw, Ih, t(dist.matrix.row[Ih,Iw]))
dev.off()

pdf(paste(dirPlots, "/", "distedgesmin_", Layout$name, ".pdf", sep=""))
image(Iw, Ih, t(dist.matrix.min[Ih,Iw]))
dev.off()


  
  
  
  
  

