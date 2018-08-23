# phase0_LayoutPixel_test.R: JAB 2018/07/09, edited WSK 2018/07/24
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

###### Paths
# Assumes we start in DetectorChecker directory!
source ("paths.R")

###### Appropriate output directory
dirPlots <- file.path(location, "Output_phase0", "Phase0_LayoutPixels")

##### Source the definition of Layout
source (file.path(location, "Rcode_phase0", "phase0_LayoutExamples_body.R"))

##### Choose Layout by name or index
choice <- "Pilatus"
# or, for example, choice <- 5

##### GENERATE CHOSEN Layout 
Layout <- generate.layout(choice, layout.list)

##### AUXILIARY FUNCTIONS
# Now read from a separate source file, because we'll want to use them seprately later.
source (file.path(location, "Rcode_phase0", "phase0_LayoutPixel_test_auxiliary.R"))


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

# dist.closest.edge <- function(x, size){
#   if ((x<1) | (x>size)){ 
#     cat("Pixel coordinate outside detector.")
#   }
#   return(min(x-1,size-x)) # Why x-1? Because pixel locations start in 1, but we want both edges inside detector for symmetry
# }
# xx <- matrix( sapply(c(1:Layout$detector.width), function(x) dist.closest.edge(x,Layout$detector.width)), nrow=Layout$detector.height, ncol=Layout$detector.width, byrow = TRUE)
# yy <- matrix( sapply(c(1:Layout$detector.height), function(x) dist.closest.edge(x,Layout$detector.height)), nrow=Layout$detector.height, ncol=Layout$detector.width)
# dist.matrix.corner <- sqrt( xx^2 + yy^2 )

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

pdf(file.path(dirPlots, paste("dummycol_", Layout$name, ".pdf", sep="")))
image(Iw,Ih, t(dummy.matrix.col[Ih,Iw]))
dev.off()   

pdf(file.path(dirPlots, paste("dummyrow_", Layout$name, ".pdf", sep="")))
image(Iw,Ih, t(dummy.matrix.row[Ih,Iw]))
dev.off()   

pdf(file.path(dirPlots, paste("distcentreeucl_", Layout$name, ".pdf", sep="")))
image(Iw,Ih, t(dist.matrix.centre.eucl[Ih,Iw]))
dev.off()    

pdf(file.path(dirPlots, paste("distcentrelinf_", Layout$name, ".pdf", sep="")))
image(Iw, Ih, t(dist.matrix.centre.linf[Ih,Iw]))
dev.off()    

pdf(file.path(dirPlots, paste("distcorner_", Layout$name, ".pdf", sep="")))
image(Iw,Ih, t(dist.matrix.corner[Ih,Iw]))
dev.off()    

pdf(file.path(dirPlots, paste("distedgescol_", Layout$name, ".pdf", sep="")))
image(Iw, Ih, t(dist.matrix.col[Ih,Iw]))
dev.off()

pdf(file.path(dirPlots, paste("distedgesrow_", Layout$name, ".pdf", sep="")))
image(Iw, Ih, t(dist.matrix.row[Ih,Iw]))
dev.off()

pdf(file.path(dirPlots, paste("distedgesmin_", Layout$name, ".pdf", sep="")))
image(Iw, Ih, t(dist.matrix.min[Ih,Iw]))
dev.off()


  
  
  
  
  

