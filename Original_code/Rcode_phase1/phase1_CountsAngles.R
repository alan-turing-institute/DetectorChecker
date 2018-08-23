# JAB 2018/07/12
#
# CONTENT:
# LIBRARIES, PATHS 
# PLOTS OF LAYOUT WITH DAMAGED PIXELS
# COUNTS
# DENSITY
# ANGLES
# SCRATCH (see To Do)

# WORKFLOW: Assumes already run (including auxiliary functions)
#    - phase0_LayoutExamples.R (for matching Layout example)
#    - phase0_LayoutVisualisation.R
#    - phase0_LayoutPixel.R (do not run the plots again, though)
#    - phase1_ReadingPixels.R, including consistency checks there
#
# INPUT: 
#    - coo.dead (dead pixel coordinates built phase1_ReadingPixels)
#    - ppp.edges.col, ppp.edges.row
#    - Layout (object built in Phase0 for suitable detector type)
#    - auxiliary variables for layout
#
# 0UTPUT: 
#    - Plot of damaged pixel locations with underlying detector layout
#    - Plots of layout with damaged pixel counts per module 
#    - Rose plots of NN (nearest neighbour) oriented angles for full detector and for individules modules
#    - Damaged pixel locations with NN oriented arrows
#    - Plots comparing DIY and spatstat package function of the previous two (for debugging, see below)
#    - Text file "counts_....txt" with counts in all modules, Chi-square test results, Layout parameters etc.
#
# RUN: 
#   - LIBRARIES, PATHS (user has to adjust some paths names referring to directories in his own computer!)
#   - VISUALISATION OF DAMAGED PIXELS WITH LAYOUT
# 
# To Do (error/debugging): 
#   - ANGLES was initially written using nnorient() from spatstat.
#   - But plots produced there with nnorient() are clearly wrong for some examples of individual modules (or all),
#     and it often actually just returns errors and stops running.
#   - Less obvious for full detector plot, but likely also wrong.
#   - May have to write that up to report (packge bug report at github.com/spatstat/spatstat).
#   - In the meantime (or for good): DIY version actually works fine!
#   - For comparison/debugging nnorient() from spatstat, see under SCRATCH below. 
#   - Also, while DIY is running, angles may be rotated and 
#     there may also be some error for some detector types (investigate further!)


##### LIBRARIES, PATHS

library(spatstat)
library(MASS)

dirAnalysis<-"/Users/julia/Dropbox/Research/Projects2014+/DetectorChecker/Phase1/"
setwd(dirAnalysis)
dirData <- paste(dirAnalysis, "Data_phase1/", sep="")

dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_CountsAngles/Misc/", sep="")
if (Layout$name == "Pilatus"){ 
  dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_CountsAngles/Pilatus/", sep="")
}

if (substring(Layout$name,1,11) == "PerkinElmer"){
  dirOut <- paste(dirAnalysis, "Output_phase1/Phase1_CountsAngles/",Layout$name,Layout$date,"/", sep="")
}


##### PLOTS OF LAYOUT WITH DAMAGED PIXELS

ppp.dead<-ppp(coo.dead[,1],coo.dead[,2],c(1,Layout$detector.width),c(1,Layout$detector.height))

pdf(paste(dirOut,  "/", "layoutDamaged_", Layout$name, ".pdf", sep=""))
if (sum(Layout$gap.col.sizes)+sum(Layout$gap.row.sizes) == 0){
  plot(ppp.edges.col, pch=".", cex.main=0.7, main=paste(Layout$name, "with damaged pixels\n (black=module edges)")) # vertical lines in x-positions given by xlines
  points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
} else {
  plot(ppp.edges.col, pch=".", cex.main=0.7, 
       main=paste(Layout$name,"with damaged pixels\n (black=module edges, grey=gaps)")) # vertical lines in x-positions given by xlines
  points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
  points(ppp.gaps.col, pch=".", col="grey") # cols without pixels (gaps)
  points(ppp.gaps.row, pch=".", col="grey") # rows without pixels (gaps)
}
points(ppp.dead, pch=22, col="brown", cex=0.7)
dev.off()

# Question:  
# Instead of pch=22 (empty square) would like dead pixels
# in full but opaque squares (pch=15)like below (works!)
# plot(ppp.dead, pch=15,  cex=0.7, transparent=TRUE)
# Tried using par transparent and alpha also in points(), but but there is does not work
# Changing order of plot() and points() above is not a way out, 
# because of the titles and because they layout should be printed under the 
# damaged pixels rather than cover them up.



##### COUNTS

### Count number of damaged pixels overall and in different modules

module.count.array <- quadratcount(X = ppp.dead, nx = Layout$module.col.n, ny=Layout$module.row.n)  #returns the count of points in each quadrat
module.count <- as.vector(module.count.array)  #returns the count of points in each quadrat
module.n <- Layout$module.col.n * Layout$module.row.n

write.matrix(module.count.array, file=paste(dirOut, "/", "counts_", Layout$name, ".txt", sep=""))
sink(paste(dirOut, "/", "counts_", Layout$name, ".txt", sep=""), append=TRUE)
cat("\n") 
cat("Total number of damaged pixels: ", dead.n, "\n")
cat("Total number of modules: ", module.n, "\n")
cat("Average number of damaged pixels per module: ", round(ppp.dead$n/module.n, digits=1), "\n")
cat("Chi square test results:\n")
chisq.test(x=module.count, p=rep(1/module.n, module.n))
cat("\n") 
cat("Detector:\n")
cat("Name: ", Layout$name, "\n")
cat("Date: ", Layout$date, "\n")
cat("Width: ", Layout$detector.width, "\n")
cat("Height: ", Layout$detector.height, "\n")
cat("Number of columns in array of module (= number of modules per row): ", Layout$module.col.n, "\n")
cat("Number of rows in array of module (= number of modules per column): ", Layout$module.row.n, "\n")
cat("Widths of modules: ", Layout$module.col.sizes, "\n")
cat("Heights of modules: ", Layout$module.row.sizes, "\n")
cat("Widths of gaps between modules: ", Layout$gap.col.sizes, "\n")
cat("Heights of gaps between modules: ", Layout$gap.row.sizes, "\n")
sink()


### Distribution of points per module in various representations

rotate <- function(x) t(apply(x, 2, rev))

pdf(paste(dirOut, "/", "countsModules_", Layout$name, ".pdf", sep=""), bg = "transparent")
par(mfrow=c(1,1), mar=c(1,1,3,1))
plot(module.count.array, main = paste("Number of damaged pixels in modules\n",
     "Total number damaged pixels: ", ppp.dead$n, 
     "\n (average per module: ", round(ppp.dead$n/module.n, digits=1),")")) 
dev.off()

pdf(paste(dirOut, "/", "countsModulesNotitle_", Layout$name, ".pdf", sep=""), bg = "transparent")
par(mfrow=c(1,1), mar=c(1,1,3,1))
plot(module.count.array, main="") 
dev.off()

pdf(paste(dirOut, "/", "countsModulesHist_", Layout$name, ".pdf", sep=""), bg = "transparent")
par(mfrow=c(1,1), mar=c(3,3,3,1))
hist(module.count, breaks=25, main="Distribution of counts in modules",
     xlab="Number of damaged pixels in module")
dev.off()

pdf(paste(dirOut, "/", "countsModulesImage_", Layout$name, ".pdf", sep=""), bg = "transparent")
par(mfrow=c(1,1), mar=c(1,1,3,1))
image( rotate( module.count.array ), axes=F, col=heat.colors(12),
       main="Counts for individual modules (darker = more points)")
dev.off()


##### DENSITY

# Full detector

pdf(paste(dirOut, "/", "density_", Layout$name, ".pdf", sep=""), bg = "transparent")
image(density(ppp.dead), main="Dead pixel density")
dev.off()

pdf(paste(dirOut, "/", "density025_", Layout$name, ".pdf", sep=""), bg = "transparent")
image(density(ppp.dead, adjust=0.25), main="Dead pixel density, adjust=0.25")
dev.off()

# Individual modules 

for (i in 1:Layout$module.col.n){
  for (j in 1:Layout$module.row.n){
    # goes through modules in the order: start bottom left, move up, start over 2nd col, move up...
    # restrict coo.dead to selected module i,j
    coo.dead.module.select <- coo.dead.module[coo.dead.module[,3]==i & coo.dead.module[,4]==j,]
    ppp.dead.module.select <- ppp(coo.dead.module.select[,1],coo.dead.module.select[,2],
                                  c(Layout$module.edges.col[1,i],Layout$module.edges.col[2,i]),
                                  c(Layout$module.edges.row[1,j],Layout$module.edges.row[2,j]))   
    pdf(paste(dirOut, "/", "densityIndivModules_",i,"_",j,"_", 
              Layout$name, ".pdf", sep=""), bg = "transparent")
    par(mfrow=c(1,1), mar=c(1,1,3,1))
    image(density(ppp.dead.module.select), 
          main=paste("Module in layout position col=",i, " row=", j, "\n", 
                      ppp.dead.module.select$n, " dead pixels\n", sep=""))
    dev.off()
  } 
}  



##### ANGLES 

# This is a DIY version.
# Version using nnorient() from spatstat see under SCRATCH.
# Using nnorient from spatstat would be short, but can not currently been used,
# because it produces errors for some of the individual modules (in Pilatus and Escalibur and probably also in others)
# and because the results also seem to be incorrect. 
# See more in comparison plots under SCRATCH.

### Auxilary functions (geometry with vectors and nn)

norm.vec <- function(v) sqrt(sum(v^2))
dist.vec <- function(v,w) norm.vec(v-w)

# Calculates distance and orientation of the oriented vector between two points 
# in order of the second pointing to first (reflecting nearest neighbour (nn) framework)
# v,w point coordinates indicating vectors wrt to the origin. 
# Values: distance and orientation (in [0,360) degrees) of w pointing towards v. 
orientdist.vec <- function(v,w){
  v <- v-w
  w <- c(0,0)
  tmp1 <- norm.vec(v)
  v <- v/tmp1
  x<-v[1]
  y<-v[2]
  tmp2 <- asin(abs(y))*180/pi
  if (x>=0){
    if (y<0) {tmp2 <- 360-tmp2}
  } else {tmp2 <- 180 - sign(y)*tmp2}
  orientdist.vec <- list(tmp2, tmp1) 
  names(orientdist.vec) <- c("orient", "dist")
  return(orientdist.vec)
}

# Calculates the angle between two vectors (in degrees)
# v, w are coordinates describe vectors wrt origin (0,0)
# undefined if one of the vectors is collapsed to nothing (referred to be coordinates (0,0))
angle.vec <- function(v,w){ 
  if (norm.vec(v)*norm.vec(w)==0) 
    cat("Point coordinates referring to vectors can not be in the origin (0,0).")
  return(as.numeric( acos( as.numeric((v%*%w  / (norm.vec(v) * norm.vec(w))))))*180/pi )
}

orientcolfct <- function(b) orientdist.vec(b[1:2],b[3:4])$orient

#PPPdata <- ppp.dead

orientnnPPP <- function(PPPdata){
  PPPnn <- PPPdata[nnwhich(PPPdata)]
  #now calculate our own thing for the orientations to compare
  A <- matrix(c(PPPdata$x,PPPdata$y), nrow=2, ncol=length(PPPdata$x), byrow=TRUE)
  # x,y values of original point pattern
  Ann <- matrix(c(PPPnn$x,PPPnn$y), nrow=2, ncol=length(PPPnn$x), byrow=TRUE)
  # x,y values of point pattern containing nn of each of the points in original
  # Assigns a point patters (ppp object) a vector of the orientations of the arrows pointing from nearest neighbours to its points
  return(round(apply(rbind(A,Ann), 2, orientcolfct), digits=3))
}


### Full detector (DIY)

pdf(paste(dirOut, "/", "anglesNNDetectorRose_DIY_", Layout$name, ".pdf", sep=""), bg = "transparent")
  par(mfrow=c(1,1), mar=c(1,1,3,1))
  rose(orientnnPPP(ppp.dead), main="NN oriented angles", breaks=72)
dev.off()

pdf(paste(dirOut, "/", "orientNNDetector_DIY_", Layout$name, ".pdf", sep=""), bg = "transparent")
  par(mfrow=c(1,1), mar=c(1,1,3,1))
  PPPnn <- ppp.dead[nnwhich(ppp.dead)]
  plot(ppp.dead, main="NN oriented arraows")
  arrows(PPPnn$x, PPPnn$y, ppp.dead$x, ppp.dead$y, angle=15, length=0.07, col="red")
dev.off()


### Indivitual modules (DIY)

# Assign module to each dead pixels

coo.dead.modules <- data.frame(coo.dead,NA,NA)
colnames(coo.dead.modules) <- c("pixcol", "pixrow", "modcol", "modrow")

for (i in 1:ppp.dead$n){
  tmp <- module.which.indices(coo.dead[i,1], coo.dead[i,2], Layout$module.edges.col, Layout$module.edges.row)
  coo.dead.modules[i,3] <- tmp$col
  coo.dead.modules[i,4] <- tmp$row
}   # To Do: more elegant with lapply or plyr etc

# Make angle histograms (rose plots) and pixel plots with arrows

for (i in 1:Layout$module.col.n){
  for (j in 1:Layout$module.row.n){
    # goes through modules in the order: start bottom left, move up, start over 2nd col, move up...
    coo.dead.module.select <- coo.dead.module[coo.dead.module[,3]==i & coo.dead.module[,4]==j,]
    ppp.dead.module.select <- ppp(coo.dead.module.select[,1],coo.dead.module.select[,2],
                                  c(Layout$module.edges.col[1,i],Layout$module.edges.col[2,i]),
                                  c(Layout$module.edges.row[1,j],Layout$module.edges.row[2,j]))   
    if (ppp.dead.module.select$n >= 2){  # need at least 2 points to calculate neighbours!
      cat(i,",",j," yes *\n")
      PPPnn <- ppp.dead.module.select[nnwhich(ppp.dead.module.select)]
      pdf(paste(dirOut, "/", "anglesOrientNNindivModules_DIY_",i,"_",j,"_", 
                Layout$name, ".pdf", sep=""), bg = "transparent")
      par(mfrow=c(1,2), mar=c(1,1,5,1), oma=c(1,1,5,1))
      plot(ppp.dead.module.select, main="NN oriented arrows")
      arrows(PPPnn$x, PPPnn$y, ppp.dead.module.select$x, ppp.dead.module.select$y, angle=15, length=0.07, col="red")
      rose(orientnnPPP(ppp.dead.module.select), breaks=72, main="NN oriented angles")
      title(paste("Module in layout position col=",i, " row=", j, "\n", ppp.dead.module.select$n, " dead pixels\n", sep=""), outer=TRUE)
      dev.off()
    }
  }
}



########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
###########################  SCRATCH  SCRATCH  SCRATCH  ########################### ########################### 
########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 


##### ANGLES using nnorient() from spatstat package 
# Runs fine for full detector, but produces errors for some(!) of the individual modules and wrong results (probably for all of them)!

### Full detector - 

pdf(paste(dirOut, "/", "anglesNNDetectorRose_", Layout$name, ".pdf", sep=""), bg = "transparent")
par(mfrow=c(1,1), mar=c(1,1,3,1))
rose(nnorient(ppp.dead, sigma=4), col="grey", 
     main=paste( "NN to points orientations ", ppp.dead$n, " dead pixels\n", sep=""))
dev.off()


### Individual modules 

# Make angle histograms (rose plots)

# Errors: This does not work for all modules, and it's also not clear (visual check) if the results in rose() are correct!!!
for (i in 1:Layout$module.col.n){
  for (j in 1:Layout$module.row.n){
    # goes through modules in the order: start bottom left, move up, start over 2nd col, move up...
    coo.dead.module.select <- coo.dead.module[coo.dead.module[,3]==i & coo.dead.module[,4]==j,]
    ppp.dead.module.select <- ppp(coo.dead.module.select[,1],coo.dead.module.select[,2],
                                      c(Layout$module.edges.col[1,i],Layout$module.edges.col[2,i]),
                                      c(Layout$module.edges.row[1,j],Layout$module.edges.row[2,j]))    
    pdf(paste(dirOut, "/", "anglesNNDetectorRose_",i,"_",j,"_", Layout$name, ".pdf", sep=""), bg = "transparent")
    par(mfrow=c(1,2), mar=c(1,1,3,1))
    plot(ppp.dead.module.select)
    if (ppp.dead.module.select$n >=2){
      rose(nnorient(ppp.dead.module.select, sigma=4), col="grey", 
           main=paste("Module ",i,",", j, "\n", ppp.dead.module.select$n, " dead pixels\n", 
                      "NN to points orientations", sep=""))
    }
    dev.off()  
  }
}


########################### COMPARISON with DIY method


### Full detector (comparing methods)

PPPnn <- ppp.dead[nnwhich(ppp.dead)]

pdf(paste(dirOut, "/", "compareRcodeAnglesOrient_rose_", 
          Layout$name, ".pdf", sep=""), bg = "transparent")
par(mfrow=c(1,2), mar=c(1,1,10,1), oma=c(1,1,5,1))
rose(orientnnPPP(ppp.dead), breaks=72, main="NN oriented angles (DIY)")
rose(nnorient(ppp.dead, sigma=4), col="grey", main="NN angles (spatstat)")
title(paste("Full detector\n", ppp.dead$n, " dead pixels\n", sep=""), outer=TRUE)
dev.off()
pdf(paste(dirOut, "/", "compareRcodeAnglesOrient_points_", 
          Layout$name, ".pdf", sep=""), bg = "transparent")
par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(1,1,5,1))
plot(ppp.dead, main="NN oriented arrows")
arrows(PPPnn$x, PPPnn$y, ppp.dead$x, ppp.dead$y, angle=15, length=0.07, col="red")
title(paste("Full detector\n", ppp.dead$n, " dead pixels\n", sep=""), outer=TRUE)
dev.off()


### Indivitual modules (comparing methods) 
# Loop may stop running half way because of nnorient() errors, 
# but this delivers some plots to compare methods and visualise incorrect results by nnorient() based code

for (i in 1:Layout$module.col.n){
  for (j in 1:Layout$module.row.n){
    # goes through modules in the order: start bottom left, move up, start over 2nd col, move up...
    coo.dead.module.select <- coo.dead.module[coo.dead.module[,3]==i & coo.dead.module[,4]==j,]
    ppp.dead.module.select <- ppp(coo.dead.module.select[,1],coo.dead.module.select[,2],
                                  c(Layout$module.edges.col[1,i],Layout$module.edges.col[2,i]),
                                  c(Layout$module.edges.row[1,j],Layout$module.edges.row[2,j]))   
    if (ppp.dead.module.select$n >= 2){  # need at least 2 points to calculate neighbours!
      cat(i,",",j," yes *\n")
      PPPnn <- ppp.dead.module.select[nnwhich(ppp.dead.module.select)]
      pdf(paste(dirOut, "/", "compareRcodeAnglesOrientIndividualModules_",i,"_",j,"_", 
                Layout$name, ".pdf", sep=""), bg = "transparent")
      par(mfrow=c(1,3), mar=c(1,1,5,1), oma=c(1,1,5,1))
      plot(ppp.dead.module.select, main="NN oriented arrows")
      arrows(PPPnn$x, PPPnn$y, ppp.dead.module.select$x, ppp.dead.module.select$y, angle=15, length=0.07, col="red")
      rose(orientnnPPP(ppp.dead.module.select), breaks=72, main="NN oriented angles (DIY)")
      rose(nnorient(ppp.dead.module.select, sigma=4), col="grey", main="NN angles (spatstat)")
      title(paste("Module in layout position col=",i, " row=", j, "\n", ppp.dead.module.select$n, " dead pixels\n", sep=""), outer=TRUE)
      dev.off()
    }
  }
}

for (i in 1:Layout$module.col.n){
  for (j in 1:Layout$module.row.n){
    # goes through modules in the order: start bottom left, move up, start over 2nd col, move up...
    coo.dead.module.select <- coo.dead.module[coo.dead.module[,3]==i & coo.dead.module[,4]==j,]
    ppp.dead.module.select <- ppp(coo.dead.module.select[,1],coo.dead.module.select[,2],
                                  c(Layout$module.edges.col[1,i],Layout$module.edges.col[2,i]),
                                  c(Layout$module.edges.row[1,j],Layout$module.edges.row[2,j]))   
    pdf(paste(dirOut, "/", "anglesNNDetectorRose_",i,"_",j,"_", Layout$name, ".pdf", sep=""), bg = "transparent")
    par(mfrow=c(1,2), mar=c(1,1,3,1))
    plot(ppp.dead.module.select)
    if (ppp.dead.module.select$n >=2){
      rose(nnorient(ppp.dead.module.select, sigma=4), col="grey", 
           main=paste("Module ",i,",", j, "\n", ppp.dead.module.select$n, " dead pixels\n", 
                      "NN to points orientations", sep=""))
    }
    dev.off() 
  }
}  







