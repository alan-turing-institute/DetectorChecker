# hase1_KFG_test.R: JAB 2018/07/18, edited WSK 2018/07/25
#
# CONTENT:
# LIBRARIES, PATHS 
# SPATIAL DESCRIPTIVE STATS: PLOTS OF K, F, G FUNCTIONS AND LAYOUT WITH DAMAGE (FOR REFERENCE)

# WORKFLOW: Assumes already run (including auxiliary functions)
#    - phase0_LayoutExamples.R (for matching Layout example)
#    - phase0_LayoutVisualisation.R
#    - phase0_LayoutPixel.R (do not run the plots again, though)
#    - phase1_PixelReading.R, including consistency checks there
#
# INPUT: 
#    - coo.dead.module (dead pixel coordinates with module indices built phase1_ReadingPixels)
#    - ppp.edges.col, ppp.edges.row
#    - Layout (object built in Phase0 for suitable detector type)
#    - auxiliary variables for layout
#
# 0UTPUT: 
#    - individual plots of K, F, G functions
#    - plot of all the above on one page along wtih dead pixels shown on detector layout (for reference)
#    - inhomogeneous versions of the above
#
# RUN: all


##### LIBRARIES, PATHS

library(spatstat)

###### Paths
# Assumes we start in DetectorChecker directory!
source ("paths.R")

dirData <- file.path(location, "Data_phase1")

source (file.path(location, "Rcode_phase0", "phase0_LayoutExamples_test.R"))
###### Source the work of LayoutVisualisation
source (file.path(location, "Rcode_phase0", "phase0_LayoutVisualisation_test.R"))
###### Source the work of LayoutPixel
source (file.path(location, "Rcode_phase0", "phase0_LayoutPixel_test.R"))
####### Note: in the above, need to stop having to hard-code "Pilatus"

###### Source the work of PixelReadingPilatus
source (file.path(location, "Rcode_phase1", "phase1_PixelReadingPilatus_test.R"))

dirOut <- file.path(location, "Output_phase1", "Phase1_KFG", "Misc", sep="")
if (Layout$name == "Pilatus"){ 
  dirOut <- file.path(location, "Output_phase1", "Phase1_KFG", "Pilatus", sep="")
}

if (substring(Layout$name,1,11) == "PerkinElmer"){
  dirOut <- file.path(location, "Output_phase1", "Phase1_KFG", paste(Layout$name,Layout$date, sep=""))
}


##### PLOTS OF K, F, G FUNCTIONS AND LAYOUT WITH DAMAGE (FOR REFERENCE)

ppp.dead<-ppp(coo.dead[,1],coo.dead[,2],c(1,Layout$detector.width),c(1,Layout$detector.height))

pdf(paste(dirOut,"/",  "layoutDeadKFG_", Layout$name, ".pdf", sep=""))
par(mfrow=c(2,2), mar=c(0,0,2,0), oma=c(1,1,3,0))
  if (sum(Layout$gap.col.sizes)+sum(Layout$gap.row.sizes) == 0){
    plot(ppp.edges.col, pch=".", main="Layout with dead pixels")
    #, cex.main=0.5, main=paste(Layout$name, "\n with damaged pixels\n (black=module edges)")) # vertical lines in x-positions given by xlines
    points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
  } else {
    plot(ppp.edges.col, pch=".", main="Layout with damaged pixels")
       #  main=paste(Layout$name,"with damaged pixels\n (black=module edges, grey=gaps)")) # vertical lines in x-positions given by xlines
    points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
    points(ppp.gaps.col, pch=".", col="grey") # cols without pixels (gaps)
    points(ppp.gaps.row, pch=".", col="grey") # rows without pixels (gaps)
  }
  points(ppp.dead, pch=22, col="brown", cex=0.7)
  par(mar=c(2,2,2,1))
  plot(Kest(ppp.dead), main="K-function")
  plot(Fest(ppp.dead), main="F-function")
  plot(Gest(ppp.dead), main="G-function")
  title(paste(Layout$name, " ",Layout$date, " - ",ppp.dead$n, " dead pixels\n", sep=""), outer=TRUE)  
dev.off()

pdf(paste(dirOut,"/",  "K_", Layout$name, ".pdf", sep=""))
par(mfrow=c(1,1), mar=c(3,3,4,2))
plot(Kest(ppp.dead), 
     main=paste(Layout$name, " ",Layout$date, " - ",ppp.dead$n, " dead pixels\n", 
               "K-function", sep=""))  
dev.off()

pdf(paste(dirOut,"/",  "F_", Layout$name, ".pdf", sep=""))
par(mfrow=c(1,1), mar=c(3,3,4,2))
plot(Fest(ppp.dead), 
     main=paste(Layout$name, " ",Layout$date, " - ",ppp.dead$n, " dead pixels\n", 
                "F-function", sep=""))  
dev.off()

pdf(paste(dirOut,"/",  "G_", Layout$name, ".pdf", sep=""))
par(mfrow=c(1,1), mar=c(3,3,4,2))
plot(Gest(ppp.dead), 
     main=paste(Layout$name, " ",Layout$date, " - ",ppp.dead$n, " dead pixels\n", 
                "G-function", sep=""))  
dev.off()


##### PLOTS OF INHOMOGENOUS K, F, G FUNCTIONS AND LAYOUT WITH DAMAGE (FOR REFERENCE)

ppp.dead<-ppp(coo.dead[,1],coo.dead[,2],c(1,Layout$detector.width),c(1,Layout$detector.height))

pdf(paste(dirOut,"/",  "layoutDeadKFGinhom_", Layout$name, ".pdf", sep=""))
par(mfrow=c(2,2), mar=c(0,0,2,0), oma=c(1,1,3,0))
if (sum(Layout$gap.col.sizes)+sum(Layout$gap.row.sizes) == 0){
  plot(ppp.edges.col, pch=".", main="Layout with dead pixels")
  #, cex.main=0.5, main=paste(Layout$name, "\n with damaged pixels\n (black=module edges)")) # vertical lines in x-positions given by xlines
  points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
} else {
  plot(ppp.edges.col, pch=".", main="Layout with damaged pixels")
  #  main=paste(Layout$name,"with damaged pixels\n (black=module edges, grey=gaps)")) # vertical lines in x-positions given by xlines
  points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
  points(ppp.gaps.col, pch=".", col="grey") # cols without pixels (gaps)
  points(ppp.gaps.row, pch=".", col="grey") # rows without pixels (gaps)
}
points(ppp.dead, pch=22, col="brown", cex=0.7)
par(mar=c(2,2,2,1))
lambda <- density(ppp.dead)
KinhomDead <- Kinhom(ppp.dead, lambda, correction="all")
plot(KinhomDead, cex=0.5,  main="Inhomogeneous K-Function")
FinhomDead <- Finhom(ppp.dead, lambda, correction="all")
plot(FinhomDead, cex=0.5,  main="Inhomogeneous F-Function")
GinhomDead <- Ginhom(ppp.dead, lambda, correction="all")
plot(GinhomDead, cex=0.5, main="Inhomogeneous G-Function")
title(paste(Layout$name, " ",Layout$date, " - ",ppp.dead$n, " dead pixels\n", sep=""), outer=TRUE)  
dev.off()



pdf(paste(dirOut,"/",  "Kinhom_", Layout$name, ".pdf", sep=""))
par(mfrow=c(1,1), mar=c(3,3,4,2))
lambda <- density(ppp.dead)
KinhomDead <- Kinhom(ppp.dead, lambda, correction="all")
plot(KinhomDead, cex=0.5,  main="Inhomogeneous K-Function")
dev.off()

pdf(paste(dirOut,"/",  "Finhom_", Layout$name, ".pdf", sep=""))
par(mfrow=c(1,1), mar=c(3,3,4,2))
FinhomDead <- Finhom(ppp.dead, lambda, correction="all")
plot(FinhomDead, cex=0.5,  main="Inhomogeneous F-Function")
dev.off()

pdf(paste(dirOut,"/",  "Ginhom_", Layout$name, ".pdf", sep=""))
par(mfrow=c(1,1), mar=c(3,3,4,2))
GinhomDead <- Ginhom(ppp.dead, lambda, correction="all")
plot(GinhomDead, cex=0.5, main="Inhomogeneous G-Function")
dev.off()



