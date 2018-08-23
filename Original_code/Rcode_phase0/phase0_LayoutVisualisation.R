# JAB 2018/07/09
#
# CONTENT:
# LIBRARIES, PATHS 
# AUXILIARY FUNCTIONS
# VISUALISE DETECTOR LAYOUT

# INPUT: Layout (object)
#
# 0UTPUT: plots
#
# RUN: 
#   - LIBRARIES, PATHS (user has to adjust some paths names referring to directories in his own computer!)
#   - AUXILIARY FUNCTIONS
#   - VISUALISE DETECTOR LAYOUT


##### LIBRARIES, PATHS

library(spatstat)

dirAnalysis<-"/Users/julia/Dropbox/Research/Projects2014+/DetectorChecker/Phase0/"
dirPlots<-"/Users/julia/Dropbox/Research/Projects2014+/DetectorChecker/Phase0/Output_phase0/Phase0_LayoutVisualisation/"
setwd(dirAnalysis)


##### AUXILIARY FUNCTIONS

### Define point patterns (spatstat) capturing module edges (including coinciding with detector boundary)

# Calls functions:
# edges()

vedges <- as.vector(Layout$module.edges.col)
ytmp <- rep(1:Layout$detector.height, length(vedges))
xtmp <- rep(vedges, rep(Layout$detector.height, length(vedges)))
ppp.edges.col <-ppp(xtmp, ytmp, c(1,Layout$detector.width),c(1,Layout$detector.height))

vedges <- as.vector(Layout$module.edges.row)
xtmp <- rep(1:Layout$detector.width, length(vedges))
ytmp <- rep(vedges, rep(Layout$detector.width, length(vedges)))
ppp.edges.row <-ppp(xtmp, ytmp, c(1,Layout$detector.width),c(1,Layout$detector.height))

### Define point patterns (spatstat) capturing gaps 

if (sum(Layout$gap.col.sizes) != 0){
vgaps <- c()
for (i in 1:(Layout$module.col.n-1)){
  vgaps <- c( vgaps, (Layout$module.edges.col[2,i]+1):(Layout$module.edges.col[1,i+1]-1) )
}
ytmp <- rep(1:Layout$detector.height, length(vgaps))
xtmp <- rep(vgaps, rep(Layout$detector.height, length(vgaps)))
ppp.gaps.col <-ppp(xtmp, ytmp, c(1,Layout$detector.width),c(1,Layout$detector.height))
}

if (sum(Layout$gap.row.sizes) != 0){
vgaps <- c()
for (i in 1:(Layout$module.row.n-1)){
  vgaps <- c(vgaps, (Layout$module.edges.row[2,i]+1):(Layout$module.edges.row[1,i+1]-1) )
}
xtmp <- rep(1:Layout$detector.width, length(vgaps))
ytmp <- rep(vgaps, rep(Layout$detector.width, length(vgaps)))
ppp.gaps.row <-ppp(xtmp, ytmp, c(1,Layout$detector.width),c(1,Layout$detector.height))
}    


##### VISUALISE DETECTOR LAYOUT

pdf(paste(dirPlots, "/", "layout_", Layout$name, ".pdf", sep=""))
if (sum(Layout$gap.col.sizes)+sum(Layout$gap.row.sizes) == 0){
plot(ppp.edges.col, pch=".", cex.main=0.7, main=paste(Layout$name, "layout\n (black=module edges)")) # vertical lines in x-positions given by xlines
points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
dev.off()
} else {
plot(ppp.edges.col, pch=".", cex.main=0.7, main=paste(Layout$name,"layout\n (black=module edges, grey=gaps)")) # vertical lines in x-positions given by xlines
points(ppp.edges.row, pch=".") # horizontal lines in y-positions given by ylines
points(ppp.gaps.col, pch=".", col="grey") # cols without pixels (gaps)
points(ppp.gaps.row, pch=".", col="grey") # rows without pixels (gaps)
dev.off()
}

