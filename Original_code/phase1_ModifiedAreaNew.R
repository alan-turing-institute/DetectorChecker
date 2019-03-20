modifyArea <- function(pePPP, MinPts=30, epsAdjust=0.05)
{
  A <- cbind((pePPP$Pixels)$x, (pePPP$Pixels)$y)
  
  epsLayout <- min(Layout$detector.width, Layout$detector.length)*epsAdjust
  res <- dbscan(A, eps = epsLayout, MinPts)    #border points are included, not noise in this context 
  
  Nclusters <- length(unique(res$cluster))

  par(mfrow=c(1,1), mar=c(0,0,3,0)+0.1, oma=c(0,0,0,0))

  pdf(file="areasHighDamage.pdf")
    plot(A, pch=22, cex=0.8, col = res$cluster + 1L,
         main="High damage areas", xlab="", ylab="")
  dev.off()
  
  pdf(file="areasHighDamageHull.pdf")
  hullplot(A, res, pch=22, 
           main="High damage areas with convex hulls", xlab="", ylab="")
  dev.off()
  
  A0 <- A[res$cluster == 0, ]

  Tbin_0 <- coo2bin(A0)              # make binary mask from coordinates
  PixelsEvents_0 <- bin2cooEvents(Tbin_0, FALSE, FALSE)    # make object with dead pixel and event coo (without outputting plots)
  pePPP_0 <- pixelsEvents2ppp(PixelsEvents_0)
  
  return(pePPP_0)
}

# RUN
pePPPmodArea<- modifyArea(pePPP, MinPts=30)