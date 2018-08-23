# phase0_LayoutPixel_test_auxiliary.R: JAB 2018/07/09, edited WSK 2018/07/24
#
# AUXILIARY FUNCTIONS

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


dist.closest.edge <- function(x, size){
  if ((x<1) | (x>size)){ 
    cat("Pixel coordinate outside detector.")
  }
  return(min(x-1,size-x)) # Why x-1? Because pixel locations start in 1, but we want both edges inside detector for symmetry
}
xx <- matrix( sapply(c(1:Layout$detector.width), function(x) dist.closest.edge(x,Layout$detector.width)), nrow=Layout$detector.height, ncol=Layout$detector.width, byrow = TRUE)
yy <- matrix( sapply(c(1:Layout$detector.height), function(x) dist.closest.edge(x,Layout$detector.height)), nrow=Layout$detector.height, ncol=Layout$detector.width)
dist.matrix.corner <- sqrt( xx^2 + yy^2 )

