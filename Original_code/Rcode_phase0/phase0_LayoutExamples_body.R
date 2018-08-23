# phase0_LayoutExamples_body.R: JAB 2018/07/09, edited WSK 2018/07/25
#
# CONTENT:
# LIBRARIES, PATHS 
# AUXILIARY FUNCTIONS
# LAYOUT OBJECT DEFINITION
# EXAMPLES 
# DERIVED PARAMETERS
# CONSISTENCY CHECKS

# 0UTPUT: Layout (object), checks, plots
#
# RUN: 
#   - LIBRARIES, PATHS (user has to adjust some paths names referring to directories in his own computer!)
#   - AUXILIARY FUNCTIONS
#   - pick only one from EXAMPLES at a time and run only(!) that one
#   - DERIVED PARAMETERS (to complete object)

##### LIBRARIES, PATHS

##### SIMPLE UNIT TEST
# generate.layout("Pilatus", layout.list)
# $name
# [1] "Pilatus"
# 
# $detector.width
# [1] 2527
# 
# $detector.height
# [1] 2463
# 
# $module.col.n
# [1] 12
# 
# $module.row.n
# [1] 5
# 
# $module.col.sizes
#  [1] 195 195 195 195 195 195 195 195 195 195 195 195
# 
# $module.row.sizes
# [1] 487 487 487 487 487
# 
# $gap.col.sizes
#  [1] 17 17 17 17 17 17 17 17 17 17 17
# 
# $gap.row.sizes
# [1] 7 7 7 7
# 
# $module.edges.col
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# left     1  213  425  637  849 1061 1273 1485 1697  1909  2121  2333
# right  195  407  619  831 1043 1255 1467 1679 1891  2103  2315  2527
# 
# $module.edges.row
#        [,1] [,2] [,3] [,4] [,5]
# top       1  495  989 1483 1977
# bottom  487  981 1475 1969 2463
# 
# $detector.inconsistency
# [1] 0


##### AUXILIARY FUNCTIONS

# From module and gap sizes, create coordinates of locations of edges.
# Function is in 1d context to be applied to rows and cols separately.
# Edges are inside the modules (first/last row/col of module). 
# m vector of module sizes, g vectors of gap sizes

edges <- function(m, g){
  if (length(m)-1 != length(g)){
    cat("\n Number of modules or gaps incorrect. \n")
    bpts <- NA
  }
  else {
    edges <- matrix(nrow=2, ncol=length(m))
    edges[1,1] <- 1
    edges[2,1] <- m[1]
    for (i in 2:length(m)){ 
      edges[1,i] <- edges[2,i-1]+g[i-1]+1
      edges[2,i] <- edges[1,i]-1+m[i]
    }
  }
  return(edges)
}

# The next function returns the completion of a given layout object.
# If not applied, there will be errors down the road!
# Conditions additional elements of Layout object that are frequently used later
# They are calculated from parameters defined in examples
# Matrices that contains xy coordiantes of edges of modules 
# By definition, edges are part of modules (not part of gaps)
# i.e. for each module two pairs: first/last col and first/last row.

derive.layout <- function(layout){
  module.edges.col <- edges(layout$module.col.sizes, layout$gap.col.sizes)

  dimnames(module.edges.col)[[1]] <- c("left", "right")

  module.edges.row <- edges(layout$module.row.sizes, layout$gap.row.sizes)  # displayed in transposed (rows are listed in columns)
  dimnames(module.edges.row)[[1]] <- c("top", "bottom")

  layout$module.edges.col <- module.edges.col
  layout$module.edges.row <- module.edges.row
  
  return(layout)
}

# The next function runs basic checks as to whether parameters entered (slightly redundant on purpose) add up.
# To Do: Warnings should appear in color red or in some other say (dialog box etc) be more noticeable.

check.layout <- function(layout){
  detector.inconsistency <- 0   # counts inconsistencies found

  # Total size of detector
  if (layout$detector.width != sum(layout$module.col.sizes) + sum(layout$gap.col.sizes)){
    cat("Dectector width is not the sum or the widths of the modules and the gaps between them.
         Please check if you entered the correct sizes.")
    detector.inconsistency <- detector.inconsistency + 1
  } 
  if (layout$detector.height != sum(layout$module.row.sizes) + sum(layout$gap.row.sizes)){
    cat("Dectector height is not the sum or the heights of the modules and the gaps between them.
         Please check if you entered the correct sizes.")
    detector.inconsistency <- detector.inconsistency + 1
  }

  # Module numbers and size vectors
  if (layout$module.col.n != length(layout$module.col.sizes)){
    cat("Number of modules per row does not match the length of the vector of their widths.")
    detector.inconsistency <- detector.inconsistency + 1
  }
  if (layout$module.row.n != length(layout$module.row.sizes)){
    cat("Number of modules per column does not match the length of the vector of their heights.")
    detector.inconsistency <- detector.inconsistency + 1
  }
  return (detector.inconsistency)
}

# Finally, this function generates a specified layout.
generate.layout <- function(layout.name, layout.list){
  names <- names(layout.list)
  if (! (layout.name %in% names)){
    valid.names <- paste(names, sep = ',\n\t', collapse = ',\n\t')
    stop(paste("\n", layout.name, " not in list of valid names\n\t", valid.names, "\n", sep=""))
  }
  layout <- layout.list[[layout.name]]
  
  layout <- derive.layout(layout)

  layout$detector.inconsistency <- check.layout(layout)

  return(layout)
}

##### LAYOUT OBJECT PARAMETERS (REAL WORLD EXAMPLES AND SAMPLE SET-UPS)
##### General class of detector type with layout parameters entered by user

# Assumptions: 
# Panels consist of modules and gaps. 
# Rectangular detector panel composed by a grid of modules seperated by gaps.
# Sizes of gaps and modules can vary in size as long as this still forms a grid, 
# i.e. the widths of the gaps and the modules in the same across all rows of the panel,
# and the hights of the gaps and the modules are the same across all columns of the panel
# To do (if necessary): generalise to non-grid structure

# Question: What would an engineer like to enter?
# We let them enter a bit more and use reduncancy to check for consistency.
# Assume as given in example. Or maybe need to detect from image?

# Empty object for illustration
# No need to run this
layout.template <- list(
  name = NA,
  date = NA,
  detector.width =  NA,
  detector.height = NA,
  module.col.n = NA,  # number of columns in the grid of modules
  module.row.n = NA,  # number of rows in the grid of modules
  module.col.sizes = NA, # vector with widths of the modules
  module.row.sizes = NA, # vector with heights of the modules
  gap.col.sizes = NA, # vector with widths of the gaps
  gap.row.sizes = NA, # vector with heights of the gaps
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA  # counts inconsistencies found in parameters entered, needs to be 0 to proceed
  # To Do: ,exclusion.area = NA # corner coordinates of rectangular area to exclude from analysis (clockwise starting in lower left corner) 
)


##### EXAMPLES 
# (contains real world detectors and sample set ups)

### SELECT ONE & RUN THAT ONLY

#####################################################################
### Example: Excalibur
layout <- list(
  name = "Excalibur",
  detector.width =  2048,
  detector.height = 1536,
  module.col.n = 8,  # number of columns in the grid of modules
  module.row.n = 6,  # number of rows in the grid of modules
  gap.col.sizes = rep(0,7), # vector with widths of the gaps
  gap.row.sizes = rep(0,5), # vector with heights of the gaps (guessed!)
  module.col.sizes = rep(256, 8), # vector with widths of the modules
  module.row.sizes = rep(256, 6), # vector with heights of the modules
  module.edges.col= NA, #
  module.edges.row= NA,  #
  detector.inconsistency = NA   
)
layout.list = list(layout)
#####################################################################


#####################################################################
### Example: PerkinElmer 
# Careful in real image examples this may deviate from the prescribed layout, 
# because engineers chop crop parts of the detector.

layout <- list(
  name = "PerkinElmerFull",
  date = NA,
  detector.width =  2000,
  detector.height = 2000,
  module.col.n = 16,  # number of columns in the grid of modules
  module.row.n = 2,  # number of rows in the grid of modules
  module.col.sizes = c(104,rep(128,14),104), # vector with widths of the modules
  module.row.sizes = rep(1000,2), # vector with heights of the modules
  gap.col.sizes = rep(0,15), # vector with widths of the gaps
  gap.row.sizes = c(0), # vector with heights of the gaps
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################

#####################################################################
layout <- list(
  name = "PerkinElmerCropped1600",
  date = NA, 
  detector.width =  2000,
  detector.height = 1600,
  module.col.n = 16,  # number of columns in the grid of modules
  module.row.n = 2,  # number of rows in the grid of modules
  module.col.sizes = c(104,rep(128,14),104), # vector with widths of the modules
  module.row.sizes = rep(800,2), # vector with heights of the modules
  gap.col.sizes = rep(0,15), # vector with widths of the gaps
  gap.row.sizes = c(0), # vector with heights of the gaps
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################

#####################################################################
layout <- list(
  name = "PerkinElmerRefurbished",
  date = NA,
  detector.width =  2000,
  detector.height = 2000,
  module.col.n = 16,  # number of columns in the grid of modules
  module.row.n = 2,  # number of rows in the grid of modules
  module.col.sizes = c(rep(128,15),80), # vector with widths of the modules
  module.row.sizes = c(1004,996), # vector with heights of the modules
  gap.col.sizes = rep(0,15), # vector with widths of the gaps
  gap.row.sizes = c(0), # vector with heights of the gaps
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################


#####################################################################
### Example: Pilatus 
module.width = 195 # this is just for equally wide modules
module.height = 487 # this is just for equally heigh modules
layout <- list(
name = "Pilatus",
detector.width =  2527,
detector.height = 2463,
module.col.n = 12,  # number columns in the array of modules
module.row.n = 5,   # number rows in the array of modules
module.col.sizes = rep(module.width, 12), #in this example all are equally wide
module.row.sizes = rep(module.height, 5), #in this example all are equally high
gap.col.sizes = rep(17,11), #in this example all equal
gap.row.sizes = rep(7,4), #in this example all equal
module.edges.col = NA, #
module.edges.row = NA,  #
detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################


#####################################################################
### Example: regular detector without gaps (hypothetical test object)
module.width = 195 # this is just for equally wide modules
module.height = 487 # this is just for equally heigh modules
layout <- list(
  name = "Sample Detector (regular without gaps)", 
  detector.width =  2340,
  detector.height = 2435,
  module.col.n = 12,  
  module.row.n = 5,   
  module.col.sizes = rep(module.width, 12),
  module.row.sizes = rep(module.height, 5),
  gap.col.sizes = rep(0,11), 
  gap.row.sizes = rep(0,4),
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################


#####################################################################
### Example: regular detector with gaps (hypothetical test object)
module.width <- 200 # equally wide modules
module.height <- 100 # equally heigh modules
gap.width <- 10 # equally wide gaps
gap.height <- 20 # equally high gaps
layout <- list(
  name = "Sample Detector (regular with gaps)", 
  detector.width =  1040,
  detector.height = 1180,
  module.col.n = 5,  
  module.row.n = 10,   
  module.col.sizes = rep(module.width, 5),
  module.row.sizes = rep(module.height, 10),
  gap.col.sizes = rep(gap.width,4), 
  gap.row.sizes = rep(gap.height,9),
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################


#####################################################################
### Example: irregular detector without gaps (hypothetical test object)
layout <- list(
  name = "Sample Detector (irregular without gaps)", 
  detector.width = 1200,
  detector.height = 1000,
  module.col.n = 10,  # number columns in the array of modules
  module.row.n = 7,   # number rows in the array of modules
  module.col.sizes = c(200, rep(50,2), rep(150,4), rep(50,2), 200), #in this example not all are equally wide
  module.row.sizes = c(250, 150, 75, 50, 75, 150, 250), #in this example not all are equally high
  gap.col.sizes = rep(0,9), 
  gap.row.sizes = rep(0,6),
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################


#####################################################################
### Example: irregular detector with gaps (hypothetical test object)
layout <- list(
  name = "Sample Detector (irregular with gaps)", 
  detector.width = 1305,
  detector.height = 1080,
  module.col.n = 10,  # number columns in the array of modules
  module.row.n = 7,   # number rows in the array of modules
  module.col.sizes = c(200, rep(50,2), rep(150,4), rep(50,2), 200), #in this example not all are equally wide
  module.row.sizes = c(250, 150, 75, 50, 75, 150, 250), #in this example not all are equally high
  gap.col.sizes = c(20, rep(5,2), rep(15,3), rep(5,2), 20), 
  gap.row.sizes = c(10,rep(10,4),30),
  module.edges.col = NA, #
  module.edges.row = NA,  #
  detector.inconsistency = NA   #
)
layout.list[[length(layout.list)+1]] <- layout
#####################################################################

#####################################################################
##### NAME ENTRIES IN layout.list
names(layout.list) <- lapply(layout.list, function(layout) layout$name)
#####################################################################
