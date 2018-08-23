# phase1_LogRegression_test.R: JAB 2018/07/12, edited WSK 2018/07/25
#
# CONTENT:
# LIBRARIES, PATHS 
# LOGSTIC REGRESSION WITH LAYOUT BASED PREDICTORS

# WORKFLOW: Assumes already run
#    - phase0_LayoutPixel.R
#    - phase1_ReadingPixels.R, including consistency checks there
#
# INPUT: - Tbin (dead pixel mask built in phase1_PixelReading), 
#        - LayoutPixel (object built in Phase0 for suitable detector type)
#
# 0UTPUT: Summaries of results for a variety of models
#
# RUN: 
#   - LIBRARIES, PATHS (user has to adjust some paths names referring to directories in his own computer!)
#   - LOGISTIC REGRESSION WITH LAYOUT BASED PREDICTORS (user can try various models)
# 
# To Do: Find good format for output of model summary and comparison of different models.
#        Look at model fit and interpretation (becomes more interesting with other data than Pilatus - too predictable...)


##### LIBRARIES, PATHS

###### Paths
# Assumes we start in DetectorChecker directory!
source ("paths.R")

###### Appropriate data directory
dirData <- file.path(location, "data")

###### Source the definition of Layout
source (file.path(location, "Rcode_phase0", "phase0_LayoutExamples_test.R"))
###### Source the work of LayoutPixel
source (file.path(location, "Rcode_phase0", "phase0_LayoutPixel_test.R"))
####### Note: in the above, need to stop having to hard-code "Pilatus"

###### Source the work of PixelReadingPilatus
source (file.path(location, "Rcode_phase1", "phase1_PixelReadingPilatus_test.R"))

###### Appropriate output directory
if (Layout$name == "Pilatus"){ 
  dirOut <- file.path(location, "Output_phase1", "Phase1_LogRegression", "Pilatus")
} else if (substring(Layout$name,1,11) == "PerkinElmer"){
  dirOut <- paste(location, "Output_phase1", "Phase1_LogRegression", paste(Layout$name,Layout$date, sep=""))
} else {
  dirOut <- file.path(location, "Output_phase1", "Phase1_LogRegression", "Misc")
}


##### LOGISTIC REGRESSION WITH LAYOUT BASED PREDICTORS

# Note: Matrix orientations are not the same in Tbin and LayoutPixel matrices:
#> dim(LayoutPixel$dist.matrix.centre.linf)
#[1] 2463 2527
#[1] 2527 2463
#> dim(Tbin)
# Therefore, transpose Tbin in the below.

# Model fitting takes its time!!! (order of minutes per model, with interaction effects longer)
# Comment Pilatus: Seems to fit okay (with very clear results) 
# Comment PerkinElmerCropped: Warnings... (not converge, proba 0/1)

M0.eucl <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.centre.eucl), 
          family=binomial(link=logit))

M0.linf <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.centre.linf), 
               family=binomial(link=logit))

M1c <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.col), 
           family=binomial(link=logit))

M1r <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.row), 
           family=binomial(link=logit))

M2 <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.col) + as.vector(LayoutPixel$dist.matrix.row), 
          family=binomial(link=logit))

M2.irc <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.col) + as.vector(LayoutPixel$dist.matrix.row)
              + as.vector(LayoutPixel$dist.matrix.col) : as.vector(LayoutPixel$dist.matrix.row), 
              family=binomial(link=logit))

M3 <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.min), 
          family=binomial(link=logit))

M4 <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.col) + as.vector(LayoutPixel$dist.matrix.row)
          + as.vector(LayoutPixel$dist.matrix.col) : as.vector(LayoutPixel$dist.matrix.row), 
          family=binomial(link=logit))

M5 <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.col) + as.vector(LayoutPixel$dist.matrix.row)
          + as.vector(LayoutPixel$dist.matrix.col) : as.vector(LayoutPixel$dist.matrix.row)
          + as.vector(LayoutPixel$dist.matrix.centre.eucl), 
          family=binomial(link=logit))

M6 <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.col) + as.vector(LayoutPixel$dist.matrix.row)
          + as.vector(LayoutPixel$dummy.matrix.col) + as.vector(LayoutPixel$dummy.matrix.row)
          + as.vector(LayoutPixel$dist.matrix.centre.eucl), 
          family=binomial(link=logit))

# include interaction between dummy for col and row edges to catch higher dead pixel rate in corners
M6idrdc <- glm(as.vector(Tbin) ~ as.vector(LayoutPixel$dist.matrix.col) + as.vector(LayoutPixel$dist.matrix.row)
               + as.vector(LayoutPixel$dummy.matrix.col) + as.vector(LayoutPixel$dummy.matrix.row)
               + as.vector(LayoutPixel$dummy.matrix.col) : as.vector(LayoutPixel$dummy.matrix.row)
               + as.vector(LayoutPixel$dist.matrix.centre.eucl), 
               family=binomial(link=logit))


sink(paste(dirOut,  "/", "modelResults_", Layout$name, ".txt", sep=""))
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
cat("\n")
print(summary(M0.eucl))
cat("\n\n")
print(summary(M0.linf))
cat("\n\n")
print(summary(M1c))
cat("\n\n")
print(summary(M1r))
cat("\n\n")
print(summary(M2))
cat("\n\n")
print(summary(M2.irc))
cat("\n\n")
print(summary(M3))
cat("\n\n")
print(summary(M4))
cat("\n\n")
print(summary(M5))
cat("\n\n")
print(summary(M6))
cat("\n\n")
print(summary(M6idrdc))
cat("\n\n")
sink()
