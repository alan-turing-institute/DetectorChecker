# phase0_paths.R: WSK 2018/07/245
#
# PATHS 
###### Current working directory (required to have basename 'DetectorChecker')
location <- getwd()
if (! identical(basename(location), "DetectorChecker")) {
  stop(paste("Basename of ", location, " not equal to 'DetectorChecker'"))
}
