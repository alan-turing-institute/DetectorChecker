# phase0_LayoutExamples_test.R: WSK 2018/07/25
#

source ("paths.R")
source (file.path(location, "Rcode_phase0", "phase0_LayoutExamples_body.R"))

##### Choose Layout by name or index
choice <- "Sample Detector (irregular with gaps)"
# or, for example, choice <- 5

#####################################################################
##### GENERATE CHOSEN Layout 
Layout <- generate.layout(choice, layout.list)

#####################################################################
##### DISPLAY CHOSEN Layout 
Layout
