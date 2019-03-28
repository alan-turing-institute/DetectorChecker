# DetectorChecker 

Master: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=master)](https://travis-ci.com/alan-turing-institute/DetectorChecker) Develop: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=develop)](https://travis-ci.com/alan-turing-institute/DetectorChecker)


## Overview

DetectorChecker is an R package to aid in the assessment of damage to CT scanners arising from exposure to high energy radiation.


## Installation 

To install from github you will need to have the [devtools](https://github.com/r-lib/devtools) package installed.

In R run the following, removing the # if you do not have devtools installed:

```
# install.packages("devtools")
install_github("alan-turing-institute/DetectorChecker/")
```

### Development version

```
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker/", ref = "develop")
```

## Vignettes 

The user guide vignette provides detailed instructions for using the package and loading specific examples:
```
library(detectorchecker)
vignette("user_guide", package = "detectorchecker")
```


## Examples
DetectorChecker includes a number of example datasets for five detector types:

1. PerkinElmer
 
2. PerkinElmer Refurbished 

3. PerkinElmer Cropped 

4. Pilatus 

5. Excalibur 

To load an example dataset you can either call:
```
library(detectorchecker)

#Inititate a PerkinElmerFull detector object
detector <-  create_module("PerkinElmerFull") 

# Path of dataset
path <- system.file("extdata", "PerkinElmer_Full", "BadPixelMap.bpm", "BadPixelMap_t1.bpm.xml", package = "detectorchecker")

# Load a pixel matrix into the detector object
detector <- load_pix_matrix(detector = detector, file_path = file_path) 
```

or you can load one of the examples by calling:

```
library(detectorchecker)
data(PerkinElmerFull_exp_1)
```

which creates an appropriate detector module and loads an example pixel dataset.

For see the full list of example datasets call

```
data(package = "detectorchecker")
```
