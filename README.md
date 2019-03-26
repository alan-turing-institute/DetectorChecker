# DetectorChecker 

Master: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=master)](https://travis-ci.com/alan-turing-institute/DetectorChecker) Develop: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=develop)](https://travis-ci.com/alan-turing-institute/DetectorChecker)


## Overview

DetectorChecker is an R package to aid in the assessment of damage to CT scanners arising from exposure to high energy radiation.


## Installation 

```
install_github("alan-turing-institute/DetectorChecker/")
```

### Development version

```
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker/", ref = "develop")
```



## Examples
DetectorChecker includes a number of example datasets for five detector types:

1. PerkinElmer $\times$ 2
 
2. PerkinElmer Refurbished $\times$ 2

3. PerkinElmer Cropped $\times$ 2

4. Pilatus $\times$ 1

5. Excalibur $\times$ 1


The user guide vignette provides detailed instructions for using the package and loading specific examples:

```
library(detectorchecker)
vignette("user_guide", package = "detectorchecker")
```

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
