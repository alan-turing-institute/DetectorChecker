# DetectorChecker <a><img src='logo_hex.png' align="right" height="139" /></a>

Master: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=master)](https://travis-ci.com/alan-turing-institute/DetectorChecker) Develop: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=develop)](https://travis-ci.com/alan-turing-institute/DetectorChecker)



Created by: [Julia Brettschneider](https://github.com/ejulia17) (original R code), [Wilfrid Kendall](https://github.com/WilfridSKendall) (testing and editing),
[Tomas Lazauskas](https://github.com/tomaslaz) (R package engineering) and [Oscar Giles](https://github.com/OscartGiles) (package development)

## Overview

DetectorChecker is an R package to aid in the assessment of damage to CT scanners arising from exposure to high energy radiation.


## Installation 

To install from github you will need to have the [devtools](https://github.com/r-lib/devtools) package installed.

In R run one of the following, depending on whether you want to build the package Vignettes, removing the # if you do not have devtools installed:

```
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker")
```


If you want to be able to view the user_guide Vignette you need to install with:
```
devtools::install_github("alan-turing-institute/DetectorChecker", build_vignettes = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```
Installing with the vignettes may be slow (~10 min)


### Development version

```
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker", ref = "develop")
```


## Vignette

The user guide vignette provides detailed instructions for using the package and loading specific examples. Make sure you installed following the `with vignette` instructions:

```
library(detectorchecker)
vignette("user_guide", package = "detectorchecker")
```

## Manual 
Documentation is provided as a [pdf](docs/detectorchecker_0.1.10.pdf)

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
detector <-  create_detector("PerkinElmerFull") 

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

## Citation
If you use DetectorChecker in your work please cite our package:

`Julia Brettschneider, Oscar Giles, Wilfrid Kendall and Tomas Lazauskas (2019). DetectorChecker: Assessment of damage to CT scanners. URL https://github.com/alan-turing-institute/DetectorChecker`

For full citation and bibtex details you can call this in R:

```
citation("detectorchecker")
```

## Getting help

Users are encouraged to report issues on the project's GitHub issue [page](https://github.com/alan-turing-institute/DetectorChecker/issues).