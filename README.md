# DetectorChecker <a><img src='logo_hex.png' align="right" height="139" /></a>

Master: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=master)](https://travis-ci.com/alan-turing-institute/DetectorChecker) Develop: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?token=zxQwzfsqCyEouTqXAVUn&branch=develop)](https://travis-ci.com/alan-turing-institute/DetectorChecker)

[![DOI](https://zenodo.org/badge/144782935.svg)](https://zenodo.org/badge/latestdoi/144782935)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Created by: [Julia Brettschneider](https://github.com/ejulia17) (original R code), [Tomas Lazauskas](https://github.com/tomaslaz) (R package engineering), [Oscar Giles](https://github.com/OscartGiles) (package development) and [Wilfrid Kendall](https://github.com/WilfridSKendall) (testing and editing).


## Overview

DetectorChecker is an R package to aid in the assessment of damage to CT scanners arising from exposure to high energy radiation.
While the target application concerns CT scanners, this package can also be used to analyze screen damage arising from other sources.


## Installation

To install from github you will need to have the [devtools](https://github.com/r-lib/devtools) package installed.

In R run one of the following, depending on whether you want to build the package Vignettes, removing the # if you do not have devtools installed:

```
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker")
```


If you want to be able to view the `DetectorChecker_user-guide` Vignette you need to install with:

```
devtools::install_github("alan-turing-institute/DetectorChecker", 
     build_vignettes = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

Installing with the vignettes may be slow (~10 min)


### Development version

```
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker", ref = "develop")
```


## WebApp

The official release of the DetectorChecker WebApp is hosted at 
<https://detectorchecker.azurewebsites.net>.

<img src="https://raw.githubusercontent.com/alan-turing-institute/DetectorChecker/master/inst/img/DetectorChecker.png" width="500" align="center">

The source code for the WebApp implementation can be found on GitHub: 
<https://github.com/alan-turing-institute/DetectorCheckerWebApp>.


## Vignette

The user guide vignette provides detailed instructions for using the package and loading specific examples. Make sure you installed the package including vignette following the instructions above (see use of `build_vignettes = TRUE` in Section Installation) and then load the package followed by the vignette command:

```
library(detectorchecker)
vignette("DetectorChecker_user-guide", package = "detectorchecker")
```

## Manual
Detailed documentation is provided as a [pdf](docs/detectorchecker_1.0.8.pdf).
See also the [DetectorChecker_user-guide](vignettes/DetectorChecker_user-guide.html) (large html file, needs download).

## Examples
DetectorChecker includes a number of example datasets for five detector types:

1. Pilatus

2. PerkinElmer

3. PerkinElmer Refurbished

4. PerkinElmer Cropped

5. Excalibur

To load an example dataset, either call:

```
library(detectorchecker)

# Initiate a PerkinElmerFull detector object
detector <-  create_detector("PerkinElmerFull")

# Path of dataset
file_path <- system.file("extdata", "PerkinElmerFull",
                        "BadPixelMap_t1.bpm.xml", 
                        package = "detectorchecker")

# Load a pixel matrix into the detector object
detector <- load_pix_matrix(detector = detector, file_path = file_path)
```

or load one of the examples by calling:

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
If you use DetectorChecker in your work please cite our package.

BibTeX:

```
  @Misc{,
    title = {{DetectorChecker}: Assessment of damage to CT scanners},
    author = {Tomas Lazauskas and Julia Brettschneider and Oscar Giles and Wilfrid Kendall},
    url = {https://github.com/alan-turing-institute/DetectorChecker},
  }
```

## Getting help
If you found a bug or need support, please submit an issue [here](https://github.com/alan-turing-institute/DetectorChecker/issues/new).

## How to contribute
We welcome contributions! If you are willing to propose new features or have bug fixes to contribute, please submit a pull request [here](https://github.com/alan-turing-institute/DetectorChecker/pulls).
