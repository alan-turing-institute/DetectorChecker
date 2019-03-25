<!-- badges: start -->
<!-- [![CRAN
status](https://www.r-pkg.org/badges/version/DetectorChecker)](https://cran.r-project.org/package=DetectorChecker) -->
Master: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?branch=master)](https://travis-ci.com/alan-turing-institute/DetectorChecker) Develop: [![Build Status](https://travis-ci.com/alan-turing-institute/DetectorChecker.svg?branch=develop)](https://travis-ci.com/alan-turing-institute/DetectorChecker)
<!-- badges: end -->

## Overview

DetectorChecker is a project aimed at developing software in R to enable assessment of damage to CT scanners arising from exposure to high energy radiation. It allows users to:

- visualise damage to different types of CT scanners,
- perform statistical analysis,
- automatically identify large areas of damage,
- apply multivariate regression models for damage analysis,
- perform individual panel analysis.

The instruction manual of the package can be found here (ADD LINK).

## WebApp

The interactive web app corresponding to the R package DetectorChecker can be accessed with a modern web browser via https://detectorchecker.azurewebsites.net. The web app's source code and instructions how to build it locally is held in a separate repository: https://github.com/alan-turing-institute/DetectorCheckerWebApp.

## Installation

```{r}
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker/")
```

### Development version

```{r}
# install.packages("devtools")
devtools::install_github("alan-turing-institute/DetectorChecker/", ref = "develop")
```

### Getting help

Users are encouraged to report issues on the project's GitHub issue [page](
https://github.com/alan-turing-institute/DetectorChecker/issues).
