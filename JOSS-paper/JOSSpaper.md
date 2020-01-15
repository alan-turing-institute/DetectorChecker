---
title: 'DetectorChecker: analyzing patterns of defects in detector screens'
tags:
  - R
  - XCT
  - bad pixel map
  - defective pixels
  - spatial statistics
authors:
  - name: Julia A. Brettschneider
    affiliation: "1, 2" 
  - name: Oscar Giles
    affiliation: 2
  - name: Wilfrid S. Kendall
    orcid: 0000-0001-9799-3480
    affiliation: "1, 2"
  - name: Tomas Lazcauskas
    affiliation: 2
affiliations:
 - name: Department of Statistics, University of Warwick, UK
   index: 1
 - name: The Alan Turing Institute, UK
   index: 2
date: 13 January 2020, revised 14  January 2020
bibliography: paper.bib
---

```
Version: 1.1.1
Stylistic revision,
added references to github repositories.
```

# Summary

Digital detector screens are crucial components of imaging systems used throughout 
modern science  and engineering systems, for example in X-ray computerised tomography (XCT).
Screen quality is typically strongly linked to system performance,
in the case of XCT this is noted by @YaffeRowlands-1997,
while the @FDA-2018 advises to replace detector screens after use for order of 10 years. 
Screen replacement or refurbishment is expensive;
regular checks of screen pixels are needed (a) to quantify screen quality
and (b) to assess possible _special causes_ of defective pixels,
in the classic quality control terminology of @ShewhartDeming-1939.
This is best done by means of considerations of spatial statistics, 
both to determine the extent to which spatial patterns of defective pixels
can be accounted for by quantifiable independent random variation
and also in describing departures from spatial randomness in ways
which are suggestive of possible explanations (for example, stress due 
to screen attachment, or failure at pixel level of data readout).
Methods of spatial statistics are crucial for this task: for some theoretical foundations
see @ChiuStoyanKendallMecke-2013.
@BaddeleyRubakTurner-2015 describe an implementation as the _spatstat_ package in the
R statistical computing environment [@RFoundation-2019].

We have made available an R package to facilitate investigations using these methods,
and also (for the benefit particularly of XCT users who otherwise have no need to get involved in R)
a corresponding but self-contained web application.
Specifically,
_DetectorChecker_ (source available at public github repository <https://github.com/alan-turing-institute/DetectorChecker>) is an R package based on _spatstat_, 
which provides functions to enable users to apply 
methods from spatial statistics (intensity mapping of point patterns;
graphical methods of point pattern analysis such as plots of $F$, $K$, and $G$ functions; logistic regression)
to provide statistical analysis of the patterns of defective
pixels on detector screens, viewed as two-dimensional point patterns.
The web application
[DetectorCheckerWebApp](https://detectorchecker.azurewebsites.net/)
(source available at public github repository <https://github.com/alan-turing-institute/DetectorCheckerWebApp>)
uses a self-contained R environment together 
with a _Shiny_ gui, implemented and made available _via_ _Azure_, to expose the
basic functionality of the R package without the need for users to install R.
The web application  can be used
to define the geometry of the panels of the detector screen
(which is to say, the arrangement and size of the component sub-panels),
to upload the spatial arrangement of the defective pixels either
directly by means of "bad pixel maps" or inferred from TIFF test images,
and then to inspect the results using the facilities offered 
by the package.
To the best of our knowledge, there is no other comparable package or web application.

The R package and web application together offer significant
opportunities to address interesting and important challenges for the data analysis of defective pixel patterns.
The web application offers the possibility of uploading users' data to 
a data repository, thus permitting the possibility of organizing cooperative
statistical investigations comparing patterns across different machines and
different modes of usage. In particular we envisage its use to collect
time sequences of images, to permit statistical investigation at Warwick
of deterioration over time, using latent Markov models
of the life and death of defective pixels which we are currently developing.
Such analysis requires sustained and regular monitoring of a diversity
of screens from various devices.
Users are encouraged to get in touch with us to discuss these possibilities,
which promise to deliver evidence-based analysis 
to support decisions on refurbishment and / or replacement 
strategies.





# Figures

**(To be discussed and supplied: envisage order of three figures demonstrating use of web application)**

# Acknowledgements

We gratefully acknowledge support from the UK EPSRC (grant EP/K031066/1)
and the Alan Turing Institute (under the EPSRC
grant EP/N510129/1) during this project.

# Software sources
**(Unsure of how to reference these in bibliography)**

* _DetectorCheckerWebApp_ (https://detectorchecker.azurewebsites.net/):
<https://github.com/alan-turing-institute/DetectorCheckerWebApp>  
* _DetectorChecker_: 
<https://github.com/alan-turing-institute/DetectorChecker>

# References 
