---
title: 'DetectorChecker: analyzing patterns of defects in detector screens'
authors:
- affiliation: 1, 2
  name: Julia A. Brettschneider
  orcid: 0000-0003-1763-466X
- affiliation: 2
  name: Oscar Giles
- affiliation: 1, 2
  name: Wilfrid S. Kendall
  orcid: 0000-0001-9799-3480
- affiliation: 2
  name: Tomas Lazcauskas
date: "13 January 2020, revised 16 January 2020"
output: pdf_document
bibliography: JOSSpaper.bib
tags:
- R
- XCT
- bad pixel map
- defective pixels
- spatial statistics
affiliations:
- index: 1
  name: Department of Statistics, University of Warwick, UK
- index: 2
  name: The Alan Turing Institute, UK
---

```
Version: 1.1.4
Cleaned up references, noted some issues for further work.
```

We have made available software and a web application aimed primarily to benefit 
users who need to analyze spatial patterns of defects in panel-structured images
formed out of sub-panels arranged in an architecture which can vary from one site
to another.[^1]
The primary use-case concerns people who are responsible for 
high-value digital detector screens used in X-ray computerised tomography (XCT),
where defects arise due to high radiation flux;
more generally the sotware will be useful for analysis of defects in other
panel-structured arrays, for example solar panels or very large display screens.[^2] 
To maximize accessibility and to avoid any requirement to engage with a specific software environment, 
we have created a web application which provides
the principal features of the software in standalone form.
The web application also affords the possibility of engaging with our team in extended analysis 
of defect patterns as they evolve over time.

[^1]:
  USE A MORE ACTIVE VOICE IN STARTING PARAGRAPH!

[^2]:
  *DESCRIBE PANEL-STRUCTURED ARRAYS BETTER, AND BE MORE SYSTEMATIC IN HOW TO REFER TO THEM LATER.
  
Digital detector screens are crucial high-value components of imaging systems used throughout 
modern science  and engineering systems, particularly in X-ray computerised tomography (XCT).
Screen quality is typically strongly linked to system performance:
in the case of XCT this is noted by @YaffeRowlands-1997,
while the @FDA-2018[^3] advises to replace XCT detector screens after order of 10 years use. 
Screen replacement or refurbishment is expensive;
regular checks of screen pixels are needed (a) to quantify screen quality
and (b) to assess possible _special causes_ of defective pixels,
using the terminology of classic quality control established by @Shewhart-1939.
This is best done by means of considerations of spatial statistics, 
both to determine the extent to which spatial patterns of defective pixels
can be accounted for by quantifiable independent random variation
and also in describing departures from spatial randomness in ways
which are suggestive of possible explanations (for example, stress due 
to screen attachment, or failure at pixel level of data readout).
Methods of spatial statistics are crucial for this task: @ChiuStoyanKendallMecke-2013
discuss some theoretical foundations
while
@BaddeleyRubakTurner-2015 describe an implementation of spatial statistics methods as the _spatstat_ package in the
R statistical computing environment [@RFoundation-2019].
_DetectorChecker_ makes available methods from _spatstat_ adapted for images based
on sub-panels of arbitrary architecture, and point patterns arising either from 
individual defects or from "clumps" of defects (determined in a manner specified by the user).[^4]

[^3]:
  (VERIFY @FDA-2018 REFERENCE!)
  
[^4]:
  CHECK DESCRIPTION OF DETECTORCHECKER AGAINST WHAT IS ACTUALLY ON OFFER.

Defects are modelled as points in an image rectangle based on screen dimensions.

1. The software enables the user to specify the exact sub-panel architecture, using a drop-down menu to specify an option
or alternatively uploading their own specification. 

2. Intensity maps can be produced _via_ kernel smoothing applied to the point pattern
(replacing each defect point by the corresponjding translate of a fixed kernel function).[^5]

3. Departure from completed randomness can be assessed using visual inspection of graphs
of $F$, $G$ and $K$ functions;
the $F$ function or "empty space function"
computes the empirical distribution of the nearest distance to a defect point from a typical location
chosen uniformly from the image rectangle;
the $G$ function
computes the empirical distribution of nearest-neighbour distances between defect points;
the $K$ function (Ripley's $K$ function) 
computes the empirical mean number of defect points within a distance $r$ of a typical defect point,
viewed as a function of $R$.[^6]

4. Finally the relationship of the defect points to sub-panel boundaries can be studied by means of various logistic regression options.[^7][^8]

[^5]:
  ADD EQUATION AND SOME COMMENTARY FOR SMOOTHING.

[^6]:
  ADD THREE EQUATIONS FOR $F$, $G$, $K$ AND SUPPLY SOME COMMENTARY.

[^7]:
  ADD EQUATION AND SOME COMMENTARY FOR LOGISTIC REGRESSION.

[^8]: Figures?
  (To be discussed and supplied: envisage order of three figures demonstrating use of web application. restrict to vignette?)


The software also provides for further graphical options, 
such as the study of direction from a typical defect point to
its nearest neighbour within the panel, 
analysing at the level of "events" (appropriately defined grouping of clumps of defect pixels) rather than individual defect points,
and exclusion of regions of the image rectangle for which the defect intensity is clearly different
(this often arises in XCT, where corners of the image exhibit high defect intensity deriving presumably from mechanical
stress due to supports of the screen).

The R package _DetectorChecker_ is made available under MIT licence to facilitate investigations using these methodas is
an associated but self-contained web application providing a graphical interface to the major aspects of the package (for the benefit particularly of XCT users who otherwise have no need to get involved in R).
_DetectorChecker_  
[@BrettschneiderGilesKendallLazcauskas-2019a]
is an R package based on _spatstat_; 
the web application
[DetectorCheckerWebApp](https://detectorchecker.azurewebsites.net/)
[@BrettschneiderGilesKendallLazcauskas-2019b]
is based on a self-contained R environment together 
with a _Shiny_ gui, implemented and made available _via_ _Azure_, so as to expose the
basic functionality of the R package without the need for users to install R.
In particular the web application  can be used
to define the geometry of the panels of the detector screen
(which is to say, the arrangement and size of the component sub-panels),
to upload the spatial arrangement of the defective pixels either
directly by means of "bad pixel maps" (XML format) or inferred from test images (formats including TIFF),
and then to inspect the results using the facilities offered 
by the package.
To the best of our knowledge, there is no other comparable package or web application
making methods of spatial statistics available for panel-based image data of arbitrary architecture.

An extended example of use of the R package, paralleled by corresponding use of the web application,
is available as a vignette in both github repositories. 

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

# Acknowledgements

We gratefully acknowledge support from the UK EPSRC (grant EP/K031066/1)
and the Alan Turing Institute (under the EPSRC
grant EP/N510129/1) during this project.



# References 
