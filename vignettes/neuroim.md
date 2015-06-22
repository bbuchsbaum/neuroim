---
title: "The neuroim package"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{The neuroim package}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

```{r, echo = FALSE, message = FALSE}
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(neuroim)
```

The **neuroim** package
========================================================

The **neuroim** package is a library of S4 classes and functions supporting neuroimaigng analysis in R. It contains data structures for 3D and 4D images and rotines for reading and writing NIFTI formatted images. It currently has experimental/limited support for reading AFNI formatted images.

# A brief summary of most important functions is below. More extended documentation can be found in the associated vignettes.

## loading an image volume

`loadVolume`

## writing an image volume to disk

`writeVolume`

## loading a 4-dimensional image (e.g. for fMRI)

`loadVector`

## writing a 4-dimensional image (e.g. for fMRI)

`writeVector`




