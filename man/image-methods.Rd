% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Display.R
\docType{methods}
\name{image,BrainVolume-method}
\alias{image,BrainVolume-method}
\title{image}
\usage{
\S4method{image}{BrainVolume}(x, slice = dim(vol)[3]/2,
  col = gray((0:255)/255, alpha = 1), zero_col = "#000000", axis = 3, ...)
}
\arguments{
\item{slice}{the voxel index of the slice to display}

\item{col}{a color map}

\item{zero_col}{the color to use when the value is 0 (e.g background color)}

\item{...}{extra arguments to passed to \code{grid.raster}}
}
\description{
image
}
