% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R, R/BrainSpace.R, R/BrainVolume.R
\docType{methods}
\name{coordToIndex}
\alias{coordToIndex}
\alias{coordToIndex,BrainSpace,matrix-method}
\alias{coordToIndex,BrainSpace,numeric-method}
\alias{coordToIndex,BrainVolume,matrix-method}
\title{Generic function to convert N-dimensional real world coordinates to 1D indices}
\usage{
coordToIndex(x, coords)

\S4method{coordToIndex}{BrainSpace,matrix}(x, coords)

\S4method{coordToIndex}{BrainSpace,numeric}(x, coords)

\S4method{coordToIndex}{BrainVolume,matrix}(x, coords)
}
\arguments{
\item{x}{the object}

\item{coords}{a matrix of real world coordinates}
}
\value{
a vector of indices
}
\description{
Generic function to convert N-dimensional real world coordinates to 1D indices
}
