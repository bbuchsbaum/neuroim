% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R, R/BrainSpace.R
\docType{methods}
\name{axisToIndex}
\alias{axisToIndex}
\alias{indexToAxis}
\alias{axisToIndex,BrainSpace,numeric,numeric-method}
\alias{indexToAxis,BrainSpace,numeric,numeric-method}
\title{Convert 1-dimensional real axis coordinates along a single axis dimension 
to a 1D index along the same axis.}
\usage{
axisToIndex(x, real, dimNum)

indexToAxis(x, index, dimNum)

\S4method{axisToIndex}{BrainSpace,numeric,numeric}(x, real, dimNum)

\S4method{indexToAxis}{BrainSpace,numeric,numeric}(x, index, dimNum)
}
\arguments{
\item{x}{the object}

\item{real}{the axis coordinates}

\item{dimNum}{the dimension number of the axis (e.g.  1, 2, 3)}

\item{index}{the axis index}

\item{x}{the object}

\item{dimNum}{the dimension number of the axis (e.g.  1, 2, 3)}
}
\value{
a vector of axis indices

a vector of axis indices
}
\description{
Convert 1-dimensional real axis coordinates along a single axis dimension 
to a 1D index along the same axis.

Convert 1-dimensional index to a coordinate value along the same axis.
}
