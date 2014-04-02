\docType{methods}
\name{eachSeries}
\alias{eachSeries}
\alias{eachSeries,BrainVector,function,missing-method}
\alias{eachSeries,SparseBrainVector,function,logical-method}
\title{Generic functions to apply a function to each series of a 4D image
That is, if the 4th dimension is 'time' each series is a 1D time series.}
\usage{
eachSeries(x, FUN, withIndex, ...)

\S4method{eachSeries}{BrainVector,function,missing}(x, FUN, withIndex = FALSE,
  ...)

\S4method{eachSeries}{SparseBrainVector,function,logical}(x, FUN,
  withIndex = FALSE, ...)
}
\arguments{
  \item{x}{a four dimensional image}

  \item{FUN}{a \code{function} taking one or two arguments
  (depending on the value of \code{withIndex}}

  \item{withIndex}{whether the index of the series is
  supplied as the second argument to the function}
}
\description{
Generic functions to apply a function to each series of a
4D image That is, if the 4th dimension is 'time' each
series is a 1D time series.
}

