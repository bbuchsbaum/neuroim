\docType{methods}
\name{indexToCoord}
\alias{indexToCoord}
\alias{indexToCoord,BrainSpace,index-method}
\alias{indexToCoord,BrainVolume,index-method}
\title{Generic function to convert 1D indices to N-dimensional real world coordinates}
\usage{
indexToCoord(x, idx)

\S4method{indexToCoord}{BrainSpace,index}(x, idx)

\S4method{indexToCoord}{BrainVolume,index}(x, idx)
}
\arguments{
  \item{x}{the object}

  \item{idx}{the 1D indices}
}
\value{
a matrix of real coordinates
}
\description{
Generic function to convert 1D indices to N-dimensional
real world coordinates

indexToCoord
}

