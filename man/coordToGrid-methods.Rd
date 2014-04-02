\docType{methods}
\name{coordToGrid}
\alias{coordToGrid}
\alias{coordToGrid,BrainSpace,matrix-method}
\alias{coordToGrid,BrainVolume,matrix-method}
\title{Generic function to convert N-dimensional real world coordinates to grid coordinates}
\usage{
coordToGrid(x, coords)

\S4method{coordToGrid}{BrainSpace,matrix}(x, coords)

\S4method{coordToGrid}{BrainVolume,matrix}(x, coords)
}
\arguments{
  \item{x}{the object}

  \item{coords}{a matrix of real world coordinates}
}
\value{
a matrix of grid coordinates
}
\description{
Generic function to convert N-dimensional real world
coordinates to grid coordinates

coordToGrid
}

