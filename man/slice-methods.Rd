\docType{methods}
\name{slice}
\alias{slice}
\alias{slice,BrainVolume,numeric,numeric,character-method}
\title{extract a 2D slice from an image volume}
\usage{
slice(x, zlevel, along, orientation, ...)

\S4method{slice}{BrainVolume,numeric,numeric,character}(x, zlevel, along,
  orientation)
}
\arguments{
  \item{x}{the object}

  \item{zlevel}{coordinate (in voxel units) along the
  sliced axis}

  \item{along}{the axis along which to slice}

  \item{orientation}{the target orientation of the 2D
  slice}

  \item{...}{additional arguments}
}
\description{
extract a 2D slice from an image volume
}

