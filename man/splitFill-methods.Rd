\docType{methods}
\name{splitFill}
\alias{splitFill}
\alias{splitFill,BrainVolume,factor,function-method}
\title{Generic function to fill disjoint sets of values with the output of a function}
\usage{
splitFill(x, fac, FUN)

\S4method{splitFill}{BrainVolume,factor,function}(x, fac, FUN)
}
\arguments{
  \item{x}{the object to split}

  \item{fac}{the factor to split by}

  \item{FUN}{the function to summarize the the clusters}

  \item{x}{the volume to operate on}

  \item{fac}{the factor used to split the volume}

  \item{FUN}{the function to apply to each split}
}
\value{
a new object where the original values have been replaced
by the function output
}
\description{
Generic function to fill disjoint sets of values with the
output of a function

split values by factor apply function and then fill in new
volume
}
\note{
FUN can return one value per category or one value per
voxel
}

