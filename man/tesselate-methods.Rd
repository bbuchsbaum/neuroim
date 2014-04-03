\docType{methods}
\name{tesselate}
\alias{tesselate}
\alias{tesselate,LogicalBrainVolume,numeric-method}
\title{tesselate}
\usage{
tesselate(x, K, ...)

\S4method{tesselate}{LogicalBrainVolume,numeric}(x, K, features = NULL,
  spatialWeight = 4)
}
\arguments{
  \item{x}{the object to tesselate}

  \item{K}{the number of partitions}

  \item{...}{extra arguments}

  \item{features}{use addiitonal feature set to tesselate
  volume}

  \item{spatialWeight}{weight voxels according to distance}
}
\description{
tesselate

tesselate a LogicalBrainVolume into K spatial disjoint
components
}

