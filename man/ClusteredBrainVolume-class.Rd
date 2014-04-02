\docType{class}
\name{ClusteredBrainVolume-class}
\alias{ClusteredBrainVolume}
\alias{ClusteredBrainVolume-class}
\title{ClusteredBrainVolume}
\usage{
ClusteredBrainVolume(mask, clusters, labelMap = NULL, source = NULL,
  label = "")
}
\arguments{
  \item{mask}{an instance of class
  \code{\linkS4class{LogicalBrainVolume}}}

  \item{clusters}{a vector of clusters ids}

  \item{labelMap}{as \code{list} that maps from cluster id
  to a cluster label}

  \item{source}{an instance of class
  \code{\linkS4class{BrainSource}}}

  \item{label}{a \code{character} string}
}
\value{
\code{\linkS4class{ClusteredBrainVolume}} instance
}
\description{
Three-dimensional brain image that is divided into N
disjoint partitions

Construct a \code{\linkS4class{ClusteredBrainVolume}}
instance
}

