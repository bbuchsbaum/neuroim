\docType{class}
\name{LogicalBrainVolume-class}
\alias{LogicalBrainVolume}
\alias{LogicalBrainVolume-class}
\title{LogicalBrainVolume}
\usage{
LogicalBrainVolume(data, space, source = NULL, label = "", indices = NULL)
}
\arguments{
  \item{data}{a three-dimensional \code{array}}

  \item{space}{an instance of class
  \code{\linkS4class{BrainSpace}}}

  \item{source}{an instance of class
  \code{\linkS4class{BrainSource}}}

  \item{label}{a \code{character} string}

  \item{indices}{an optional 1-d index vector}
}
\value{
\code{\linkS4class{LogicalBrainVolume}} instance
}
\description{
Three-dimensional brain image where all values are either
TRUE or FALSE

Construct a \code{\linkS4class{LogicalBrainVolume}}
instance
}

