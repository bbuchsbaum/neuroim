\docType{class}
\name{BrainVector-class}
\alias{BrainVector}
\alias{BrainVector-class}
\title{Four-dimensional brain image}
\usage{
BrainVector(data, space, mask = NULL, source = NULL, label = "")
}
\arguments{
  \item{data}{the image data}

  \item{space}{a \code{\linkS4class{BrainSpace}} object}

  \item{mask}{an optional \code{array} of type
  \code{logical}}

  \item{source}{an optional
  \code{\linkS4class{BrainSource}} object}

  \item{label}{a label of type \code{character}}
}
\value{
a concrete instance of \code{\linkS4class{BrainVector}}
class
}
\description{
Four-dimensional brain image

constructor function for virtual class
\code{\linkS4class{BrainVector}}
}

