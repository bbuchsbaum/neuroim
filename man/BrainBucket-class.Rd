\docType{class}
\name{BrainBucket-class}
\alias{BrainBucket-class}
\title{BrainBucket}
\description{
a four-dimensional image that conists of a sequence of
labeled image volumes backed by a list
}
\section{Slots}{

\describe{
\item{\code{source}}{the data source for the bucket volumes}

\item{\code{labels}}{the names of the sub-volumes contained in the bucket}

\item{\code{data}}{a list of \code{\linkS4class{BrainVolume}} instances with names corresponding to volume labels}
}}

