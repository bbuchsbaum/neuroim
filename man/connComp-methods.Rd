\docType{methods}
\name{connComp}
\alias{connComp}
\alias{connComp,BrainVolume-method}
\title{Find connected components}
\usage{
connComp(x, ...)

\S4method{connComp}{BrainVolume}(x, threshold = 0, coords = TRUE,
  clusterTable = TRUE, localMaxima = TRUE, localMaximaDistance = 15)
}
\arguments{
  \item{x}{the image object}

  \item{...}{additonal arguments}
}
\description{
Find connected components

find connected components in BrainVolume
}

