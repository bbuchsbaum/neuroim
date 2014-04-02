\docType{methods}
\name{writeVector}
\alias{writeVector}
\alias{writeVector,BrainVector,character,character,missing-method}
\alias{writeVector,BrainVector,character,missing,character,ANY-method}
\alias{writeVector,BrainVector,character,missing,character-method}
\alias{writeVector,BrainVector,character,missing,missing-method}
\title{Generic function to write an image vector to disk}
\usage{
writeVector(x, fileName, format, dataType)

\S4method{writeVector}{BrainVector,character,missing,missing}(x, fileName)

\S4method{writeVector}{BrainVector,character,character,missing}(x, fileName,
  format)

\S4method{writeVector}{BrainVector,character,missing,character}(x, fileName,
  dataType)
}
\arguments{
  \item{x}{the image to write}

  \item{fileName}{the bane of the file to write}

  \item{format}{the file format}

  \item{dataType}{the numeric data type}
}
\description{
Generic function to write an image vector to disk

writeVector

writeVector

writeVector
}

