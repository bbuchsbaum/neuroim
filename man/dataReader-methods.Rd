\docType{methods}
\name{dataReader}
\alias{dataReader}
\alias{dataReader,AFNIMetaInfo-method}
\alias{dataReader,NIfTIMetaInfo-method}
\title{Generic function to create data reader}
\usage{
dataReader(x, offset)

\S4method{dataReader}{NIfTIMetaInfo}(x, offset = 0)

\S4method{dataReader}{AFNIMetaInfo}(x, offset = 0)
}
\arguments{
  \item{x}{an object specifying the infromation required to
  produce the reader}

  \item{offset}{the byte offset (number of bytes to skip
  before reading)}
}
\description{
Generic function to create data reader
}

