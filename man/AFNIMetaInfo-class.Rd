\docType{class}
\name{AFNIMetaInfo-class}
\alias{AFNIMetaInfo}
\alias{AFNIMetaInfo-class}
\title{This class contains meta information for a AFNI image file}
\usage{
AFNIMetaInfo(descriptor, afni_header)
}
\arguments{
  \item{descriptor}{an instance of class
  \code{\linkS4class{AFNIFileDescriptor}}}

  \item{afni_header}{a \code{list} returned by
  \code{readAFNIHeader}}
}
\value{
an instance of class \code{\linkS4class{AFNIMetaInfo}}
}
\description{
This class contains meta information for a AFNI image file

Constructor for \code{\linkS4class{AFNIMetaInfo}} class
}
\section{Slots}{

\describe{
\item{\code{afni_header}}{a list of attributes specific to the AFNI file format}
}}

