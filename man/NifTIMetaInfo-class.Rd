\docType{class}
\name{NIfTIMetaInfo-class}
\alias{NIfTIMetaInfo}
\alias{NIfTIMetaInfo-class}
\title{This class contains meta information for a NIfTI image file}
\usage{
NIfTIMetaInfo(descriptor, nifti_header)
}
\arguments{
  \item{descriptor}{an instance of class
  \code{\linkS4class{NIfTIFileDescriptor}}}

  \item{nifti_header}{a \code{list} returned by
  \code{readNIftiHeader}}
}
\value{
an instance of class \code{\linkS4class{NIfTIMetaInfo}}
}
\description{
This class contains meta information for a NIfTI image file

Constructor for \code{\linkS4class{NIfTIMetaInfo}} class
}
\section{Slots}{

\describe{
\item{\code{nifti_header}}{a list of attributes specific to the NIfTI file format}
}}

