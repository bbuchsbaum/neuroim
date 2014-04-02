\docType{class}
\name{FileMetaInfo-class}
\alias{FileMetaInfo-class}
\title{This class contains meta information from an image data file}
\description{
This class contains meta information from an image data
file
}
\section{Slots}{

\describe{
\item{\code{headerFile}}{name of the file containing meta information}

\item{\code{dataFile}}{name of the file containing data}

\item{\code{fileDescriptor}}{descriptor of image file format}

\item{\code{endian}}{byte order of data ('little' or 'big')}

\item{\code{dataOffset}}{the number of bytes preceding the start of image data in data file}

\item{\code{bytesPerElement}}{number of bytes per element}

\item{\code{intercept}}{constant value added to image -- multiple values allowed (must equal numer of sub-images)}

\item{\code{slope}}{image multiplier -- multiple values allowed (must equal numer of sub-images)}

\item{\code{header}}{a list of format specific attributes}
}}

