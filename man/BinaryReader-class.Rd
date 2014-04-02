\docType{class}
\name{BinaryReader-class}
\alias{BinaryReader-class}
\title{BinaryReader}
\description{
This class supports reading of bulk binary data from a
connection
}
\section{Slots}{

\describe{
\item{\code{input}}{the binary input connection}

\item{\code{byteOffset}}{the number of bytes to skip at the start of input}

\item{\code{dataType}}{the dataType of the binary Elements}

\item{\code{bytesPerElement}}{number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}

\item{\code{endian}}{endianness of binary input connection}
}}

