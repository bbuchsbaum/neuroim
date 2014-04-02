\name{loadVolume}
\alias{loadVolume}
\title{load an image volume from a file}
\usage{
loadVolume(fileName, index = 1)
}
\arguments{
  \item{fileName}{the name of the file to load}

  \item{index}{the index of the volume (e.g. if the file is
  4-dimensional)}
}
\value{
an instance of the class \code{\linkS4class{BrainVolume}}
}
\description{
load an image volume from a file
}
\examples{
fname <- paste0(system.file(package="neuroim"), "/data/clusvol.nii")
x <- loadVolume(fname)
print(dim(x))
space(x)
}

