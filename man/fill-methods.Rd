\docType{methods}
\name{fill}
\alias{fill}
\alias{fill,BrainVolume,matrix-method}
\title{Generic function to map values from one set to another using a user-supplied lookup table}
\usage{
fill(x, lookup)

\S4method{fill}{BrainVolume,matrix}(x, lookup)
}
\arguments{
  \item{x}{the object to map values from}

  \item{lookup}{the lookup table}
}
\value{
a new object where the original values have been filled in
with the values in the lookup table
}
\description{
Generic function to map values from one set to another
using a user-supplied lookup table
}

