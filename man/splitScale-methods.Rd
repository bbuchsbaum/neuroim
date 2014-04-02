\docType{methods}
\name{splitScale}
\alias{splitScale}
\alias{splitScale,matrix,factor,logical,logical,ANY-method}
\alias{splitScale,matrix,factor,logical,logical-method}
\title{Generic function to center/scale subsets of an object}
\usage{
splitScale(x, f, center, scale)

\S4method{splitScale}{matrix,factor,logical,logical}(x, f, center = TRUE,
  scale = TRUE)
}
\arguments{
  \item{x}{a numeric matrix(like) object}

  \item{f}{the conditioning expression (usually a factor)}

  \item{center}{should values be centered?}

  \item{scale}{should values be scaled?}
}
\value{
a new matrix(like) object where the original values have
been grouped by a factor and then centered and/or scaled
for each grouping
}
\description{
Generic function to center/scale subsets of an object
}

