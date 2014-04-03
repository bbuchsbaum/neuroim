\docType{methods}
\name{series}
\alias{series}
\alias{series,BrainVector,matrix-method}
\alias{series,BrainVector,numeric-method}
\alias{series,SparseBrainVector,matrix-method}
\alias{series,SparseBrainVector,numeric-method}
\title{Extract vector series from object}
\usage{
series(x, i, ...)

\S4method{series}{BrainVector,matrix}(x, i)

\S4method{series}{BrainVector,numeric}(x, i, j, k)

\S4method{series}{SparseBrainVector,matrix}(x, i)

\S4method{series}{SparseBrainVector,numeric}(x, i, j, k)
}
\arguments{
  \item{x}{the object}

  \item{i}{the series index}

  \item{...}{additional arguments}

  \item{j}{index of second dimension}

  \item{k}{index of third dimension}
}
\description{
Extract vector series from object

series

series
}

