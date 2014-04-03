\docType{methods}
\name{partition}
\alias{partition}
\alias{partition,ClusteredBrainVolume,numeric,matrix-method}
\title{partition}
\usage{
partition(x, K, features, ...)

\S4method{partition}{ClusteredBrainVolume,numeric,matrix}(x, K, features,
  method = "kmeans")
}
\arguments{
  \item{x}{the object to partition}

  \item{K}{the number of partitions}

  \item{features}{the features used to define the
  partition}

  \item{...}{additional arguments}
}
\description{
partition

partition a \code{ClusteredBrainVolume} into K spatial
disjoint components for every existing partition in the
volume
}

