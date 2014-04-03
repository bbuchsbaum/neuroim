\docType{methods}
\name{loadData}
\alias{loadData}
\alias{loadData,BrainBucketSource-method}
\alias{loadData,BrainVectorSource-method}
\alias{loadData,BrainVolumeSource-method}
\alias{loadData,SparseBrainVectorSource-method}
\title{Generic function to load data from a data source}
\usage{
loadData(x, ...)

\S4method{loadData}{BrainVectorSource}(x, mmap = FALSE)

\S4method{loadData}{BrainBucketSource}(x, key)

\S4method{loadData}{BrainVolumeSource}(x)

\S4method{loadData}{SparseBrainVectorSource}(x)
}
\arguments{
  \item{x}{a data source}

  \item{...}{additional arguments}

  \item{key}{the name or index of the bucket to load}
}
\value{
an instance of class \code{\linkS4class{BrainVector}}

an instance of class \code{\linkS4class{BrainVolume}}
}
\description{
Generic function to load data from a data source

Load data from a \code{\linkS4class{BrainVectorSource}}

Load data from a \code{\linkS4class{BrainBucketSource}}

load a BrainVolume

loadData
}

