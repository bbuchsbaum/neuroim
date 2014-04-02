\docType{class}
\name{BrainSpace-class}
\alias{BrainSpace-class}
\title{BrainSpace}
\description{
This class represents the geometry of a brain image
}
\section{Slots}{

\describe{
\item{\code{Dim}}{the grid dimensions of the image}

\item{\code{origin}}{the coordinates of the spatial origin}

\item{\code{spacing}}{the dimensions (in mm) of the grid units (voxels)}

\item{\code{axes}}{the set of named spatial axes}

\item{\code{trans}}{an affine transformation matrix that moves from grid -> real world coordinates}

\item{\code{inverseTrans}}{an inverse matrix that moves from real world -> grid coordinates}
}}

