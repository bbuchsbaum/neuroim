\name{Kernel}
\alias{Kernel}
\title{Create a Kernel object}
\usage{
Kernel(kerndim, vdim, FUN = dnorm, ...)
}
\arguments{
  \item{kerndim}{the dimensions in voxels of the kernel}

  \item{vdim}{the dimensions of the voxels in real units}

  \item{FUN}{the kernel function taking as its first
  argument representing the distance from the center of the
  kernel}

  \item{...}{additional parameters to the kernel FUN}
}
\description{
Create a Kernel object
}

