\name{RegionCube}
\alias{RegionCube}
\title{Create A Cuboid Region of Interest}
\usage{
RegionCube(bvol, centroid, surround, fill = NULL, nonzero = TRUE)
}
\arguments{
  \item{bvol}{an image volume}

  \item{centroid}{the center of the cube in voxel space}

  \item{surround}{the number of voxels on either side of
  the central voxel}

  \item{fill}{optional value(s) to assign to data slot}

  \item{nonzero}{keep only nonzero elements from
  \code{bvol}}
}
\value{
an instance of class \code{ROIVolume}
}
\description{
Create A Cuboid Region of Interest
}

