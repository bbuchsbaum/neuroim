#' @import iterators

#' @include AllClass.R
{}
#' @include AllGeneric.R
{}


#' Create an instance of class \code{\linkS4class{ROIVolume}}
#' 
#' @param space an instance of class \code{BrainSpace}
#' @param coords matrix of voxel coordinates
#' @param data the data values, numeric vector
#' @return an instance of class \code{ROIVolume}
#' @rdname ROIVolume
#' @export
ROIVolume <- function(vspace, coords, data=rep(nrow(coords),1)) {
  new("ROIVolume", space=vspace, coords=coords, data=as.vector(data))
}

#' Create an instance of class \code{\linkS4class{ROIVector}}
#' 
#' @param vspace an instance of class \code{BrainSpace}
#' @param coords matrix of voxel coordinates
#' @param data the \code{matrix} of data values
#' @return an instance of class \code{ROIVector}
#' @rdname ROIVector
#' @export
ROIVector <- function(vspace, coords, data=rep(nrow(coords),1)) {
  new("ROIVector", space=vspace, coords=coords, data=data)
}

#' convert a \code{ROIVector} to a matrix
#' 
#' @rdname as.matrix-methods
#' @param x the object
#' @export 
setMethod(f="as.matrix", signature=signature(x = "ROIVector"), def=function(x) {
  as(x, "matrix")						
})


#' convert a \code{ROISufaceVector} to a matrix 
#' 
#' @rdname as.matrix-methods
#' @param x the object
#' @export 
setMethod(f="as.matrix", signature=signature(x = "ROIVector"), def=function(x) {
  as(x, "matrix")						
})



#' Create an instance of class \code{\linkS4class{ROISurface}}
#' 
#' @param geometry the parent geometry: an instance of class \code{SurfaceGeometry}
#' @param indices the parent surface indices
#' @param data the data values, numeric \code{vector}
#' @return an instance of class \code{ROISurface}
#' @rdname ROISurface
#' @export
ROISurface <- function(geometry, indices, data) {
  vert <- vertices(geometry, indices)
  new("ROISurface", geometry=geometry, data=data, coords=vert, indices=indices)
}

#' Create an instance of class \code{\linkS4class{ROISurfaceVector}}
#' 
#' @param geometry the parent geometry: an instance of class \code{SurfaceGeometry}
#' @param indices the parent surface indices
#' @param data the data values, a \code{matrix}
#' @return an instance of class \code{ROISurfaceVector}
#' @rdname ROISurfaceVector
#' @export
ROISurfaceVector <- function(geometry, indices, data) {
  vert <- vertices(geometry, indices)
  new("ROISurfaceVector", geometry=geometry, data=data, coords=vert, indices=indices)
}

#' convert a \code{ROISurfaceVector} to an augmented matrix
#' 
#' @rdname as.matrix-methods
#' @param x the object
#' @export 
setMethod(f="as.matrix", signature=signature(x = "ROISurfaceVector"), def=function(x) {
  as(x, "matrix")						
})


.makeSquareGrid <- function(bvol, centroid, surround, fixdim=3) {
  vspacing <- spacing(bvol)
  vdim <- dim(bvol)
  centroid <- as.integer(centroid)
  
  dimnums <- seq(1,3)[-fixdim]
  
  coords <- lapply(centroid, function(x) { round(seq(x-surround, x+surround)) })
  coords <- lapply(dimnums, function(i) {
    x <- coords[[i]]
    x[x > 0 & x <= vdim[i]]
  })
  
  if (all(sapply(coords, length) == 0)) {
    stop(paste("invalid cube for centroid", centroid, " with surround", surround, ": volume is zero"))
  }
  
  if (fixdim == 3) {
    grid <- as.matrix(expand.grid(x=coords[[1]],y=coords[[2]],z=centroid[3]))
  } else if (fixdim == 2) {
    grid <- as.matrix(expand.grid(x=coords[[1]],y=centroid[2],z=coords[[2]]))
  } else if (fixdim == 1) {
    grid <- as.matrix(expand.grid(x=centroid[1],y=coords[[1]],z=coords[[2]]))
  }
  
  grid
  
}

.makeCubicGrid <- function(bvol, centroid, surround) {
  vspacing <- spacing(bvol)
  vdim <- dim(bvol)
  centroid <- as.integer(centroid)
  
  coords <- lapply(centroid, function(x) { round(seq(x-surround, x+surround)) })
  coords <- lapply(1:3, function(i) {
    x <- coords[[i]]
    x[x > 0 & x <= vdim[i]]
  })
 
  if (all(sapply(coords, length) == 0)) {
    stop(paste("invalid cube for centroid", centroid, " with surround", surround, ": volume is zero"))
  }
  
  grid <- as.matrix(expand.grid(x=coords[[1]],y=coords[[2]],z=coords[[3]]))
}



#' Create a square region of interest where the z-dimension is fixed at one voxel coordinate.
#' 
#' @param bvol an \code{BrainVolume} or \code{BrainSpace} instance.
#' @param centroid the center of the cube in \emph{voxel} coordinates.
#' @param surround the number of voxels on either side of the central voxel.
#' @param fill optional value(s) to assign to data slot.
#' @param nonzero keep only nonzero elements from \code{bvol}. If \code{bvol} is A \code{BrainSpace} then this argument is ignored.
#' @param fixdim the fixed dimension is the third, or z, dimension.
#' @return an instance of class \code{ROIVolume}.
#' @examples
#'  sp1 <- BrainSpace(c(10,10,10), c(1,1,1))
#'  square <- RegionSquare(sp1, c(5,5,5), 1)
#'  vox <- coords(square)
#'  ## a 3 X 3 X 1 grid
#'  nrow(vox) == 9
#' @export
RegionSquare <- function(bvol, centroid, surround, fill=NULL, nonzero=FALSE, fixdim=3) {
  if (is.matrix(centroid)) {
    centroid <- drop(centroid)
  }
  
  if (length(centroid) != 3) {
    stop("RegionSquare: centroid must have length of 3 (x,y,z coordinates)")
  }
  
  if (surround < 0) {
    stop("'surround' argument cannot be negative")
  }
  
  if (is(bvol, "BrainSpace") && is.null(fill)) {
    fill = 1
  }
  
  grid <- .makeSquareGrid(bvol,centroid,surround,fixdim=fixdim)
  
  vals <- if (!is.null(fill)) {
    rep(fill, nrow(grid))
  } else {
    as.numeric(bvol[grid])
  }   
  
  keep <- if (nonzero) {
    which(vals != 0)    
  } else {
    seq_along(vals)
  }
  
  ### add central voxel
  ROIVolume(space(bvol), data = vals[keep], coords = grid[keep, ])
  
}

  
#' Create A Cuboid Region of Interest
#' @param bvol an \code{BrainVolume} or \code{BrainSpace} instance
#' @param centroid the center of the cube in \emph{voxel} coordinates
#' @param surround the number of voxels on either side of the central voxel. A \code{vector} of length 3.
#' @param fill optional value(s) to assign to data slot. 
#' @param nonzero keep only nonzero elements from \code{bvol}. If \code{bvol} is A \code{BrainSpace} then this argument is ignored.
#' @return an instance of class \code{ROIVolume}
#' @rdname RegionCube
#' @examples
#'  sp1 <- BrainSpace(c(10,10,10), c(1,1,1))
#'  cube <- RegionCube(sp1, c(5,5,5), 3)
#'  vox <- coords(cube)
#'  cube2 <- RegionCube(sp1, c(5,5,5), 3, fill=5)
#'  
#'  
#' @export
RegionCube <- function(bvol, centroid, surround, fill=NULL, nonzero=FALSE) {
  if (is.matrix(centroid)) {
    centroid <- drop(centroid)
  }
  
  if (length(centroid) != 3) {
    stop("RegionCube: centroid must have length of 3 (x,y,z coordinates)")
  }
  
  if (surround < 0) {
    stop("'surround' argument cannot be negative")
  }
  
  if (is(bvol, "BrainSpace") && is.null(fill)) {
    fill = 1
  }
  
  grid <- .makeCubicGrid(bvol,centroid,surround)
  
  vals <- if (!is.null(fill)) {
    rep(fill, nrow(grid))
  } else {
    as.numeric(bvol[grid])
  }   
  
  keep <- if (nonzero) {
    which(vals != 0)    
  } else {
    seq_along(vals)
  }
  
  ### add central voxel
  ROIVolume(space(bvol), data = vals[keep], coords = grid[keep, ])
  
}

#' @importFrom rflann RadiusSearch
.makeSphericalGrid <- function(bvol, centroid, radius) {
  
  vspacing <- spacing(bvol)
  
  if (radius < min(vspacing)) {
    stop("'radius' is too small; must be greater than at least one voxel dimension in image")
  }
  
  vdim <- dim(bvol)
  centroid <- as.integer(centroid)
  
  
  cube <- as.matrix(expand.grid(
    seq(centroid[1] - round(radius/vspacing[1]), centroid[1] + round(radius/vspacing[1])),
    seq(centroid[2] - round(radius/vspacing[2]), centroid[2] + round(radius/vspacing[2])),
    seq(centroid[3] - round(radius/vspacing[3]), centroid[3] + round(radius/vspacing[3]))))
  
  
  coords <- t(t(cube) * vspacing)
  
  res <- rflann::RadiusSearch(matrix(centroid * vspacing, ncol=3), coords, radius=radius^2, max_neighbour=nrow(cube))
  
  cube[res$indices[[1]],]
        
}

# .makeSphericalGrid <- function(bvol, centroid, radius) {
#   vspacing <- spacing(bvol)
#   vdim <- dim(bvol)
#   centroid <- as.integer(centroid)
#   mcentroid <- ((centroid-1) * vspacing + vspacing/2)
#   cubedim <- ceiling(radius/vspacing)
#   
#   nsamples <- max(cubedim) * 2 + 1
#   vmat <- apply(cbind(cubedim, centroid), 1, function(cdim) {
#     round(seq(cdim[2] - cdim[1], cdim[2] + cdim[1], length.out=nsamples))
#   })
#   
#   vlist <- lapply(1:NCOL(vmat), function(i) {
#     v <- vmat[,i]
#     unique(v[v >= 1 & v <= vdim[i]])
#   })
#   
#   
#   if (all(sapply(vlist, length) == 0)) {
#     stop(paste("invalid sphere for centroid", paste(centroid, collapse=" "), " with radius",
#                radius))
#   }
# 
#  
#   grid <- as.matrix(expand.grid(x = vlist[[1]], y = vlist[[2]], z = vlist[[3]]))
#   
#   dvals <- apply(grid, 1, function(gvals) {
#     coord <- (gvals-1) * vspacing + vspacing/2
#     sqrt(sum((coord - mcentroid)^2))
#   })
#   
#   grid[which(dvals <= radius),]
#   
# }



#' @title Create a Spherical Region of Interest
#' 
#' @description Creates a Spherical ROI based on a Centroid.
#' @param bvol an \code{BrainVolume} or \code{BrainSpace} instance
#' @param centroid the center of the sphere in voxel space
#' @param radius the radius in real units (e.g. millimeters) of the spherical ROI
#' @param fill optional value(s) to store as data
#' @param nonzero if \code{TRUE}, keep only nonzero elements from \code{bvol}
#' @return an instance of class \code{ROIVolume}
#' @examples
#'  sp1 <- BrainSpace(c(10,10,10), c(1,2,3))
#'  cube <- RegionSphere(sp1, c(5,5,5), 3.5)
#'  vox <- coords(cube)
#'  cds <- coords(cube, real=TRUE)
#'  ## fill in ROI with value of 6
#'  cube1 <- RegionSphere(sp1, c(5,5,5), 3.5, fill=6)
#'  all(cube1@data == 6)
#' @export
RegionSphere <- function (bvol, centroid, radius, fill=NULL, nonzero=FALSE) {
  if (is.matrix(centroid)) {
    assertthat::assert_that(ncol(centroid == 3) & nrow(centroid) == 1)
    centroid <- drop(centroid)
  }
  
  assertthat::assert_that(length(centroid) == 3)
  
  if (is.null(fill) && is(bvol, "BrainSpace")) {
    fill = 1
  }
  
  bspace <- space(bvol)
  vspacing <- spacing(bvol)
  vdim <- dim(bvol)
  centroid <- as.integer(centroid)
  grid <- .makeSphericalGrid(bvol, centroid, radius)
   
  vals <- if (!is.null(fill)) {
    rep(fill, nrow(grid))
  } else {    
    as.numeric(bvol[grid])
  }   
  
  if (nonzero) {
    keep <- vals != 0 
    ROIVolume(bspace, data = vals[keep], coords = grid[keep, ,drop=FALSE])
  } else {
    ROIVolume(bspace, data = vals, coords = grid)
  }
  
}

.resample <- function(x, ...) x[sample.int(length(x), ...)]


roi_vector_matrix <- function(mat, refspace, indices, coords) {
  structure(mat,
            refspace=refspace,
            indices=indices,
            coords=coords,
            class=c("roi_vector_matrix", "matrix"))
  
}

roi_surface_matrix <- function(mat, refspace, indices, coords) {
  structure(mat,
            refspace=refspace,
            indices=indices,
            coords=coords,
            class=c("roi_surface_matrix", "matrix"))
  
}


#' @name as
#' @rdname as-methods
setAs(from="ROIVector", to="matrix", function(from) {
  ind <- indices(from)
  roi_vector_matrix(from@data, refspace=from@space, indices=ind, coords=indexToCoord(dropDim(from@space), as.numeric(ind)))
  
})


#' @name as
#' @rdname as-methods
setAs(from="ROIVolume", to="DenseBrainVolume", function(from) {
  dat <- array(0, dim(from@space))
  dat[coords(from)] <- from@data
  ovol <- DenseBrainVolume(dat, from@space, from@source)
})


#' @rdname values-methods
#' @export 
setMethod("values", signature(x="ROIVolume"),
          function(x, ...) {
             x@data
          })

#' @rdname values-methods
#' @export 
setMethod("values", signature(x="ROIVector"),
          function(x, ...) {
            x@data
          })


#' @rdname indices-methods
#' @export 
setMethod("indices", signature(x="ROIVolume"),
          function(x) {
			      gridToIndex(x@space, x@coords)
		  })

#' @rdname indices-methods
#' @export 
setMethod("indices", signature(x="ROIVector"),
          function(x) {
            gridToIndex(x@space, x@coords)
          })
            

#' @export
#' @param real if \code{TRUE}, return coordinates in real world units
#' @rdname coords-methods
setMethod(f="coords", signature=signature(x="ROIVolume"),
          function(x, real=FALSE) {
            if (real) {
              input <- t(cbind(x@coords-.5, rep(1, nrow(x@coords)))) 
              ret <- t(trans(x) %*% input)
              ret[,1:3,drop=FALSE]
            } else {
              x@coords
            }
          })


#' @export 
#' @rdname length-methods
setMethod(f="length", signature=signature(x="ROIVolume"),
          function(x) {
            nrow(x@coords)
          })





#' subset an \code{ROIVolume}
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param drop drop dimension
#' @rdname vol_subset-methods
#' @aliases [,ROIVolume,numeric,missing,ANY-method
setMethod("[", signature=signature(x = "ROIVolume", i = "numeric", j = "missing", drop = "ANY"),
          function (x, i, j, drop) {
            ROIVolume(x@space, x@coords[i,,drop=FALSE], x@data[i])
          })

#' @rdname vol_subset-methods
#' @aliases [,ROIVolume,logical,missing,ANY-method
setMethod("[", signature=signature(x="ROIVolume", i="logical", j="missing", drop="ANY"),
          function(x,i,j,drop) {
            ROIVolume(x@space, x@coords[i,,drop=FALSE], x@data[i])
          })


#' show an \code{\linkS4class{ROIVolume}} 
#' @param object the object
#' @export
setMethod("show", signature=signature(object = "ROIVolume"),
		  function (object) {
			  cat("\n\n\tROIVolume", "\n")
			  cat("\t size: ", length(object), "\n")
			  cat("\t parent dim:", dim(object), "\n")
			  cat("\t num data cols:", if (is.matrix(object@data)) ncol(object@data) else 1, "\n" )
			  cat("\t voxel center of mass: ", colMeans(coords(object)), "\n")
		  })

  
      
.distance <- function(p1, p2) {
  diffs = (p1 - p2)
  sqrt(sum(diffs*diffs))
}


#' Create a Kernel object
#' @param kerndim the dimensions in voxels of the kernel
#' @param vdim the dimensions of the voxels in real units
#' @param FUN the kernel function taking as its first argument representing the distance from the center of the kernel
#' @param ... additional parameters to the kernel FUN
#' @importFrom stats dnorm
#' @export
Kernel <- function(kerndim, vdim, FUN=dnorm, ...) {
  if (length(kerndim) < 2) {
    stop("kernel dim length must be greater than 1")
  }
  
  #kern <- array(0, kerndim)
  
  ## the half-width for each dimensions
  hwidth <- sapply(kerndim, function(d) ceiling(d/2 -1))
  
  ## note, if a kernel dim is even, this will force it to be odd numbered
  grid.vec <- lapply(hwidth, function(sv) seq(-sv, sv))

  # compute relative voxel locations (i.e. centered at 0,0,0)
  voxel.ind <- as.matrix(do.call("expand.grid", grid.vec))
  
  # fractional voxel locations so that the location of a voxel coordinate is centered within the voxel
  cvoxel.ind <- t(apply(voxel.ind, 1, function(vals) sign(vals)* ifelse(vals == 0, 0, abs(vals)-.5)))
  
  ## the coordinates ofthe voxels (i.e. after multiplying by pixel dims)
  coords <- t(apply(cvoxel.ind, 1, function(v) (v * vdim)))
  
  ## distance of coordinate from kernel center
  coord.dist <- apply(coords, 1, .distance, c(0,0,0))
  
  wts <- FUN(coord.dist, ...)
  wts <- wts/sum(wts)

  
  kern.weights <- wts
  
  new("Kernel", width=kerndim, weights=kern.weights, voxels=voxel.ind, coords=coords)

}



#' @param centerVoxel the absolute location of the center of the voxel, default is (0,0,0)
#' @rdname voxels-methods
#' @export
setMethod(f="voxels", signature=signature(x="Kernel"),
          function(x, centerVoxel=NULL) {
            if (is.null(centerVoxel)) {
              x@voxels
            } else {
              sweep(x@voxels, 2, centerVoxel, "+")
            }
          })


  

  
