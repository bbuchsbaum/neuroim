#' @include AllClass.R
{}
#' @include Axis.R
{}


#' Constructor function for \code{\linkS4class{BrainSpace}} class
#' 
#' @param Dim a vector describing the dimensions of the spatial grid
#' @param origin the coordinate origin of the image space
#' @param spacing the real-valued voxel dimensions (usually in millimeters)
#' @param axes the image axes ordering (default is based on the NIFTI standard, Left-Posterior-Inferior)
#' @param trans a matrix representing the coordinate transformation associated with the image space (default is based on the NIFTI standard, Left-Posterior-Inferior)
#' @return an instance of class \code{\linkS4class{BrainSpace}}
#' @note one should rarely need to create a new \code{BrainSpace} instance, as it will almost always be created automatically using information stored in an image header.
#' Also, If one already has an existing image object, its \code{BrainSpace} instance can be easily extracted with the \code{space} method.
#' @export
#' @rdname BrainSpace
#' @examples
#' bspace <- BrainSpace(c(64,64,64), origin=c(0,0,0), spacing=c(2,2,2))
#' print(bspace)
#' origin(bspace)
#' axes(bspace)
#' trans(bspace)
BrainSpace <- function(Dim, spacing=NULL, origin=NULL, axes=NULL, trans=NULL) {
	
	if (is.null(spacing)) {
		spacing <- rep(1, min(length(Dim), 3))
	}    
	
	if (is.null(origin)) {
		origin <- rep(0, min(length(Dim), 3))
	}
  
  if (length(origin) != length(spacing)) {
    stop("length of 'origin' must equal length of 'spacing'")
  }
	
	if (is.null(trans)) {
		D <- min(length(Dim), 3)
		trans <- diag(c(spacing,1))
		trans[1:D,D+1] <- origin
	}
  
	if (is.null(axes) && length(Dim) >= 3) {
	  axes <- .nearestAnatomy(trans)
	} else if (is.null(axes) && length(Dim) == 2) {
	  ### need .nearestAnatomy for 2d slice
	  ## TODO
	  axes <- AxisSet2D(LEFT_RIGHT, POST_ANT)
	}
	
	new("BrainSpace", Dim=as.integer(Dim),
			origin=origin,
			spacing=spacing,
			axes=axes,
			trans=trans,
			inverseTrans=solve(trans))
}

#' show a \code{BrainSpace}
#' @param object the object
#' @export
setMethod(f="show", signature=signature("BrainSpace"),
		def=function(object) {
			cat("BrainSpace\n")
			cat("  Type           :", class(object), "\n")
			cat("  Dimension      :", object@Dim, "\n")
			cat("  Spacing        :", paste(paste(object@spacing[1:(length(object@spacing)-1)], " X ", collapse=" "), 
							object@spacing[length(object@spacing)], "\n"))
			cat("  Origin         :", paste(paste(object@origin[1:(length(object@origin)-1)], " X ", collapse=" "), 
							object@origin[length(object@origin)], "\n"))
			#cat("  Axes           :", paste(object@axes@i@axis, object@axes@j@axis, object@axes@k@axis, "\n"))
			cat("  Coordinate Transform :", object@trans, "\n")
			
			
		}
)

#' add dimension to \code{\linkS4class{BrainSpace}}
#' @export
#' @rdname addDim-methods
setMethod(f="addDim", signature=signature(x = "BrainSpace", n="numeric"),
		def=function(x, n) {
			BrainSpace(c(dim(x), n), origin=origin(x), spacing=spacing(x), axes=axes(x), trans=trans(x))
		})


#' @export
#' @rdname dropDim-methods
setMethod(f="dropDim", signature=signature(x="BrainSpace", dimnum="numeric"),
          def=function(x, dimnum) {
            D <- dim(x)
            stopifnot(length(D) >= 2)
            
            Dind <- seq(1,length(D))[-dimnum]
            if (ndim(x) > 3) {
              BrainSpace(D[Dind], origin=origin(x)[Dind], spacing=spacing(x)[Dind], axes=axes(x), trans=trans(x))
            } else {
              tx <- trans(x)
              tx <- rbind(cbind(tx[Dind,Dind], origin(x)[Dind]), c(rep(0, length(Dind)), 1))
              BrainSpace(D[Dind], origin=origin(x)[Dind], spacing=spacing(x)[Dind], axes=dropDim(axes(x), dimnum), trans=tx)
            }
            
          })

#' @export
#' @rdname dropDim-methods
setMethod(f="dropDim", signature=signature(x = "BrainSpace", dimnum="missing"),
		def=function(x) {			
			D <- dim(x)		
			stopifnot(length(D) >= 2)
			Dind <- 1:(length(D)-1)		
			
			
			### doesn't drop dimension in transformation matrix...
      ### brain vector's don't have th axis and these are incorrectly dropped
      if (ndim(x) > 3) {
			  BrainSpace(D[Dind], origin=origin(x)[Dind], spacing=spacing(x)[Dind], axes=axes(x), trans=trans(x))
      } else {
        tx <- trans(x)
        tx <- rbind(cbind(tx[Dind,Dind], origin(x)[Dind]), c(rep(0, length(Dind)),1))
        BrainSpace(D[Dind], origin=origin(x)[Dind], spacing=spacing(x)[Dind], axes=dropDim(axes(x)), trans=tx)
      }
		})

#' dim
#' 
#' @export
#' @param x the object
setMethod(f="dim", signature=signature(x = "BrainSpace"),
		def=function(x) x@Dim)


#' @export
#' @rdname ndim-methods
setMethod(f="ndim", signature=signature(x = "BrainSpace"),
		def=function(x) length(x@Dim))

#' @export
#' @rdname union-methods
setMethod(f="union", signature=signature(x = "BrainSpace", y="BrainSpace"),
          def=function(x, y) {
            s1 <- sign(diag(trans(x)))
            s2 <- sign(diag(trans(y)))
            if (length(s1) != length(s2) || !all(s1==s2)) {
              stop("union requires argument with the same dimensionality and axis orientation")
            }
            bds1 <- bounds(x)
            bds2 <- bounds(y)
            direc <- sign(diag(trans(x))[1:ndim(x)])
            ubounds <- t(sapply(1:nrow(bds1), function(i) {
              if (direc[i] < 0) {
                c(max(bds1[i,1], bds2[i,1]), min(bds1[i,2], bds2[i,2]))
              } else {
                c(min(bds1[i,1], bds2[i,1]), max(bds1[i,2], bds2[i,2]))
              }
            }))
            uspacing <- pmin(spacing(x), spacing(y))
            udim <- pmax(dim(x), dim(y))
            
            BrainSpace(udim, spacing=uspacing, origin=ubounds[,1], axes=x@axes)
          })

#' spacing
#' 
#' @export
#' @rdname spacing-methods
setMethod(f="spacing", signature=signature(x = "BrainSpace"),
		def=function(x) x@spacing)

#' bounds
#' 
#' @export
#' @rdname bounds-methods
setMethod(f="bounds", signature=signature(x = "BrainSpace"),
		def=function(x) {
		  c1 <- gridToCoord(x, c(1,1,1))
		  c2 <- gridToCoord(x, c(dim(x)[1], dim(x)[2], dim(x)[3]))
      #direc <- diag(trans(x))
      #direc <- sign(direc[1:(length(direc)-1)])
			#mat <- cbind(x@origin, x@origin+(spacing(x)*dim(x)*direc))
			mat <- cbind(x@origin, x@origin+(spacing(x)*dim(x)))
			return(mat)
		}
)

 
#' @export 
#' @rdname indexToGrid-methods
setMethod(f="indexToGrid", signature=signature(x="BrainSpace", idx="numeric"),
          def=function(x, idx) {
            array.dim <- dim(x)          
            .indexToGrid(idx, array.dim)        
          })


#' @export 
#' @rdname indexToCoord-methods
setMethod(f="indexToCoord", signature=signature(x="BrainSpace", idx="numeric"),
          def=function(x, idx) {
            grid <- indexToGrid(x, idx) - .5
            res <- trans(x) %*% t(cbind(grid, rep(1,nrow(grid))))
            t(res[1:ndim(x),])
          })


#' @export 
#' @rdname coordToIndex-methods
setMethod(f="coordToIndex", signature=signature(x="BrainSpace", coords="matrix"),
          def=function(x, coords) {
            grid = t(inverseTrans(x) %*% t(cbind(coords, rep(1, nrow(coords)))))
            gridToIndex(x, grid[,1:3] + .5)
          })
 
#' @export 
#' @rdname coordToIndex-methods
setMethod(f="coordToIndex", signature=signature(x="BrainSpace", coords="numeric"),
          def=function(x, coords) {
            coords <- matrix(coords, nrow=1)
            callGeneric(x,coords)
          })



#' @export 
#' @rdname axisToIndex-methods
setMethod(f="axisToIndex", signature=signature(x="BrainSpace", real="numeric", dimNum="numeric"),
          def=function(x, real, dimNum) {
            # todo check that real is within bounds
            bds <- bounds(x)[dimNum,]           
            floor(abs(real - bds[1])/(spacing(x)[dimNum]) + 1)
            
          })

#' @export 
#' @rdname axisToIndex-methods
setMethod(f="indexToAxis", signature=signature(x="BrainSpace", index="numeric", dimNum="numeric"),
          def=function(x, index, dimNum) {
            vd <- spacing(x)[dimNum]  
            offset <- origin(x)[dimNum]
            ((index - .5) * vd) + offset
            
            
          })


 
#' @export 
#' @rdname coordToGrid-methods
setMethod(f="coordToGrid", signature=signature(x="BrainSpace", coords="matrix"),
          def=function(x, coords) {
            grid = t(inverseTrans(x) %*% t(cbind(coords, rep(1, nrow(coords)))))
            grid[,1:3]+ 1
          })

 
#' @export 
#' @rdname coordToGrid-methods
setMethod(f="coordToGrid", signature=signature(x="BrainSpace", coords="numeric"),
          def=function(x, coords) {
            coords <- matrix(coords, nrow=1)
            callGeneric(x, coords)
          })



#expand.grid.alt <- function(seq1,seq2) {
#  cbind(rep.int(seq1, length(seq2)),
#        c(t(matrix(rep.int(seq2, length(seq1)), nrow=length(seq2)))))
#}
# https://stackoverflow.com/questions/10405637/use-outer-instead-of-expand-grid
# expand.grid.alt(seq_len(nrow(dat)), seq_len(ncol(dat)))

# even faster...
#expand.grid.jc <- function(seq1,seq2) {
#  cbind(Var1 = rep.int(seq1, length(seq2)), 
#        Var2 = rep.int(seq2, rep.int(length(seq1),length(seq2))))
#}


#' @export 
#' @rdname gridToGrid-methods
setMethod(f="gridToGrid", signature=signature(x="BrainSpace", vox="matrix"),
          def=function(x, vox) {
            nd <- ndim(x)
            stopifnot(ncol(vox) == nd)
            ## todo permMat doesn't work when reference space is not LPI
            
            itx <- inverseTrans(x)[1:nd,1:nd]
            tx <- t(apply(itx, 2, function(r) {
              id <- which.max(abs(r))
              out <- numeric(length(r)+1)
              out[id] <- sign(r[id])
              if (out[id] < 0) {
                out[length(r)+1] <- dim(x)[id] + 1
              }
              out
            }))
            vox <- t(cbind(vox, rep(1, nrow(vox))))
            t(tx %*% vox)
          })


#' @export 
#' @rdname gridToCoord-methods
setMethod(f="gridToCoord", signature=signature(x="BrainSpace", coords="matrix"),
          def=function(x, coords) {
            input <- t(cbind(coords-1, rep(1, nrow(coords)))) 
            ret <- t(trans(x) %*% input)
            ret[,1:3,drop=FALSE]
          })

#' @export 
#' @rdname gridToCoord-methods
setMethod(f="gridToCoord", signature=signature(x="BrainSpace", coords="numeric"),
          def=function(x, coords) {
            coords <- matrix(coords, ncol=length(coords))
            input <- t(cbind(coords-1, rep(1, nrow(coords)))) 
            ret <- t(trans(x) %*% input)
            ret[,1:3,drop=FALSE]
            
          })


#' @export 
#' @rdname gridToCoord-methods
setMethod(f="gridToCoord", signature=signature(x="BrainVolume", coords="matrix"),
          def=function(x, coords) {
            callGeneric(space(x), coords)
            
          })



#' @export 
#' @rdname gridToIndex-methods
setMethod(f="gridToIndex", signature=signature(x="BrainSpace", coords="matrix"),
		def=function(x, coords) {
			array.dim <- dim(x)
      ### TODO assumes 3D index ....
			.gridToIndex3D(dim(x)[1:3], coords)
		})

 
#' @export 
#' @rdname gridToIndex-methods
setMethod(f="gridToIndex", signature=signature(x="BrainSpace", coords="numeric"),
		def=function(x, coords) {
		  ### TODO assumes 3D index ....
			array.dim <- dim(x)
			.gridToIndex3D(dim(x), matrix(coords, nrow=1, byrow=TRUE))
		})



#' @export
#' @rdname reorient-methods
setMethod(f="reorient", signature=signature(x = "BrainSpace", orient="character"),
          def=function(x, orient) {
            anat <- findAnatomy3D(orient[1], orient[2], orient[3])
            pmat_orig <- permMat(x)
            pmat_new <- permMat(anat)
            
            ## from orient to original voxel space
            itx <- pmat_new %*% inverseTrans(x)[1:ndim(x), 1:(ndim(x)+1)]
            
            ## from voxel space to new orient
            tx <- zapsmall(MASS::ginv(rbind(itx, c(0,0,0,1))))
            
            perm <- pmat_new %*% MASS::ginv(pmat_orig)
            newdim <- abs(perm %*% dim(x))[,1]
            newspacing <- abs(perm %*% spacing(x))[,1]
            
            BrainSpace(newdim, newspacing, axes=x@axes, trans=tx, 
                       origin=tx[1:(ndim(x)) ,ndim(x)+1])
       
          })

#' @export
#' @rdname origin-methods
setMethod(f="origin", signature=signature(x = "BrainSpace"),
		def=function(x) x@origin)


 
#' @export
#' @rdname axes-methods
setMethod(f="axes", signature=signature(x = "BrainSpace"),
		def=function(x) x@axes)


#' @export
#' @rdname trans-methods
setMethod(f="trans", signature=signature(x = "BrainSpace"),
		def=function(x) x@trans)

 
#' @export
#' @rdname inverseTrans-methods
setMethod(f="inverseTrans", signature=signature(x = "BrainSpace"),
		def=function(x) x@inverseTrans)


 
#' @export
#' @rdname bounds-methods
setMethod(f="bounds", signature=signature(x = "BrainData"),
		def=function(x) {
			bounds(space(x))
		})

#' @export
#' @rdname axes-methods
setMethod(f="axes", signature=signature(x = "BrainData"),
		def=function(x) {
			axes(space(x))
		})

 
#' @export
#' @rdname origin-methods
setMethod(f="origin", signature=signature(x = "BrainData"),
		def=function(x) {
			origin(space(x))
		})


#' @export
#' @rdname space-methods
setMethod(f="space", signature=signature(x = "BrainSpace"),
          def=function(x) {
           x
          })

 
#' @export
#' @rdname trans-methods
setMethod(f="trans", signature=signature(x = "BrainData"),
		def=function(x) trans(space(x)))



#' @export
#' @rdname inverseTrans-methods
setMethod(f="inverseTrans", signature=signature(x = "BrainData"),
		def=function(x) inverseTrans(space(x)))




